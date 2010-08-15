% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_replicate).
-behaviour(gen_server).

% public API
-export([replicate/4]).

% gen_server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-include("couch_db.hrl").
-include("couch_api_wrap.hrl").

% Can't be greater than the maximum number of child restarts specified
% in couch_rep_sup.erl.
-define(MAX_RESTARTS, 3).


-record(stats, {
    missing_checked = 0,
    missing_found = 0,
    docs_read = 0,
    docs_written = 0,
    doc_write_failures = 0
    }).

-record(rep_state, {
    rep_id,
    rep_options,
    source_name,
    target_name,
    source,
    target,
    history,
    checkpoint_history,
    start_seq,
    current_through_seq,
    committed_seq,
    source_log,
    target_log,
    rep_starttime,
    src_starttime,
    tgt_starttime,
    timer, % checkpoint timer
    missing_revs_queue,
    changes_queue,
    changes_reader,
    missing_revs_finder,
    doc_copier,
    seqs_in_progress = gb_trees:from_orddict([]),
    stats = #stats{}
    }).


replicate(Src, Tgt, Options, UserCtx) ->
    RepId = make_replication_id(Src, Tgt, UserCtx, Options),
    case couch_util:get_value(cancel, Options, false) of
    true ->
        end_replication(RepId);
    false ->
        {ok, Listener} = rep_result_listener(RepId),
        {ok, _Pid} = start_replication(RepId, Src, Tgt, Options, UserCtx),
        wait_for_result(RepId, Listener)
    end.


start_replication({BaseId, Extension} = RepId, Src, Tgt, Options, UserCtx) ->
    RepChildId = BaseId ++ Extension,
    ChildSpec = {
        RepChildId,
        {gen_server, start_link,
           [?MODULE, [RepId, Src, Tgt, Options, UserCtx], []]},
        transient,
        1,
        worker,
        [?MODULE]
    },
    RepPid = case supervisor:start_child(couch_rep_sup, ChildSpec) of
    {ok, Pid} ->
        ?LOG_INFO("starting new replication ~p at ~p", [RepChildId, Pid]),
        Pid;
    {error, already_present} ->
        case supervisor:restart_child(couch_rep_sup, RepChildId) of
        {ok, Pid} ->
            ?LOG_INFO("starting replication ~p at ~p", [RepChildId, Pid]),
            Pid;
        {error, running} ->
            %% this error occurs if multiple replicators are racing
            %% each other to start and somebody else won.  Just grab
            %% the Pid by calling start_child again.
            {error, {already_started, Pid}} =
                supervisor:start_child(couch_rep_sup, ChildSpec),
            ?LOG_DEBUG("replication ~p already running at ~p",
                [RepChildId, Pid]),
            Pid;
        {error, {db_not_found, DbUrl}} ->
            throw({db_not_found, <<"could not open ", DbUrl/binary>>})
        end;
    {error, {already_started, Pid}} ->
        ?LOG_DEBUG("replication ~p already running at ~p", [RepChildId, Pid]),
        Pid;
    {error, {{db_not_found, DbUrl}, _}} ->
        throw({db_not_found, <<"could not open ", DbUrl/binary>>});
    {error, Error} ->
        throw({error, Error})
    end,
    {ok, RepPid}.


rep_result_listener(RepId) ->
    ReplyTo = self(),
    {ok, _Listener} = couch_replication_notifier:start_link(
        fun({_, RepId2, _} = Ev) when RepId2 =:= RepId ->
                ReplyTo ! Ev;
            (_) ->
                ok
        end).


wait_for_result(RepId, Listener) ->
    wait_for_result(RepId, Listener, ?MAX_RESTARTS).

wait_for_result(RepId, Listener, RetriesLeft) ->
    receive
    {finished, RepId, RepResult} ->
        couch_replication_notifier:stop(Listener),
        {ok, RepResult};
    {error, RepId, Reason} ->
        case RetriesLeft > 0 of
        true ->
            wait_for_result(RepId, Listener, RetriesLeft - 1);
        false ->
            couch_replication_notifier:stop(Listener),
            {error, couch_util:to_binary(Reason)}
        end
    end.


end_replication({BaseId, Extension}) ->
    FullRepId = BaseId ++ Extension,
    case supervisor:terminate_child(couch_rep_sup, FullRepId) of
    {error, not_found} = R ->
        R;
    ok ->
        ok = supervisor:delete_child(couch_rep_sup, FullRepId),
        {ok, {cancelled, ?l2b(BaseId)}}
    end.


init(InitArgs) ->
    try
        do_init(InitArgs)
    catch
    throw:{db_not_found, DbUrl} ->
        {stop, {db_not_found, DbUrl}}
    end.

do_init([RepId, Src, Tgt, Options, UserCtx]) ->
    process_flag(trap_exit, true),

    #rep_state{
        source = Source,
        target = Target,
        start_seq = StartSeq
    } = State = init_state(RepId, Src, Tgt, Options, UserCtx),

    {ok, MissingRevsQueue} = couch_work_queue:new(100000, 500),

    case couch_util:get_value(doc_ids, Options) of
    undefined ->
        {ok, ChangesQueue} = couch_work_queue:new(100000, 500),

        % This starts the _changes reader process. It adds the changes from
        % the source db to the ChangesQueue.
        ChangesReader = spawn_changes_reader(self(), StartSeq, Source,
            ChangesQueue, Options),

        % This starts the missing revs finder. It checks the target for changes
        % in the ChangesQueue to see if they exist on the target or not. If not,
        % adds them to MissingRevsQueue.
        MissingRevsFinder = spawn_missing_revs_finder(self(), Target,
            ChangesQueue, MissingRevsQueue);
    DocIds ->
        ChangesQueue = nil,
        ChangesReader = nil,
        MissingRevsFinder = nil,

        lists:foreach(
            fun(DocId) ->
                ok = couch_work_queue:queue(MissingRevsQueue, {doc_id, DocId})
            end, DocIds),
        couch_work_queue:close(MissingRevsQueue)
    end,

    % This starts the doc copy process. It fetches documents from the
    % MissingRevsQueue and copies them from the source to the target database.
    DocCopier = spawn_doc_copy(self(), Source, Target, MissingRevsQueue),

    {ok, State#rep_state{
            missing_revs_queue = MissingRevsQueue,
            changes_queue = ChangesQueue,
            changes_reader = ChangesReader,
            missing_revs_finder = MissingRevsFinder,
            doc_copier = DocCopier
        }
    }.


handle_info({seq_start, {Seq, NumChanges}}, State) ->
    SeqsInProgress2 = gb_trees:insert(Seq, NumChanges,
        State#rep_state.seqs_in_progress),
    {noreply, State#rep_state{seqs_in_progress = SeqsInProgress2}};

handle_info({seq_changes_done, {Seq, NumChangesDone}}, State) ->
    #rep_state{seqs_in_progress = SeqsInProgress} = State,
    % Decrement the # changes for this seq by NumChangesDone.
    TotalChanges = gb_trees:get(Seq, State#rep_state.seqs_in_progress),
    NewState = case TotalChanges - NumChangesDone of
    0 ->
        % This seq is completely processed. Check to see if it was the
        % smallest seq in progess. If so, we've made progress that can
        % be checkpointed.
        State2 = case gb_trees:smallest(SeqsInProgress) of
        {Seq, _} ->
            State#rep_state{current_through_seq = Seq};
        _ ->
            State
        end,
        State2#rep_state{
            seqs_in_progress = gb_trees:delete(Seq, SeqsInProgress)
        };
    NewTotalChanges when NewTotalChanges > 0 ->
        % There are still some changes that need work done.
        % Put the new count back.
        State#rep_state{
            seqs_in_progress =
                gb_trees:update(Seq, NewTotalChanges, SeqsInProgress)
        }
    end,
    {noreply, NewState};

handle_info({add_stat, {StatPos, Val}}, #rep_state{stats = Stats} = State) ->
    Stat = element(StatPos, Stats),
    NewStats = setelement(StatPos, Stats, Stat + Val),
    {noreply, State#rep_state{stats = NewStats}};

handle_info(done, #rep_state{seqs_in_progress = SeqsInProgress} = State) ->
    % This means all the worker processes have completed their work.
    % Assert that all the seqs have been processed
    0 = gb_trees:size(SeqsInProgress),
    NewState = do_checkpoint(State),
    cancel_timer(NewState),
    {stop, normal, NewState};

handle_info({'EXIT', Pid, normal}, #rep_state{changes_reader=Pid} = State) ->
    {noreply, State};

handle_info({'EXIT', Pid, Reason}, #rep_state{changes_reader=Pid} = State) ->
    cancel_timer(State),
    ?LOG_ERROR("ChangesReader process died with reason: ~p", [Reason]),
    {stop, changes_reader_died, State};

handle_info({'EXIT', Pid, normal}, #rep_state{missing_revs_finder=Pid} = St) ->
    {noreply, St};

handle_info({'EXIT', Pid, Reason}, #rep_state{missing_revs_finder=Pid} = St) ->
    cancel_timer(St),
    ?LOG_ERROR("MissingRevsFinder process died with reason: ~p", [Reason]),
    {stop, missing_revs_finder_died, St};

handle_info({'EXIT', Pid, normal}, #rep_state{doc_copier=Pid} = State) ->
    {noreply, State};

handle_info({'EXIT', Pid, Reason}, #rep_state{doc_copier=Pid} = State) ->
    cancel_timer(State),
    ?LOG_ERROR("DocCopier process died with reason: ~p", [Reason]),
    {stop, doc_copier_died, State};

handle_info({'EXIT', Pid, normal}, #rep_state{missing_revs_queue=Pid} = St) ->
    {noreply, St};

handle_info({'EXIT', Pid, Reason}, #rep_state{missing_revs_queue=Pid} = St) ->
    cancel_timer(St),
    ?LOG_ERROR("MissingRevsQueue process died with reason: ~p", [Reason]),
    {stop, missing_revs_queue_died, St};

handle_info({'EXIT', Pid, normal}, #rep_state{changes_queue=Pid} = State) ->
    {noreply, State};

handle_info({'EXIT', Pid, Reason}, #rep_state{changes_queue=Pid} = State) ->
    cancel_timer(State),
    ?LOG_ERROR("ChangesQueue process died with reason: ~p", [Reason]),
    {stop, changes_queue_died, State}.


handle_call(Msg, _From, State) ->
    ?LOG_ERROR("Replicator received an unexpected synchronous call: ~p", [Msg]),
    {stop, unexpected_sync_message, State}.


handle_cast(checkpoint, State) ->
    State2 = do_checkpoint(State),
    {noreply, State2#rep_state{timer = start_timer(State)}};

handle_cast(Msg, State) ->
    ?LOG_ERROR("Replicator received an unexpected asynchronous call: ~p", [Msg]),
    {stop, unexpected_async_message, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


terminate(normal, #rep_state{rep_id = RepId} = State) ->
    terminate_cleanup(State),
    couch_replication_notifier:notify({finished, RepId, get_result(State)});

terminate(shutdown, State) ->
    % cancelled replication throught ?MODULE:end_replication/1
    terminate_cleanup(State);

terminate(Reason, #rep_state{rep_id = RepId} = State) ->
    terminate_cleanup(State),
    couch_replication_notifier:notify({error, RepId, Reason}).


terminate_cleanup(#rep_state{source = Source, target = Target}) ->
    couch_api_wrap:db_close(Source),
    couch_api_wrap:db_close(Target).


start_timer(#rep_state{rep_options = Options} = State) ->
    case couch_util:get_value(doc_ids, Options) of
    undefined ->
        After = checkpoint_interval(State),
        case timer:apply_after(After, gen_server, cast, [self(), checkpoint]) of
        {ok, Ref} ->
            Ref;
        Error ->
            ?LOG_ERROR("Replicator, error scheduling checkpoint:  ~p", [Error]),
            nil
        end;
    _DocIdList ->
        nil
    end.


cancel_timer(#rep_state{timer = nil}) ->
    ok;
cancel_timer(#rep_state{timer = Timer}) ->
    {ok, cancel} = timer:cancel(Timer).


get_result(#rep_state{stats = Stats, rep_options = Options} = State) ->
    case couch_util:get_value(doc_ids, Options) of
    undefined ->
        State#rep_state.checkpoint_history;
    _DocIdList ->
        {[
            {<<"start_time">>, ?l2b(State#rep_state.rep_starttime)},
            {<<"end_time">>, ?l2b(httpd_util:rfc1123_date())},
            {<<"docs_read">>, Stats#stats.docs_read},
            {<<"docs_written">>, Stats#stats.docs_written},
            {<<"doc_write_failures">>, Stats#stats.doc_write_failures}
        ]}
    end.


init_state({BaseId, _Ext} = RepId, Src, Tgt, Options, UserCtx) ->
    {ok, Source} = couch_api_wrap:db_open(Src, [{user_ctx, UserCtx}]),
    {ok, Target} = couch_api_wrap:db_open(Tgt, [{user_ctx, UserCtx}],
        couch_util:get_value(create_target, Options, false)),

    {ok, SourceInfo} = couch_api_wrap:get_db_info(Source),
    {ok, TargetInfo} = couch_api_wrap:get_db_info(Target),

    DocId = ?l2b(?LOCAL_DOC_PREFIX ++ BaseId),
    case couch_api_wrap:open_doc(Source, DocId, []) of
    {ok, SourceLog} ->  SourceLog;
    _ ->                SourceLog = #doc{id=DocId}
    end,
    case couch_api_wrap:open_doc(Target, DocId, []) of
    {ok, TargetLog} ->  TargetLog;
    _ ->                TargetLog = #doc{id=DocId}
    end,
    {StartSeq, History} = compare_replication_logs(SourceLog, TargetLog),
    #doc{body={CheckpointHistory}} = SourceLog,
    State = #rep_state{
        rep_id = RepId,
        rep_options = Options,
        source_name = Src,
        target_name = Tgt,
        source = Source,
        target = Target,
        history = History,
        checkpoint_history = {[{<<"no_changes">>, true}| CheckpointHistory]},
        start_seq = StartSeq,
        current_through_seq = StartSeq,
        committed_seq = StartSeq,
        source_log = SourceLog,
        target_log = TargetLog,
        rep_starttime = httpd_util:rfc1123_date(),
        src_starttime = couch_util:get_value(instance_start_time, SourceInfo),
        tgt_starttime = couch_util:get_value(instance_start_time, TargetInfo)
    },
    State#rep_state{timer = start_timer(State)}.


spawn_changes_reader(Cp, StartSeq, Source, ChangesQueue, Options) ->
    spawn_link(
        fun()->
            couch_api_wrap:changes_since(Source, all_docs, StartSeq,
                fun(#doc_info{high_seq=Seq, revs=Revs} = DocInfo) ->
                    Cp ! {seq_start, {Seq, length(Revs)}},
                    Cp ! {add_stat, {#stats.missing_checked, length(Revs)}},
                    ok = couch_work_queue:queue(ChangesQueue, DocInfo)
                end, Options),
            couch_work_queue:close(ChangesQueue)
        end).


spawn_missing_revs_finder(StatsProcess, 
        Target, ChangesQueue, MissingRevsQueue) ->
    % Note, we could spawn more missing revs processes here. Before that's
    % possible the work_queue code needs to be modified to work with multiple
    % dequeueing processes
    spawn_link(fun() ->
        missing_revs_finder_loop(StatsProcess, 
                Target, ChangesQueue, MissingRevsQueue)
        end).


missing_revs_finder_loop(Cp, 
        Target, ChangesQueue, MissingRevsQueue) ->
    case couch_work_queue:dequeue(ChangesQueue) of
    closed ->
        couch_work_queue:close(MissingRevsQueue);
    {ok, DocInfos} ->
        IdRevs = [{Id, [Rev || #rev_info{rev=Rev} <- RevsInfo]} ||
                #doc_info{id=Id, revs=RevsInfo} <- DocInfos],
        {ok, Missing} = couch_api_wrap:get_missing_revs(Target, IdRevs),
        % Figured out which on the target are missing.
        % Missing contains the id and revs missing, and any possible
        % ancestors that already exist on the target. This enables
        % incremental attachment replication, so the source only needs to send
        % attachments modified since the common ancestor on target.

        % Signal to the checkpointer any that are already on the target are
        % now complete.
        IdRevsSeqDict = dict:from_list(
            [{Id, {[Rev || #rev_info{rev=Rev} <- RevsInfo], Seq}} ||
                    #doc_info{id=Id, revs=RevsInfo, high_seq=Seq} <- DocInfos]),
        NonMissingIdRevsSeqDict = remove_missing(IdRevsSeqDict, Missing),
        % signal the completion of these that aren't missing
        lists:foreach(fun({_Id, {Revs, Seq}}) ->
                Cp ! {seq_changes_done, {Seq, length(Revs)}}
            end, dict:to_list(NonMissingIdRevsSeqDict)),

        % Expand out each docs and seq into it's own work item
        lists:foreach(fun({Id, Revs, PAs}) ->
            % PA means "possible ancestor"
            Cp ! {add_stat, {#stats.missing_found, length(Revs)}},
            {_, Seq} = dict:fetch(Id, IdRevsSeqDict),
            ok = couch_work_queue:queue(MissingRevsQueue, {Id, Revs, PAs, Seq})
            end, Missing),
        missing_revs_finder_loop(Cp, Target, ChangesQueue, MissingRevsQueue)
    end.


remove_missing(IdRevsSeqDict, []) ->
    IdRevsSeqDict;
remove_missing(IdRevsSeqDict, [{MissingId, MissingRevs, _} | Rest]) ->
    {AllChangedRevs, Seq} = dict:fetch(MissingId, IdRevsSeqDict),
    case AllChangedRevs -- MissingRevs of
    [] ->
        remove_missing(dict:erase(MissingId, IdRevsSeqDict), Rest);
    NotMissingRevs ->
        IdRevsSeqDict2 =
                dict:store(MissingId, {NotMissingRevs, Seq}, IdRevsSeqDict),
        remove_missing(IdRevsSeqDict2, Rest)
    end.


spawn_doc_copy(Cp, Source, Target, MissingRevsQueue) ->
    % Note, we could spawn many doc copy process here. Before that's possible
    % the work_queue code needs to be modified to work with multiple
    % dequeueing processes
    spawn_link(fun() ->
        doc_copy_loop(Cp, Source, Target, MissingRevsQueue)
    end).


doc_copy_loop(Cp, Source, Target, MissingRevsQueue) ->
    case couch_work_queue:dequeue(MissingRevsQueue,1) of
    closed ->
        Cp ! done;
    {ok, [{doc_id, Id}]} ->
        couch_api_wrap:open_doc(
            Source, Id, [], fun(R) -> doc_handler(R, Target, Cp) end),
        doc_copy_loop(Cp, Source, Target, MissingRevsQueue);
    {ok, [{Id, Revs, PossibleAncestors, Seq}]} ->
        couch_api_wrap:open_doc_revs(
            Source, Id, Revs, [{atts_since, PossibleAncestors}],
            fun(R, _) -> doc_handler(R, Target, Cp) end, []),
        Cp ! {seq_changes_done, {Seq, length(Revs)}},
        doc_copy_loop(Cp, Source, Target, MissingRevsQueue)
    end.

doc_handler({ok, Doc}, Target, Cp) ->
    % we are called for every rev read on the source
    Cp ! {add_stat, {#stats.docs_read, 1}},
    % now write the doc to the target.
    case couch_api_wrap:update_doc(Target, Doc, [], replicated_changes) of
    {ok, _} ->
        Cp ! {add_stat, {#stats.docs_written, 1}};
    _Error ->
        Cp ! {add_stat, {#stats.doc_write_failures, 1}}
    end;
doc_handler(_, _, _) ->
    ok.


checkpoint_interval(_State) ->
    5000.

do_checkpoint(#rep_state{current_through_seq=Seq, committed_seq=Seq} = State) ->
    State;
do_checkpoint(State) ->
    #rep_state{
        source_name=SourceName,
        target_name=TargetName,
        source = Source,
        target = Target,
        history = OldHistory,
        start_seq = StartSeq,
        current_through_seq = NewSeq,
        source_log = SourceLog,
        target_log = TargetLog,
        rep_starttime = ReplicationStartTime,
        src_starttime = SrcInstanceStartTime,
        tgt_starttime = TgtInstanceStartTime,
        stats = Stats
    } = State,
    case commit_to_both(Source, Target) of
    {SrcInstanceStartTime, TgtInstanceStartTime} ->
        ?LOG_INFO("recording a checkpoint for ~p -> ~p at source update_seq ~p",
            [SourceName, TargetName, NewSeq]),
        SessionId = couch_uuids:random(),
        NewHistoryEntry = {[
            {<<"session_id">>, SessionId},
            {<<"start_time">>, list_to_binary(ReplicationStartTime)},
            {<<"end_time">>, list_to_binary(httpd_util:rfc1123_date())},
            {<<"start_last_seq">>, StartSeq},
            {<<"end_last_seq">>, NewSeq},
            {<<"recorded_seq">>, NewSeq},
            {<<"missing_checked">>, Stats#stats.missing_checked},
            {<<"missing_found">>, Stats#stats.missing_found},
            {<<"docs_read">>, Stats#stats.docs_read},
            {<<"docs_written">>, Stats#stats.docs_written},
            {<<"doc_write_failures">>, Stats#stats.doc_write_failures}
        ]},
        % limit history to 50 entries
        NewRepHistory = {[
            {<<"session_id">>, SessionId},
            {<<"source_last_seq">>, NewSeq},
            {<<"history">>, lists:sublist([NewHistoryEntry | OldHistory], 50)}
        ]},

        try
            {ok, {SrcRevPos,SrcRevId}} = couch_api_wrap:update_doc(Source,
                SourceLog#doc{body=NewRepHistory}, [delay_commit]),
            {ok, {TgtRevPos,TgtRevId}} = couch_api_wrap:update_doc(Target,
                TargetLog#doc{body=NewRepHistory}, [delay_commit]),
            State#rep_state{
                checkpoint_history = NewRepHistory,
                committed_seq = NewSeq,
                source_log = SourceLog#doc{revs={SrcRevPos, [SrcRevId]}},
                target_log = TargetLog#doc{revs={TgtRevPos, [TgtRevId]}}
            }
        catch throw:conflict ->
            ?LOG_ERROR("checkpoint failure: conflict (are you replicating to "
                "yourself?)", []),
            State
        end;
    _Else ->
        ?LOG_INFO("rebooting ~p -> ~p from last known replication checkpoint",
            [SourceName, TargetName]),
        throw(restart)
    end.


commit_to_both(Source, Target) ->
    % commit the src async
    ParentPid = self(),
    SrcCommitPid = spawn_link(
        fun() ->
            ParentPid ! {self(), couch_api_wrap:ensure_full_commit(Source)}
        end),

    % commit tgt sync
    {ok, TargetStartTime} = couch_api_wrap:ensure_full_commit(Target),

    SourceStartTime =
    receive
    {SrcCommitPid, {ok, Timestamp}} ->
        receive
        {'EXIT', SrcCommitPid, normal} ->
            ok
        end,
        Timestamp;
    {'EXIT', SrcCommitPid, _} ->
        exit(replication_link_failure)
    end,
    {SourceStartTime, TargetStartTime}.


make_replication_id(Source, Target, UserCtx, Options) ->
    %% funky algorithm to preserve backwards compatibility
    {ok, HostName} = inet:gethostname(),
    % Port = mochiweb_socket_server:get(couch_httpd, port),
    Src = get_rep_endpoint(UserCtx, Source),
    Tgt = get_rep_endpoint(UserCtx, Target),
    Base = [HostName, Src, Tgt] ++
        case couch_util:get_value(filter, Options) of
        undefined ->
            case couch_util:get_value(doc_ids, Options) of
            undefined ->
                [];
            DocIds ->
                [DocIds]
            end;
        Filter ->
            [Filter, couch_util:get_value(query_params, Options, {[]})]
        end,
    Extension = maybe_append_options([continuous, create_target], Options),
    {couch_util:to_hex(couch_util:md5(term_to_binary(Base))), Extension}.


maybe_append_options(Options, RepOptions) ->
    lists:foldl(fun(Option, Acc) ->
        Acc ++
        case couch_util:get_value(Option, RepOptions, false) of
        true ->
            "+" ++ atom_to_list(Option);
        false ->
            ""
        end
    end, [], Options).


get_rep_endpoint(_UserCtx, #httpdb{url=Url, headers=Headers, oauth=OAuth}) ->
    case OAuth of
    nil ->
        {remote, Url, Headers};
    {OAuth} ->
        {remote, Url, Headers, OAuth}
    end;
get_rep_endpoint(UserCtx, <<DbName/binary>>) ->
    {local, DbName, UserCtx}.


compare_replication_logs(SrcDoc, TgtDoc) ->
    #doc{body={RepRecProps}} = SrcDoc,
    #doc{body={RepRecPropsTgt}} = TgtDoc,
    case couch_util:get_value(<<"session_id">>, RepRecProps) ==
            couch_util:get_value(<<"session_id">>, RepRecPropsTgt) of
    true ->
        % if the records have the same session id,
        % then we have a valid replication history
        OldSeqNum = couch_util:get_value(<<"source_last_seq">>, RepRecProps, 0),
        OldHistory = couch_util:get_value(<<"history">>, RepRecProps, []),
        {OldSeqNum, OldHistory};
    false ->
        SourceHistory = couch_util:get_value(<<"history">>, RepRecProps, []),
        TargetHistory = couch_util:get_value(<<"history">>, RepRecPropsTgt, []),
        ?LOG_INFO("Replication records differ. "
                "Scanning histories to find a common ancestor.", []),
        ?LOG_DEBUG("Record on source:~p~nRecord on target:~p~n",
                [RepRecProps, RepRecPropsTgt]),
        compare_rep_history(SourceHistory, TargetHistory)
    end.

compare_rep_history(S, T) when S =:= [] orelse T =:= [] ->
    ?LOG_INFO("no common ancestry -- performing full replication", []),
    {0, []};
compare_rep_history([{S} | SourceRest], [{T} | TargetRest] = Target) ->
    SourceId = couch_util:get_value(<<"session_id">>, S),
    case has_session_id(SourceId, Target) of
    true ->
        RecordSeqNum = couch_util:get_value(<<"recorded_seq">>, S, 0),
        ?LOG_INFO("found a common replication record with source_seq ~p",
            [RecordSeqNum]),
        {RecordSeqNum, SourceRest};
    false ->
        TargetId = couch_util:get_value(<<"session_id">>, T),
        case has_session_id(TargetId, SourceRest) of
        true ->
            RecordSeqNum = couch_util:get_value(<<"recorded_seq">>, T, 0),
            ?LOG_INFO("found a common replication record with source_seq ~p",
                [RecordSeqNum]),
            {RecordSeqNum, TargetRest};
        false ->
            compare_rep_history(SourceRest, TargetRest)
        end
    end.


has_session_id(_SessionId, []) ->
    false;
has_session_id(SessionId, [{Props} | Rest]) ->
    case couch_util:get_value(<<"session_id">>, Props, nil) of
    SessionId ->
        true;
    _Else ->
        has_session_id(SessionId, Rest)
    end.

