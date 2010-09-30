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
-export([replicate/1]).

% gen_server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-include("couch_db.hrl").
-include("couch_api_wrap.hrl").

-import(couch_util, [
    get_value/2,
    get_value/3
]).

% Can't be greater than the maximum number of child restarts specified
% in couch_rep_sup.erl.
-define(MAX_RESTARTS, 3).
-define(DOC_BATCH_SIZE, 50).


-record(stats, {
    missing_checked = 0,
    missing_found = 0,
    docs_read = 0,
    docs_written = 0,
    doc_write_failures = 0
    }).

-record(rep_state, {
    rep_details,
    source_name,
    target_name,
    source,
    target,
    history,
    checkpoint_history,
    start_seq,
    current_through_seq,
    next_through_seqs = ordsets:new(),
    is_successor_seq,
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
    doc_copiers,
    finished_doc_copiers = 0,
    seqs_in_progress = gb_trees:from_orddict([]),
    stats = #stats{}
    }).


replicate(#rep{id = RepId, options = Options} = Rep) ->
    case get_value(cancel, Options, false) of
    true ->
        end_replication(RepId);
    false ->
        {ok, Listener} = rep_result_listener(RepId),
        Result = do_replication_loop(Rep),
        couch_replication_notifier:stop(Listener),
        Result
    end.


do_replication_loop(#rep{options = Options, source = Src} = Rep) ->
    DocIds = get_value(doc_ids, Options),
    Continuous = get_value(continuous, Options, false),
    Seq = case {DocIds, Continuous} of
    {undefined, false} ->
        last_seq(Src, Rep#rep.user_ctx);
    _ ->
        undefined
    end,
    do_replication_loop(Rep, Seq).

do_replication_loop(#rep{id = {BaseId,_} = Id, options = Options} = Rep, Seq) ->
    case start_replication(Rep) of
    {ok, _Pid} ->
        case get_value(continuous, Options, false) of
        true ->
            {ok, {continuous, ?l2b(BaseId)}};
        false ->
            Result = wait_for_result(Id),
            maybe_retry(Result, Rep, Seq)
        end;
    Error ->
        Error
    end.


maybe_retry(RepResult, _Rep, undefined) ->
    RepResult;
maybe_retry({ok, {Props}} = Result, Rep, Seq) ->
    case get_value(source_last_seq, Props) >= Seq of
    true ->
        Result;
    false ->
        do_replication_loop(Rep, Seq)
    end;
maybe_retry(RepResult, _Rep, _Seq) ->
    RepResult.


last_seq(DbName, UserCtx) ->
    case (catch couch_api_wrap:db_open(DbName, [{user_ctx, UserCtx}])) of
    {ok, Db} ->
        {ok, DbInfo} = couch_api_wrap:get_db_info(Db),
        couch_api_wrap:db_close(Db),
        get_value(<<"update_seq">>, DbInfo);
    _ ->
        undefined
    end.


start_replication(#rep{id = {BaseId, Extension}} = Rep) ->
    RepChildId = BaseId ++ Extension,
    ChildSpec = {
        RepChildId,
        {gen_server, start_link, [?MODULE, Rep, []]},
        transient,
        1,
        worker,
        [?MODULE]
    },
    case supervisor:start_child(couch_rep_sup, ChildSpec) of
    {ok, Pid} ->
        ?LOG_INFO("starting new replication ~p at ~p", [RepChildId, Pid]),
        {ok, Pid};
    {error, already_present} ->
        case supervisor:restart_child(couch_rep_sup, RepChildId) of
        {ok, Pid} ->
            ?LOG_INFO("starting replication ~p at ~p", [RepChildId, Pid]),
            {ok, Pid};
        {error, running} ->
            %% this error occurs if multiple replicators are racing
            %% each other to start and somebody else won.  Just grab
            %% the Pid by calling start_child again.
            {error, {already_started, Pid}} =
                supervisor:start_child(couch_rep_sup, ChildSpec),
            ?LOG_DEBUG("replication ~p already running at ~p",
                [RepChildId, Pid]),
            {ok, Pid};
        {error, _} = Err ->
            Err
        end;
    {error, {already_started, Pid}} ->
        ?LOG_DEBUG("replication ~p already running at ~p", [RepChildId, Pid]),
        {ok, Pid};
    {error, {Error, _}} ->
        {error, Error}
    end.


rep_result_listener(RepId) ->
    ReplyTo = self(),
    {ok, _Listener} = couch_replication_notifier:start_link(
        fun({_, RepId2, _} = Ev) when RepId2 =:= RepId ->
                ReplyTo ! Ev;
            (_) ->
                ok
        end).


wait_for_result(RepId) ->
    wait_for_result(RepId, ?MAX_RESTARTS).

wait_for_result(RepId, RetriesLeft) ->
    receive
    {finished, RepId, RepResult} ->
        {ok, RepResult};
    {error, RepId, Reason} ->
        case RetriesLeft > 0 of
        true ->
            wait_for_result(RepId, RetriesLeft - 1);
        false ->
            {error, Reason}
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
    throw:Error ->
        {stop, Error}
    end.

do_init(#rep{options = Options} = Rep) ->
    process_flag(trap_exit, true),

    #rep_state{
        source = Source,
        target = Target,
        start_seq = StartSeq
    } = State = init_state(Rep),

    {ok, MissingRevsQueue} = couch_work_queue:new(
        [{max_size, 100000}, {max_items, 500}, {multi_workers, true}]),

    case get_value(doc_ids, Options) of
    undefined ->
        {ok, ChangesQueue} = couch_work_queue:new(
            [{max_size, 100000}, {max_items, 500}]),

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
        MissingRevsFinder = case DocIds of
        [] ->
            % avoid getting the doc copier process get blocked if it dequeues
            % before the queue is closed
            couch_work_queue:close(MissingRevsQueue),
            nil;
        Ids when is_list(Ids) ->
            spawn_missing_revs_finder(self(), Target, Ids, MissingRevsQueue)
        end
    end,

    % This starts the doc copy processes. They fetch documents from the
    % MissingRevsQueue and copy them from the source to the target database.
    DocCopiers = spawn_doc_copiers(self(), Source, Target, MissingRevsQueue),

    {ok, State#rep_state{
            missing_revs_queue = MissingRevsQueue,
            changes_queue = ChangesQueue,
            changes_reader = ChangesReader,
            missing_revs_finder = MissingRevsFinder,
            doc_copiers = DocCopiers,
            is_successor_seq = get_value(is_successor_seq, Options,
                fun(Seq, NextSeq) -> (Seq + 1) =:= NextSeq end)
        }
    }.


handle_info({seq_start, {Seq, NumChanges}}, State) ->
    SeqsInProgress2 = gb_trees:insert(Seq, NumChanges,
        State#rep_state.seqs_in_progress),
    {noreply, State#rep_state{seqs_in_progress = SeqsInProgress2}};

handle_info({seq_changes_done, {Seq, NumChangesDone}}, State) ->
    #rep_state{
        seqs_in_progress = SeqsInProgress,
        next_through_seqs = DoneSeqs,
        is_successor_seq = IsSuccFun
    } = State,
    % Decrement the # changes for this seq by NumChangesDone.
    TotalChanges = gb_trees:get(Seq, SeqsInProgress),
    NewState = case TotalChanges - NumChangesDone of
    0 ->
        % This seq is completely processed. Check to see if it was the
        % smallest seq in progess. If so, we've made progress that can
        % be checkpointed.
        State2 = case gb_trees:smallest(SeqsInProgress) of
        {Seq, _} ->
            {CheckpointSeq, DoneSeqs2} = next_seq_before_gap(
                Seq, DoneSeqs, IsSuccFun),
            State#rep_state{
                current_through_seq = CheckpointSeq,
                next_through_seqs = DoneSeqs2
            };
        _ ->
            DoneSeqs2 = ordsets:add_element(Seq, DoneSeqs),
            State#rep_state{next_through_seqs = DoneSeqs2}
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

handle_info({done, _CopierId}, State) ->
    #rep_state{
        finished_doc_copiers = Finished,
        doc_copiers = DocCopiers,
        next_through_seqs = DoneSeqs,
        current_through_seq = Seq
    } = State,
    State1 = State#rep_state{finished_doc_copiers = Finished + 1},
    case length(DocCopiers) - 1 of
    Finished ->
        % This means all the worker processes have completed their work.
        % Assert that all the seqs have been processed.
        0 = gb_trees:size(State#rep_state.seqs_in_progress),
        LastSeq = case DoneSeqs of
        [] ->
            Seq;
        _ ->
            lists:max([Seq, lists:last(DoneSeqs)])
        end,
        State2 = do_checkpoint(State1#rep_state{current_through_seq = LastSeq}),
        cancel_timer(State2),
        {stop, normal, State2};
    _ ->
        {noreply, State1}
    end;

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
    {stop, changes_queue_died, State};

handle_info({'EXIT', Pid, Reason}, State) ->
    #rep_state{doc_copiers = DocCopiers} = State,
    case get_value(Pid, DocCopiers) of
    undefined ->
        cancel_timer(State),
        {stop, {unknown_process_died, Pid, Reason}, State};
    CopierId ->
        case Reason of
        normal ->
            {noreply, State};
        _ ->
            cancel_timer(State),
            ?LOG_ERROR("DocCopier process ~p died with reason: ~p",
                [CopierId, Reason]),
            {stop, doc_copier_died, State}
        end
    end.


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


terminate(normal, #rep_state{rep_details = #rep{id = RepId}} = State) ->
    terminate_cleanup(State),
    couch_replication_notifier:notify({finished, RepId, get_result(State)});

terminate(shutdown, State) ->
    % cancelled replication throught ?MODULE:end_replication/1
    terminate_cleanup(State);

terminate(Reason, #rep_state{rep_details = #rep{id = RepId}} = State) ->
    terminate_cleanup(State),
    couch_replication_notifier:notify({error, RepId, Reason}).


terminate_cleanup(#rep_state{source = Source, target = Target}) ->
    couch_api_wrap:db_close(Source),
    couch_api_wrap:db_close(Target).


start_timer(#rep_state{rep_details = #rep{options = Options}} = State) ->
    case get_value(doc_ids, Options) of
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


get_result(#rep_state{stats = Stats, rep_details = Rep} = State) ->
    case get_value(doc_ids, Rep#rep.options) of
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


init_state(Rep) ->
    #rep{
        id = {BaseId, _Ext},
        source = Src, target = Tgt,
        options = Options, user_ctx = UserCtx
    } = Rep,
    {ok, Source} = couch_api_wrap:db_open(Src, [{user_ctx, UserCtx}]),
    {ok, Target} = couch_api_wrap:db_open(Tgt, [{user_ctx, UserCtx}],
        get_value(create_target, Options, false)),

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
        rep_details = Rep,
        source_name = couch_api_wrap:db_uri(Source),
        target_name = couch_api_wrap:db_uri(Target),
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
        src_starttime = get_value(<<"instance_start_time">>, SourceInfo),
        tgt_starttime = get_value(<<"instance_start_time">>, TargetInfo)
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


missing_revs_finder_loop(_, _, DocIds, MissingRevsQueue) when is_list(DocIds) ->
    lists:foreach(
        fun(DocId) ->
            % Ensure same behaviour as old replicator: accept a list of percent
            % encoded doc IDs.
            Id = ?l2b(couch_httpd:unquote(DocId)),
            ok = couch_work_queue:queue(MissingRevsQueue, {doc_id, Id})
        end, DocIds),
    couch_work_queue:close(MissingRevsQueue);

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


spawn_doc_copiers(Cp, Source, Target, MissingRevsQueue) ->
    Count = ?l2i(couch_config:get("replicator", "copy_processes", "10")),
    lists:map(
        fun(CopierId) ->
            Pid = spawn_link(fun() ->
                doc_copy_loop(CopierId, Cp, Source, Target, MissingRevsQueue)
            end),
            {Pid, CopierId}
        end,
        lists:seq(1, Count)).


doc_copy_loop(CopierId, Cp, Source, Target, MissingRevsQueue) ->
    case couch_work_queue:dequeue(MissingRevsQueue, ?DOC_BATCH_SIZE) of
    closed ->
        ?LOG_DEBUG("Doc copier ~p got missing revs queue closed", [CopierId]),
        Cp ! {done, CopierId};
    {ok, [{doc_id, _} | _] = DocIds} ->
        {BulkList, []} = lists:foldl(
            fun({doc_id, Id}, Acc) ->
                ?LOG_DEBUG("Doc copier ~p got {doc_id, ~p}", [CopierId, Id]),
                {ok, Acc2} = couch_api_wrap:open_doc_revs(
                    Source, Id, all, [],
                    fun(R, A) -> doc_handler(R, nil, Target, Cp, A) end, Acc),
                Acc2
            end,
            {[], []}, DocIds),
        bulk_write_docs(lists:reverse(BulkList), [], Target, Cp),
        doc_copy_loop(CopierId, Cp, Source, Target, MissingRevsQueue);
    {ok, IdRevList} ->
        {Source2, {BulkList, SeqList}} = lists:foldl(
            fun({Id, Revs, PossibleAncestors, Seq} = IdRev, {SrcDb, BulkAcc}) ->
                ?LOG_DEBUG("Doc copier ~p got ~p", [CopierId, IdRev]),
                SrcDb2 = couch_api_wrap:maybe_reopen_db(SrcDb, Seq),
                {ok, BulkAcc2} = couch_api_wrap:open_doc_revs(
                    SrcDb2, Id, Revs, [{atts_since, PossibleAncestors}],
                    fun(R, A) -> doc_handler(R, Seq, Target, Cp, A) end,
                    BulkAcc),
                {SrcDb2, BulkAcc2}
            end,
            {Source, {[], []}}, IdRevList),
        bulk_write_docs(
            lists:reverse(BulkList),
            lists:reverse(SeqList),
            Target,
            Cp),
        doc_copy_loop(CopierId, Cp, Source2, Target, MissingRevsQueue)
    end.


doc_handler({ok, #doc{atts = []} = Doc}, Seq, _Target, Cp, Acc) ->
    Cp ! {add_stat, {#stats.docs_read, 1}},
    update_bulk_doc_acc(Acc, Seq, Doc);

doc_handler({ok, Doc}, Seq, Target, Cp, Acc) ->
    Cp ! {add_stat, {#stats.docs_read, 1}},
    write_doc(Doc, Seq, Target, Cp),
    Acc;

doc_handler(_, _, _, _, Acc) ->
    Acc.


update_bulk_doc_acc({DocAcc, SeqAcc}, nil, Doc) ->
    {[Doc | DocAcc], SeqAcc};
update_bulk_doc_acc({DocAcc, [{Seq, Count} | RestSeq]}, Seq, Doc) ->
    {[Doc | DocAcc], [{Seq, Count + 1} | RestSeq]};
update_bulk_doc_acc({DocAcc, SeqAcc}, Seq, Doc) ->
    {[Doc | DocAcc], [{Seq, 1} | SeqAcc]}.


write_doc(Doc, Seq, Db, Cp) ->
    case couch_api_wrap:update_doc(Db, Doc, [], replicated_changes) of
    {ok, _} ->
        Cp ! {add_stat, {#stats.docs_written, 1}};
    {error, <<"unauthorized">>} ->
        Cp ! {add_stat, {#stats.doc_write_failures, 1}},
        ?LOG_ERROR("Replicator: unauthorized to write document ~s to ~s",
            [Doc#doc.id, couch_api_wrap:db_uri(Db)]);
    _ ->
        Cp ! {add_stat, {#stats.doc_write_failures, 1}}
    end,
    seqs_done([{Seq, 1}], Cp).


bulk_write_docs(Docs, Seqs, Db, Cp) ->
    case couch_api_wrap:update_docs(
        Db, Docs, [delay_commit], replicated_changes) of
    {ok, []} ->
        Cp ! {add_stat, {#stats.docs_written, length(Docs)}};
    {ok, Errors} ->
        Cp ! {add_stat, {#stats.doc_write_failures, length(Errors)}},
        Cp ! {add_stat, {#stats.docs_written, length(Docs) - length(Errors)}},
        DbUri = couch_api_wrap:db_uri(Db),
        lists:foreach(
            fun({[ {<<"id">>, Id}, {<<"error">>, <<"unauthorized">>} ]}) ->
                    ?LOG_ERROR("Replicator: unauthorized to write document"
                        " ~s to ~s", [Id, DbUri]);
                (_) ->
                    ok
            end, Errors)
    end,
    seqs_done(Seqs, Cp).


seqs_done(SeqCounts, Cp) ->
    lists:foreach(fun({nil, _}) ->
            ok;
        (SeqCount) ->
            Cp ! {seq_changes_done, SeqCount}
        end, SeqCounts).


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


next_seq_before_gap(Seq, [], _IsSuccFun) ->
    {Seq, []};

next_seq_before_gap(Seq, [Next | NextSeqs] = AllSeqs , IsSuccFun) ->
    case IsSuccFun(Seq, Next) of
    false ->
        {Seq, AllSeqs};
    true ->
        next_seq_before_gap(Next, NextSeqs, IsSuccFun)
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


compare_replication_logs(SrcDoc, TgtDoc) ->
    #doc{body={RepRecProps}} = SrcDoc,
    #doc{body={RepRecPropsTgt}} = TgtDoc,
    case get_value(<<"session_id">>, RepRecProps) ==
            get_value(<<"session_id">>, RepRecPropsTgt) of
    true ->
        % if the records have the same session id,
        % then we have a valid replication history
        OldSeqNum = get_value(<<"source_last_seq">>, RepRecProps, 0),
        OldHistory = get_value(<<"history">>, RepRecProps, []),
        {OldSeqNum, OldHistory};
    false ->
        SourceHistory = get_value(<<"history">>, RepRecProps, []),
        TargetHistory = get_value(<<"history">>, RepRecPropsTgt, []),
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
    SourceId = get_value(<<"session_id">>, S),
    case has_session_id(SourceId, Target) of
    true ->
        RecordSeqNum = get_value(<<"recorded_seq">>, S, 0),
        ?LOG_INFO("found a common replication record with source_seq ~p",
            [RecordSeqNum]),
        {RecordSeqNum, SourceRest};
    false ->
        TargetId = get_value(<<"session_id">>, T),
        case has_session_id(TargetId, SourceRest) of
        true ->
            RecordSeqNum = get_value(<<"recorded_seq">>, T, 0),
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
    case get_value(<<"session_id">>, Props, nil) of
    SessionId ->
        true;
    _Else ->
        has_session_id(SessionId, Rest)
    end.

