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

-export([start/4]).

-include("couch_db.hrl").
-include("couch_api_wrap.hrl").


-record(rep_state, {
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
    timer % checkpoint timer
    }).

-record(stats, {
    missing_checked=0,
    missing_found=0,
    docs_read=0,
    docs_written=0,
    doc_write_failures=0
    }).
        
start(Src, Tgt, Options, UserCtx) ->
    
    _Continuous = proplists:get_value(continuous, Options, false),
    _CreateTarget = proplists:get_value(create_target, Options, false),
    
    % initalize the replication state, looking for existing rep records
    % for incremental replication.
    #rep_state{source=Source,target=Target,start_seq=StartSeq} = State = 
            init_state(Src, Tgt, Options, UserCtx), 
    
    % Create the work queues
    {ok, ChangesQueue} = couch_work_queue:new(100000, 500),
    {ok, MissingRevsQueue} = couch_work_queue:new(100000, 500),
    
    % this is starts the _changes reader process. It adds the changes from
    % the source db to the ChangesQueue.
    spawn_changes_reader(self(), StartSeq, Source, ChangesQueue),
    
    % this starts the missing revs finder, it checks the target for changes
    % in the ChangesQueue to see if they exist on the target or not. If not, 
    % adds them to MissingRevsQueue.
    spawn_missing_revs_finder(self(), Target, ChangesQueue, MissingRevsQueue),
    
    % This starts the doc copy process. It gets the documents from the
    % MissingRevsQueue, copying them from the source to the target database.
    spawn_doc_copy(self(), Source, Target, MissingRevsQueue),
    
    % This is the checkpoint loop, it updates the replication record in the
    % database every X seconds, so that if the replication is interuppted,
    % it can restart near where it left off.
    {ok, State2, _Stats} = checkpoint_loop(State, gb_trees:from_orddict([]),
            #stats{}),
    couch_api_wrap:db_close(Source),        
    couch_api_wrap:db_close(Target),
    {ok, State2#rep_state.checkpoint_history}.



init_state(Src,Tgt,Options,UserCtx)->    
    {ok, Source} = couch_api_wrap:db_open(Src, [{user_ctx, UserCtx}]),
    {ok, Target} = couch_api_wrap:db_open(Tgt, [{user_ctx, UserCtx}]),

    {ok, SourceInfo} = couch_api_wrap:get_db_info(Source),
    {ok, TargetInfo} = couch_api_wrap:get_db_info(Target),

    RepId = make_replication_id(Src, Tgt, UserCtx, Options),
    DocId = ?l2b(?LOCAL_DOC_PREFIX ++ RepId),
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
    State#rep_state{timer = erlang:start_timer(checkpoint_interval(State), 
            self(), timed_checkpoint)}.


spawn_changes_reader(Cp, StartSeq, Source, ChangesQueue) ->
    spawn_link(
        fun()->
            couch_api_wrap:changes_since(Source, all_docs, StartSeq,
                fun(#doc_info{high_seq=Seq,revs=Revs}=DocInfo, _)->
                    Cp ! {seq_start, {Seq, length(Revs)}},
                    Cp ! {add_stat, {#stats.missing_checked, length(Revs)}},
                    ok = couch_work_queue:queue(ChangesQueue, DocInfo),
                    {ok, ok}
                end, ok),
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
                #doc_info{id=Id,revs=RevsInfo} <- DocInfos],
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
                    #doc_info{id=Id,revs=RevsInfo,high_seq=Seq} <- DocInfos]),
        NonMissingIdRevsSeqDict = remove_missing(IdRevsSeqDict, Missing),
        % signal the completion of these that aren't missing
        lists:foreach(fun({_Id, {Revs, Seq}})->
                Cp ! {seq_changes_done, {Seq, length(Revs)}}
            end, dict:to_list(NonMissingIdRevsSeqDict)),

        % Expand out each docs and seq into it's own work item
        lists:foreach(fun({Id, Revs, PAs})->
            % PA means "possible ancestor"
            Cp ! {add_stat, {#stats.missing_found, length(Revs)}},
            {_, Seq} = dict:fetch(Id, IdRevsSeqDict),
            ok = couch_work_queue:queue(MissingRevsQueue,
                {Id, Revs, PAs, Seq})
            end, Missing),
        missing_revs_finder_loop(Cp, Target, ChangesQueue, 
                MissingRevsQueue)
    end.


remove_missing(IdRevsSeqDict, []) ->
    IdRevsSeqDict;
remove_missing(IdRevsSeqDict, [{MissingId, MissingRevs, _}|Rest]) ->
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
    {ok, [{Id, Revs, PossibleAncestors, Seq}]} ->
        couch_api_wrap:open_doc_revs(Source, Id, Revs,
                [{atts_since,PossibleAncestors}],
                fun({ok, Doc}, _) ->
                    % we are called for every rev read on the source
                    Cp ! {add_stat, {#stats.docs_read, 1}},
                    % now write the doc to the target.
                    case couch_api_wrap:update_doc(Target, Doc, [],
                            replicated_changes) of
                    {ok, _} ->
                        Cp ! {add_stat, {#stats.docs_written, 1}};
                    _Error ->
                        Cp ! {add_stat, {#stats.doc_write_failures, 1}}
                    end;
                (_, _) ->
                    ok
                end, []),
        Cp ! {seq_changes_done, {Seq, length(Revs)}},
        doc_copy_loop(Cp, Source, Target, MissingRevsQueue)
    end.

checkpoint_loop(State, SeqsInProgress, Stats) ->
    % SeqsInProgress contains the number of revs for each seq foiund by the
    % changes process.
    receive
    {seq_start, {Seq, NumChanges}} ->
        % Add this seq to the SeqsInProgress
        SeqsInProgress2 = gb_trees:insert(Seq, NumChanges, SeqsInProgress),
        checkpoint_loop(State, SeqsInProgress2, Stats);
    {seq_changes_done, {Seq, NumChangesDone}} ->
        % decrement the # changes for this seq by NumChangesDone 
        TotalChanges = gb_trees:get(Seq, SeqsInProgress),
        case TotalChanges - NumChangesDone of
        0 ->
            % this seq is completely processed. Chck to see if it was the
            % smallest seq in progess. If so, we've made progress that can
            % be checkpointed.
            State2 =
            case gb_trees:smallest(SeqsInProgress) of
            {Seq, _} ->
                State#rep_state{current_through_seq=Seq};
            _ ->
                State
            end,
            checkpoint_loop(State2, 
                    gb_trees:delete(Seq,SeqsInProgress), Stats);
        NewTotalChanges when NewTotalChanges > 0 ->
            % Still some changes that need work done. Put the new count back.
            SeqsInProgress2 =
                gb_trees:update(Seq, NewTotalChanges, SeqsInProgress),
            checkpoint_loop(State, SeqsInProgress2, Stats)
        end;
    {add_stat, {StatPos, Val}} ->
        % Increment the stat at the pos.
        Stat = element(StatPos, Stats),
        Stats2 = setelement(StatPos, Stats, Stat + Val),
        checkpoint_loop(State, SeqsInProgress, Stats2);
    done ->
        % This means all the worker processes have completed their work.
        % Assert that all the seqs have been processed
        0 = gb_trees:size(SeqsInProgress),
        State2 = do_checkpoint(State, Stats),
        erlang:cancel_timer(State2#rep_state.timer),
        receive timed_checkpoint -> ok
        after 0 -> ok
        end,
        {ok, State2, Stats};
    timed_checkpoint ->
        % every checkpoint interval while processing
        State2 = do_checkpoint(State, Stats),
        Timer = erlang:start_timer(checkpoint_interval(State), 
                self(), timed_checkpoint),
        checkpoint_loop(State2#rep_state{timer=Timer}, SeqsInProgress, Stats)
    end.


checkpoint_interval(_State) ->
    5000.

do_checkpoint(#rep_state{current_through_seq=Seq,committed_seq=OldSeq}=State,
        _Stats) when Seq == OldSeq ->
    State;
do_checkpoint(State, Stats) ->
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
        tgt_starttime = TgtInstanceStartTime
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
    SrcCommitPid = spawn_link(fun() ->
            ParentPid ! {self(), couch_api_wrap:ensure_full_commit(Source)} end),

    % commit tgt sync
    {ok, TargetStartTime} = couch_api_wrap:ensure_full_commit(Target),

    SourceStartTime =
    receive
    {SrcCommitPid, {ok, Timestamp}} ->
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
        case proplists:get_value(filter, Options) of
        undefined ->
            [];
        Filter ->
            [Filter, proplists:get_value(query_params, Options, {[]})]
        end,
    couch_util:to_hex(erlang:md5(term_to_binary(Base))).

get_rep_endpoint(_UserCtx, #httpdb{url=Url,headers=Headers,oauth=OAuth}) ->
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
compare_rep_history([{S}|SourceRest], [{T}|TargetRest]=Target) ->
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

