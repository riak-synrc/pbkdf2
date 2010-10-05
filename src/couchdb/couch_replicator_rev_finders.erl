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

-module(couch_replicator_rev_finders).

-export([spawn_missing_rev_finders/5]).

-include("couch_db.hrl").

-define(REV_BATCH_SIZE, 100).



spawn_missing_rev_finders(_, _, DocIds, MissingRevsQueue, _)
    when is_list(DocIds) ->
    lists:foreach(
        fun(DocId) ->
            % Ensure same behaviour as old replicator: accept a list of percent
            % encoded doc IDs.
            Id = ?l2b(couch_httpd:unquote(DocId)),
            ok = couch_work_queue:queue(MissingRevsQueue, {doc_id, Id})
        end, DocIds),
    couch_work_queue:close(MissingRevsQueue),
    [];

spawn_missing_rev_finders(StatsProcess,
        Target, ChangesQueue, MissingRevsQueue, RevFindersCount) ->
    lists:map(
        fun(_) ->
            spawn_link(fun() ->
                missing_revs_finder_loop(StatsProcess,
                    Target, ChangesQueue, MissingRevsQueue)
            end)
        end, lists:seq(1, RevFindersCount)).


missing_revs_finder_loop(Cp, Target, ChangesQueue, RevsQueue) ->
    case couch_work_queue:dequeue(ChangesQueue, ?REV_BATCH_SIZE) of
    closed ->
        ok;
    {ok, DocInfos} ->
        IdRevs = [{Id, [Rev || #rev_info{rev=Rev} <- RevsInfo]} ||
                #doc_info{id=Id, revs=RevsInfo} <- DocInfos],
        ?LOG_DEBUG("Revs finder ~p got ~p IdRev pairs from queue",
            [self(), length(IdRevs)]),
        {ok, Missing} = couch_api_wrap:get_missing_revs(Target, IdRevs),
        ?LOG_DEBUG("Revs finder ~p found ~p missing IdRev pairs",
            [self(), length(Missing)]),
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
        report_non_missing(NonMissingIdRevsSeqDict, Cp),

        % Expand out each docs and seq into it's own work item
        MissingCount = lists:foldl(
            fun({Id, Revs, PAs}, Count) ->
                % PA means "possible ancestor"
                {_, Seq} = dict:fetch(Id, IdRevsSeqDict),
                ok = couch_work_queue:queue(RevsQueue, {Id, Revs, PAs, Seq}),
                Count + length(Revs)
            end, 0, Missing),
        send_missing_found(MissingCount, Cp),
        missing_revs_finder_loop(Cp, Target, ChangesQueue, RevsQueue)
    end.


report_non_missing(RevsDict, Cp) ->
    case dict:size(RevsDict) of
    0 ->
        ok;
    N when N > 0 ->
        SeqsDone = [{Seq, length(Revs)} ||
            {_Id, {Revs, Seq}} <- dict:to_list(RevsDict)],
        ok = gen_server:cast(Cp, {seq_changes_done, SeqsDone})
    end.


send_missing_found(0, _Cp) ->
    ok;
send_missing_found(Value, Cp) ->
    ok = gen_server:cast(Cp, {add_stats, #rep_stats{missing_found = Value}}).


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
