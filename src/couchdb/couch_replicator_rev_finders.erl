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
        fun(FinderId) ->
            Pid = spawn_link(fun() ->
                missing_revs_finder_loop(FinderId, StatsProcess,
                    Target, ChangesQueue, MissingRevsQueue)
            end),
            {Pid, FinderId}
        end, lists:seq(1, RevFindersCount)).


missing_revs_finder_loop(FinderId, Cp, Target, ChangesQueue, RevsQueue) ->
    case couch_work_queue:dequeue(ChangesQueue, ?REV_BATCH_SIZE) of
    closed ->
        ok;
    {ok, DocInfos} ->
        IdRevs = [{Id, [Rev || #rev_info{rev=Rev} <- RevsInfo]} ||
                #doc_info{id=Id, revs=RevsInfo} <- DocInfos],
        ?LOG_DEBUG("Revs finder ~p got ~p IdRev pairs from queue",
            [FinderId, length(IdRevs)]),
        {ok, Missing} = couch_api_wrap:get_missing_revs(Target, IdRevs),
        ?LOG_DEBUG("Revs finder ~p found ~p missing IdRev pairs",
            [FinderId, length(Missing)]),
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
        Cp ! {seq_changes_done,
            [{Seq, length(Revs)} ||
                {_Id, {Revs, Seq}} <- dict:to_list(NonMissingIdRevsSeqDict)]},

        % Expand out each docs and seq into it's own work item
        lists:foreach(fun({Id, Revs, PAs}) ->
            % PA means "possible ancestor"
            Cp ! {add_stat, {#rep_stats.missing_found, length(Revs)}},
            {_, Seq} = dict:fetch(Id, IdRevsSeqDict),
            ok = couch_work_queue:queue(RevsQueue, {Id, Revs, PAs, Seq})
            end, Missing),
        missing_revs_finder_loop(FinderId, Cp, Target, ChangesQueue, RevsQueue)
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
