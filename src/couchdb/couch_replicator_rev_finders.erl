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

-export([spawn_missing_rev_finders/6]).

-include("couch_db.hrl").



spawn_missing_rev_finders(StatsProcess,
        Target, ChangesQueue, MissingRevsQueue, RevFindersCount, BatchSize) ->
    lists:map(
        fun(_) ->
            spawn_link(fun() ->
                missing_revs_finder_loop(StatsProcess,
                    Target, ChangesQueue, MissingRevsQueue, BatchSize)
            end)
        end, lists:seq(1, RevFindersCount)).


missing_revs_finder_loop(Cp, Target, ChangesQueue, RevsQueue, BatchSize) ->
    case couch_work_queue:dequeue(ChangesQueue, BatchSize) of
    closed ->
        ok;
    {ok, DocInfos} ->
        IdRevs = [{Id, [Rev || #rev_info{rev=Rev} <- RevsInfo]} ||
                #doc_info{id=Id, revs=RevsInfo} <- DocInfos],
        ?LOG_DEBUG("Revs finder ~p got ~p IdRev pairs from queue",
            [self(), length(IdRevs)]),
        Target2 = reopen_db(Target),
        {ok, Missing} = couch_api_wrap:get_missing_revs(Target2, IdRevs),
        ?LOG_DEBUG("Revs finder ~p found ~p missing IdRev pairs",
            [self(), length(Missing)]),
        % Figured out which on the target are missing.
        % Missing contains the id and revs missing, and any possible
        % ancestors that already exist on the target. This enables
        % incremental attachment replication, so the source only needs to send
        % attachments modified since the common ancestor on target.
        queue_missing_revs(Missing, DocInfos, RevsQueue, Cp),
        missing_revs_finder_loop(Cp, Target2, ChangesQueue, RevsQueue, BatchSize)
    end.


queue_missing_revs([], _, _, _) ->
    ok;
queue_missing_revs(Missing, DocInfos, Queue, Cp) ->
    IdRevsSeqDict = dict:from_list(
        [{Id, {[Rev || #rev_info{rev = Rev} <- RevsInfo], Seq}} ||
            #doc_info{id = Id, revs = RevsInfo, high_seq = Seq} <- DocInfos]),
    {MissingCount, QueueItemList} = lists:foldl(
        fun({Id, Revs, PAs}, {Count, Q}) ->
            {_, Seq} = dict:fetch(Id, IdRevsSeqDict),
            {Count + length(Revs), [{Id, Revs, PAs, Seq} | Q]}
        end,
        {0, []},
        Missing),
    [{_, _, _, LargestSeq} | _] = QueueItemList,
    ok = gen_server:cast(Cp, {seq_start, {LargestSeq, MissingCount}}),
    ok = couch_work_queue:queue(
           Queue, {LargestSeq, QueueItemList}).


reopen_db(#db{main_pid = Pid, user_ctx = UserCtx}) ->
    {ok, NewDb} = gen_server:call(Pid, get_db, infinity),
    NewDb#db{user_ctx = UserCtx};
reopen_db(HttpDb) ->
    HttpDb.
