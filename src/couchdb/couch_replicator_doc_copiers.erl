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

-module(couch_replicator_doc_copiers).

-export([spawn_doc_copiers/5]).

-include("couch_db.hrl").
-include("couch_api_wrap.hrl").

-define(DOC_BATCH_SIZE, 50).



spawn_doc_copiers(Cp, Source, Target, MissingRevsQueue, CopiersCount) ->
    lists:map(
        fun(CopierId) ->
            Pid = spawn_link(fun() ->
                doc_copy_loop(CopierId, Cp, Source, Target, MissingRevsQueue)
            end),
            {Pid, CopierId}
        end,
        lists:seq(1, CopiersCount)).


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
    Cp ! {add_stat, {#rep_stats.docs_read, 1}},
    update_bulk_doc_acc(Acc, Seq, Doc);

doc_handler({ok, Doc}, Seq, Target, Cp, Acc) ->
    Cp ! {add_stat, {#rep_stats.docs_read, 1}},
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
        Cp ! {add_stat, {#rep_stats.docs_written, 1}};
    {error, <<"unauthorized">>} ->
        Cp ! {add_stat, {#rep_stats.doc_write_failures, 1}},
        ?LOG_ERROR("Replicator: unauthorized to write document ~s to ~s",
            [Doc#doc.id, couch_api_wrap:db_uri(Db)]);
    _ ->
        Cp ! {add_stat, {#rep_stats.doc_write_failures, 1}}
    end,
    seqs_done([{Seq, 1}], Cp).


bulk_write_docs(Docs, Seqs, Db, Cp) ->
    case couch_api_wrap:update_docs(
        Db, Docs, [delay_commit], replicated_changes) of
    {ok, []} ->
        Cp ! {add_stat, {#rep_stats.docs_written, length(Docs)}};
    {ok, Errors} ->
        Cp ! {add_stat, {#rep_stats.doc_write_failures, length(Errors)}},
        Cp ! {add_stat, {#rep_stats.docs_written, length(Docs) - length(Errors)}},
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
