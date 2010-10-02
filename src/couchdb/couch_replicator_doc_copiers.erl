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


-record(doc_acc, {
    docs = [],
    seqs = [],
    read = 0,
    written = 0,
    wfail = 0,
    cp
}).

doc_copy_loop(CopierId, Cp, Source, Target, MissingRevsQueue) ->
    case couch_work_queue:dequeue(MissingRevsQueue, ?DOC_BATCH_SIZE) of
    closed ->
        ?LOG_DEBUG("Doc copier ~p got missing revs queue closed", [CopierId]),
        Cp ! {done, CopierId};

    {ok, [{doc_id, _} | _] = DocIds} ->
        DocAcc = lists:foldl(
            fun({doc_id, Id}, Acc) ->
                ?LOG_DEBUG("Doc copier ~p got {doc_id, ~p}", [CopierId, Id]),
                {ok, Acc2} = couch_api_wrap:open_doc_revs(
                    Source, Id, all, [],
                    fun(R, A) -> doc_handler(R, nil, Target, A) end, Acc),
                Acc2
            end,
            #doc_acc{cp = Cp}, DocIds),
        maybe_send_stat(DocAcc#doc_acc.read, #rep_stats.docs_read, Cp),
        #doc_acc{written = W, wfail = Wf} = bulk_write_docs(DocAcc, Target),
        maybe_send_stat(W, #rep_stats.docs_written, Cp),
        maybe_send_stat(Wf, #rep_stats.doc_write_failures, Cp),
        doc_copy_loop(CopierId, Cp, Source, Target, MissingRevsQueue);

    {ok, IdRevList} ->
        {Source2, DocAcc} = lists:foldl(
            fun({Id, Revs, PossibleAncestors, Seq} = IdRev, {SrcDb, BulkAcc}) ->
                ?LOG_DEBUG("Doc copier ~p got ~p", [CopierId, IdRev]),
                SrcDb2 = couch_api_wrap:maybe_reopen_db(SrcDb, Seq),
                {ok, BulkAcc2} = couch_api_wrap:open_doc_revs(
                    SrcDb2, Id, Revs, [{atts_since, PossibleAncestors}],
                    fun(R, A) -> doc_handler(R, Seq, Target, A) end,
                    BulkAcc),
                {SrcDb2, BulkAcc2}
            end,
            {Source, #doc_acc{cp = Cp}}, IdRevList),
        maybe_send_stat(DocAcc#doc_acc.read, #rep_stats.docs_read, Cp),
        #doc_acc{written = W, wfail = Wf} = bulk_write_docs(DocAcc, Target),
        maybe_send_stat(W, #rep_stats.docs_written, Cp),
        maybe_send_stat(Wf, #rep_stats.doc_write_failures, Cp),
        doc_copy_loop(CopierId, Cp, Source2, Target, MissingRevsQueue)
    end.


doc_handler({ok, #doc{atts = []} = Doc}, Seq, _Target, Acc) ->
    update_bulk_doc_acc(Acc, Seq, Doc);

doc_handler({ok, Doc}, Seq, Target, Acc) ->
    write_doc(Doc, Seq, Target, Acc);

doc_handler(_, _, _, Acc) ->
    Acc.


update_bulk_doc_acc(#doc_acc{docs = Docs, read = Read} = Acc, nil, Doc) ->
    Acc#doc_acc{
        docs = [Doc | Docs],
        read = Read + 1
    };

update_bulk_doc_acc(#doc_acc{seqs = [{Seq, Count} | Rest]} = Acc, Seq, Doc) ->
    Acc#doc_acc{
        docs = [Doc | Acc#doc_acc.docs],
        seqs = [{Seq, Count + 1} | Rest],
        read = Acc#doc_acc.read + 1
    };

update_bulk_doc_acc(#doc_acc{seqs = Seqs, read = Read} = Acc, Seq, Doc) ->
    Acc#doc_acc{
        docs = [Doc | Acc#doc_acc.docs],
        seqs = [{Seq, 1} | Seqs],
        read = Read + 1
    }.


write_doc(Doc, Seq, Db, #doc_acc{written = W, wfail = F, read = R} = Acc) ->
    Acc2 = case couch_api_wrap:update_doc(Db, Doc, [], replicated_changes) of
    {ok, _} ->
        Acc#doc_acc{written = W + 1, read = R + 1};
    {error, <<"unauthorized">>} ->
        ?LOG_ERROR("Replicator: unauthorized to write document ~s to ~s",
            [Doc#doc.id, couch_api_wrap:db_uri(Db)]),
        Acc#doc_acc{wfail = F + 1, read = R + 1};
    _ ->
        Acc#doc_acc{wfail = F + 1, read = R + 1}
    end,
    seqs_done([{Seq, 1}], Acc#doc_acc.cp),
    Acc2.


bulk_write_docs(#doc_acc{docs = []} = Acc, _) ->
    Acc;
bulk_write_docs(#doc_acc{docs = Docs, seqs = Seqs, cp = Cp} = Acc, Db) ->
    {ok, Errors} = couch_api_wrap:update_docs(
        Db, Docs, [delay_commit], replicated_changes),
    DbUri = couch_api_wrap:db_uri(Db),
    lists:foreach(
        fun({[ {<<"id">>, Id}, {<<"error">>, <<"unauthorized">>} ]}) ->
                ?LOG_ERROR("Replicator: unauthorized to write document"
                    " ~s to ~s", [Id, DbUri]);
            (_) ->
                ok
        end, Errors),
    seqs_done(Seqs, Cp),
    Acc#doc_acc{
        wfail = Acc#doc_acc.wfail + length(Errors),
        written = Acc#doc_acc.written + length(Docs) - length(Errors)
    }.


seqs_done([], _) ->
    ok;
seqs_done([{nil, _} | _], _) ->
    ok;
seqs_done(SeqCounts, Cp) ->
    Cp ! {seq_changes_done, SeqCounts}.


maybe_send_stat(0, _StatPos, _Cp) ->
    ok;
maybe_send_stat(Value, StatPos, Cp) ->
    Cp ! {add_stat, {StatPos, Value}}.

