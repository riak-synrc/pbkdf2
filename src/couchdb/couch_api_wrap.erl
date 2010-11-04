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

-module(couch_api_wrap).

% This module wraps the native erlang API, and allows for performing
% operations on a remote vs. local databases via the same API.
%
% Notes:
% Many options and apis aren't yet supported here, they are added as needed.
%
% This file neesds a lot of work to "robustify" the common failures, and
% convert the json errors back to Erlang style errors.
%
% Also, we open a new connection for every HTTP call, to avoid the
% problems when requests are pipelined over a single connection and earlier
% requests that fail and disconnect don't cause network errors for other
% requests. This should eventually be optimized so each process has it's own
% connection that's kept alive between requests.
%

-include("couch_db.hrl").
-include("couch_api_wrap.hrl").

-export([
    db_open/2,
    db_open/3,
    maybe_reopen_db/2,
    db_close/1,
    get_db_info/1,
    update_doc/3,
    update_doc/4,
    update_docs/3,
    update_docs/4,
    ensure_full_commit/1,
    get_missing_revs/2,
    open_doc/3,
    open_doc_revs/6,
    changes_since/5,
    db_uri/1
    ]).

-import(couch_api_wrap_httpc, [
    httpdb_setup/1,
    send_req/3
    ]).

-import(couch_util, [
    encode_doc_id/1,
    get_value/2,
    get_value/3
    ]).


db_uri(#httpdb{url = Url}) ->
    couch_util:url_strip_password(Url);

db_uri(#db{name = Name}) ->
    ?b2l(Name).


db_open(Db, Options) ->
    db_open(Db, Options, false).

db_open(#httpdb{} = Db, _Options, Create) ->
    case Create of
    false ->
        ok;
    true ->
        send_req(Db, [{method, put}], fun(_, _, _) -> ok end)
    end,
    send_req(Db, [{method, head}],
        fun(200, _, _) ->
            {ok, Db};
        (401, _, _) ->
            throw({unauthorized, ?l2b(db_uri(Db))});
        (_, _, _) ->
            throw({db_not_found, ?l2b(Db#httpdb.url)})
        end);
db_open(DbName, Options, Create) ->
    case Create of
    false ->
        ok;
    true ->
        case couch_db:create(DbName, Options) of
        {ok, _Db} ->
            ok;
        file_exists ->
            ok
        end
    end,
    case (catch couch_db:open(DbName, Options)) of
    {not_found, _Reason} ->
        throw({db_not_found, DbName});
    {ok, _Db2} = Success ->
        Success;
    {unauthorized, _} ->
        throw({unauthorized, DbName})
    end.

db_close(#httpdb{}) ->
    ok;
db_close(DbName) ->
    couch_db:close(DbName).


get_db_info(#httpdb{} = Db) ->
    send_req(Db, [],
        fun(200, _, {Props}) ->
            {ok, Props}
        end);
get_db_info(Db) ->
    {ok, Info} = couch_db:get_db_info(Db),
    {ok, [{couch_util:to_binary(K), V} || {K, V} <- Info]}.


ensure_full_commit(#httpdb{} = Db) ->
    send_req(
        Db,
        [{method, post}, {path, "_ensure_full_commit"},
            {headers, [{"Content-Type", "application/json"}]}],
        fun(201, _, {Props}) ->
            {ok, get_value(<<"instance_start_time">>, Props)}
        end);
ensure_full_commit(Db) ->
    couch_db:ensure_full_commit(Db).


get_missing_revs(#httpdb{} = Db, IdRevs) ->
    JsonBody = {[{Id, couch_doc:revs_to_strs(Revs)} || {Id, Revs} <- IdRevs]},
    send_req(
        Db,
        [{method, post}, {path, "_revs_diff"}, {body, ?JSON_ENCODE(JsonBody)}],
        fun(200, _, {Props}) ->
            ConvertToNativeFun = fun({Id, {Result}}) ->
                MissingRevs = couch_doc:parse_revs(
                    get_value(<<"missing">>, Result)
                ),
                PossibleAncestors = couch_doc:parse_revs(
                    get_value(<<"possible_ancestors">>, Result, [])
                ),
                {Id, MissingRevs, PossibleAncestors}
            end,
            {ok, lists:map(ConvertToNativeFun, Props)}
        end);
get_missing_revs(Db, IdRevs) ->
    couch_db:get_missing_revs(Db, IdRevs).



open_doc_revs(#httpdb{} = HttpDb, Id, Revs, Options, Fun, Acc) ->
    RevStr = case Revs of
    all ->
        "all";
    _ ->
        ?JSON_ENCODE(couch_doc:revs_to_strs(Revs))
    end,
    QArgs = [
        {"revs", "true"}, {"open_revs", RevStr} |
        options_to_query_args(Options, [])
    ],
    Self = self(),
    Streamer = spawn_link(fun() ->
            send_req(
                HttpDb,
                [{path, encode_doc_id(Id)}, {qs, QArgs},
                    {ibrowse_options, [{stream_to, {self(), once}}]},
                    {headers, [{"accept", "multipart/mixed"}]}],
                fun(200, Headers, StreamDataFun) ->
                    couch_httpd:parse_multipart_request(
                        get_value("Content-Type", Headers),
                        StreamDataFun,
                        fun(Ev) -> mp_parse_mixed(Ev) end)
                end),
            unlink(Self)
        end),
    receive_docs(Streamer, Fun, Acc);
open_doc_revs(Db, Id, Revs, Options, Fun, Acc) ->
    {ok, Results} = couch_db:open_doc_revs(Db, Id, Revs, Options),
    {ok, lists:foldl(Fun, Acc, Results)}.


open_doc(#httpdb{} = Db, Id, Options) ->
    send_req(
        Db,
        [{path, encode_doc_id(Id)}, {qs, options_to_query_args(Options, [])}],
        fun(200, _, Body) ->
            {ok, couch_doc:from_json_obj(Body)};
        (_, _, {Props}) ->
            {error, get_value(<<"error">>, Props)}
        end);
open_doc(Db, Id, Options) ->
    couch_db:open_doc(Db, Id, Options).


maybe_reopen_db(#httpdb{} = Db, _TargetSeq) ->
    Db;
maybe_reopen_db(#db{update_seq = UpSeq, main_pid = Pid} = Db, TargetSeq) ->
    case TargetSeq > UpSeq of
    true ->
        {ok, Db2} = gen_server:call(Pid, get_db, infinity),
        Db2;
    false ->
        Db
    end.

update_doc(Db, Doc, Options) ->
    update_doc(Db, Doc, Options, interactive_edit).

update_doc(#httpdb{} = HttpDb, #doc{id = DocId} = Doc, Options, Type) ->
    QArgs = case Type of
    replicated_changes ->
        [{"new_edits", "false"}];
    _ ->
        []
    end ++ options_to_query_args(Options, []),
    Boundary = couch_uuids:random(),
    JsonBytes = ?JSON_ENCODE(
        couch_doc:to_json_obj(Doc, [revs, attachments, follows | Options])),
    {ContentType, Len} = couch_doc:len_doc_to_multi_part_stream(Boundary,
        JsonBytes, Doc#doc.atts, false),
    Headers = case lists:member(delay_commit, Options) of
    true ->
        [{"X-Couch-Full-Commit", "false"}];
    false ->
        []
    end ++ [{"Content-Type", ?b2l(ContentType)}, {"Content-Length", Len}],
    Self = self(),
    DocStreamer = spawn_link(fun() ->
        couch_doc:doc_to_multi_part_stream(
            Boundary, JsonBytes, Doc#doc.atts,
            fun(Data) ->
                receive {get_data, From} ->
                    From ! {data, Data}
                end
            end, false),
        unlink(Self)
    end),
    SendFun = fun(0) ->
            eof;
        (LenLeft) when LenLeft > 0 ->
            DocStreamer ! {get_data, self()},
            receive {data, Data} ->
                {ok, Data, LenLeft - iolist_size(Data)}
            end
    end,
    send_req(
        HttpDb,
        [{method, put}, {path, encode_doc_id(DocId)},
            {qs, QArgs}, {headers, Headers}, {body, {SendFun, Len}}],
        fun(Code, _, {Props}) when Code =:= 200 orelse Code =:= 201 ->
                {ok, couch_doc:parse_rev(get_value(<<"rev">>, Props))};
            (_, _, {Props}) ->
                {error, get_value(<<"error">>, Props)}
        end);
update_doc(Db, Doc, Options, Type) ->
    try
        couch_db:update_doc(Db, Doc, Options, Type)
    catch
    throw:{unauthorized, _} ->
        {error, <<"unauthorized">>}
    end.


update_docs(Db, DocList, Options) ->
    update_docs(Db, DocList, Options, interactive_edit).

update_docs(#httpdb{} = HttpDb, DocList, Options, UpdateType) ->
    FullCommit = atom_to_list(not lists:member(delay_commit, Options)),
    Part1 = case UpdateType of
    replicated_changes ->
        {prefix, <<"{\"new_edits\":false,\"docs\":[">>};
    interactive_edit ->
        {prefix, <<"{\"docs\":[">>}
    end,
    BodyFun = fun(eof) ->
            eof;
        ([]) ->
            {ok, <<"]}">>, eof};
        ([{prefix, Prefix} | Rest]) ->
            {ok, Prefix, Rest};
        ([Doc]) when is_binary(Doc) ->
            {ok, Doc, []};
        ([Doc | RestDocs]) when is_binary(Doc) ->
            {ok, [Doc, ","], RestDocs};
        ([Doc]) when is_record(Doc, doc) ->
            DocJson = couch_doc:to_json_obj(Doc, [revs, attachments]),
            {ok, ?JSON_ENCODE(DocJson), []};
        ([Doc | RestDocs]) when is_record(Doc, doc) ->
            DocJson = couch_doc:to_json_obj(Doc, [revs, attachments]),
            {ok, [?JSON_ENCODE(DocJson), ","], RestDocs}
    end,
    send_req(
        HttpDb,
        [{method, post}, {path, "_bulk_docs"},
            {body, {chunkify, BodyFun, [Part1 | DocList]}},
            {headers, [
                {"X-Couch-Full-Commit", FullCommit},
                {"Content-Type", "application/json"} ]}],
        fun(201, _, Results) when is_list(Results) ->
                {ok, bulk_results_to_errors(DocList, Results, remote)};
           (417, _, Results) when is_list(Results) ->
                {ok, bulk_results_to_errors(DocList, Results, remote)}
        end);
update_docs(Db, DocList, Options, UpdateType) ->
    Result = couch_db:update_docs(Db, DocList, Options, UpdateType),
    {ok, bulk_results_to_errors(DocList, Result, UpdateType)}.


changes_since(#httpdb{} = HttpDb, Style, StartSeq, UserFun, Options) ->
    QArgs = changes_q_args(
        [{"style", atom_to_list(Style)}, {"since", integer_to_list(StartSeq)}],
        Options),
    send_req(
        % Shouldn't be infinity, but somehow if it's not, issues arise
        % frequently with ibrowse.
        HttpDb#httpdb{timeout = infinity},
        [{path, "_changes"}, {qs, QArgs},
            {ibrowse_options, [{stream_to, {self(), once}}]}],
        fun(200, _, DataStreamFun) ->
            case couch_util:get_value(continuous, Options, false) of
            true ->
                continuous_changes(DataStreamFun, UserFun);
            false ->
                EventFun = fun(Ev) ->
                    changes_ev1(Ev, fun(DocInfo, _) -> UserFun(DocInfo) end, [])
                end,
                json_stream_parse:events(DataStreamFun, EventFun)
            end
        end);
changes_since(Db, Style, StartSeq, UserFun, Options) ->
    Args = #changes_args{
        style = Style,
        since = StartSeq,
        filter = ?b2l(get_value(filter, Options, <<>>)),
        feed = case get_value(continuous, Options, false) of
            true ->
                "continuous";
            false ->
                "normal"
        end,
        timeout = infinity
    },
    QueryParams = get_value(query_params, Options, {[]}),
    Req = changes_json_req(Db, Args#changes_args.filter, QueryParams),
    ChangesFeedFun = couch_changes:handle_changes(Args, {json_req, Req}, Db),
    ChangesFeedFun(fun({change, Change, _}, _) ->
            UserFun(json_to_doc_info(Change));
        (_, _) ->
            ok
    end).


% internal functions

changes_q_args(BaseQS, Options) ->
    case get_value(filter, Options) of
    undefined ->
        BaseQS;
    FilterName ->
        {Params} = get_value(query_params, Options, {[]}),
        [{"filter", ?b2l(FilterName)} | lists:foldl(
            fun({K, V}, QSAcc) ->
                Ks = couch_util:to_list(K),
                case lists:keymember(Ks, 1, QSAcc) of
                true ->
                    QSAcc;
                false ->
                    [{Ks, couch_util:to_list(V)} | QSAcc]
                end
            end,
            BaseQS, Params)]
    end ++
    case get_value(continuous, Options, false) of
    false ->
        [{"feed", "normal"}];
    true ->
        [{"feed", "continuous"}, {"heartbeat", "10000"}]
    end.

changes_json_req(_Db, "", _QueryParams) ->
    {[]};
changes_json_req(Db, FilterName, {QueryParams}) ->
    {ok, Info} = couch_db:get_db_info(Db),
    % simulate a request to db_name/_changes
    {[
        {<<"info">>, {Info}},
        {<<"id">>, null},
        {<<"method">>, 'GET'},
        {<<"path">>, [couch_db:name(Db), <<"_changes">>]},
        {<<"query">>, {[{<<"filter">>, FilterName} | QueryParams]}},
        {<<"headers">>, []},
        {<<"body">>, []},
        {<<"peer">>, <<"replicator">>},
        {<<"form">>, []},
        {<<"cookie">>, []},
        {<<"userCtx">>, couch_util:json_user_ctx(Db)}
    ]}.

options_to_query_args([], Acc) ->
    lists:reverse(Acc);
options_to_query_args([delay_commit | Rest], Acc) ->
    options_to_query_args(Rest, Acc);
options_to_query_args([{atts_since, []} | Rest], Acc) ->
    options_to_query_args(Rest, Acc);
options_to_query_args([{atts_since, PossibleAncestors} | Rest], Acc) ->
    % NOTE, we should limit the # of PossibleAncestors sent. Since a large
    % # can exceed the max URL length. Limiting the # only results in
    % attachments being fully copied from source to target, instead of
    % incrementally.
    AncestorsJson = ?JSON_ENCODE(couch_doc:revs_to_strs(PossibleAncestors)),
    options_to_query_args(Rest, [{"atts_since", AncestorsJson} | Acc]).


receive_docs(Streamer, UserFun, UserAcc) ->
    Streamer ! {get_headers, self()},
    receive
    {headers, Headers} ->    
        case get_value("content-type", Headers) of
        {"multipart/related", _} = ContentType ->
            case couch_doc:doc_from_multi_part_stream(ContentType, 
                fun() -> receive_doc_data(Streamer) end) of
            {ok, Doc} ->
                UserAcc2 = UserFun({ok, Doc}, UserAcc),
                receive_docs(Streamer, UserFun, UserAcc2)
            end;
        {"application/json", []} ->
            Doc = couch_doc:from_json_obj(
                    ?JSON_DECODE(receive_all(Streamer, []))),
            UserAcc2 = UserFun({ok, Doc}, UserAcc),
            receive_docs(Streamer, UserFun, UserAcc2);
        {"application/json", [{"error","true"}]} ->
            {ErrorProps} = ?JSON_DECODE(receive_all(Streamer, [])),
            Rev = get_value(<<"missing">>, ErrorProps),
            Result = {{not_found, missing}, couch_doc:parse_rev(Rev)},
            UserAcc2 = UserFun(Result, UserAcc),
            receive_docs(Streamer, UserFun, UserAcc2)
        end;
    done ->
        {ok, UserAcc}
    end.

receive_all(Streamer, Acc)->
    Streamer ! {next_bytes, self()},
    receive
    {body_bytes, Bytes} ->
        receive_all(Streamer, [Bytes | Acc]);
    body_done ->
        lists:reverse(Acc)
     end.


receive_doc_data(Streamer)->    
    Streamer ! {next_bytes, self()},
    receive
    {body_bytes, Bytes} ->
        {Bytes, fun() -> receive_doc_data(Streamer) end};
    body_done ->
        {<<>>, fun() -> receive_doc_data(Streamer) end}
    end.


mp_parse_mixed(eof) ->
    receive {get_headers, From} ->
        From ! done
    end,
    ok;
mp_parse_mixed({headers, H}) ->
    receive {get_headers, From} ->
        From ! {headers, H}
    end,
    fun(Next) ->
        mp_parse_mixed(Next)
    end;
mp_parse_mixed({body, Bytes}) ->
    receive {next_bytes, From} ->
        From ! {body_bytes, Bytes}
    end,
    fun (Next) ->
        mp_parse_mixed(Next)
    end;
mp_parse_mixed(body_end) ->
    receive {next_bytes, From} ->
        From ! body_done;
    {get_headers, From} ->
        self() ! {get_headers, From}
    end,
    fun (Next) ->
        mp_parse_mixed(Next)
    end.

changes_ev1(object_start, UserFun, UserAcc) ->
    fun(Ev) -> changes_ev2(Ev, UserFun, UserAcc) end.

changes_ev2({key, <<"results">>}, UserFun, UserAcc) ->
    fun(Ev) -> changes_ev3(Ev, UserFun, UserAcc) end;
changes_ev2(_, UserFun, UserAcc) ->
    fun(Ev) -> changes_ev2(Ev, UserFun, UserAcc) end.

changes_ev3(array_start, UserFun, UserAcc) ->
    fun(Ev)-> changes_ev_loop(Ev, UserFun, UserAcc) end.

changes_ev_loop(object_start, UserFun, UserAcc) ->
    fun(Ev) ->
        json_stream_parse:collect_object(Ev,
            fun(Obj) ->
                UserAcc2 = UserFun(json_to_doc_info(Obj), UserAcc),
                fun(Ev2) -> changes_ev_loop(Ev2, UserFun, UserAcc2) end
            end)
    end;
changes_ev_loop(array_end, _UserFun, _UserAcc) ->
    fun(_Ev) -> changes_ev_done() end.

changes_ev_done() ->
    fun(_Ev) -> changes_ev_done() end.

continuous_changes(DataFun, UserFun) ->
    {DataFun2, _, Rest} = json_stream_parse:events(
        DataFun,
        fun(Ev) -> parse_changes_line(Ev, UserFun) end),
    continuous_changes(fun() -> {Rest, DataFun2} end, UserFun).

parse_changes_line(object_start, UserFun) ->
    fun(Ev) ->
        json_stream_parse:collect_object(Ev,
            fun(Obj) -> UserFun(json_to_doc_info(Obj)) end)
    end.

json_to_doc_info({Props}) ->
    RevsInfo = lists:map(
        fun({Change}) ->
            Rev = couch_doc:parse_rev(get_value(<<"rev">>, Change)),
            Del = (true =:= get_value(<<"deleted">>, Change)),
            #rev_info{rev=Rev, deleted=Del}
        end, get_value(<<"changes">>, Props)),
    #doc_info{
        id = get_value(<<"id">>, Props),
        high_seq = get_value(<<"seq">>, Props),
        revs = RevsInfo
    }.


bulk_results_to_errors(Docs, {ok, Results}, interactive_edit) ->
    lists:reverse(lists:foldl(
        fun({_, {ok, _}}, Acc) ->
            Acc;
        ({#doc{id = Id}, Error}, Acc) ->
            {_, Error, _Reason} = couch_httpd:error_info(Error),
            [ {[{<<"id">>, Id}, {<<"error">>, Error}]} | Acc ]
        end,
        [], lists:zip(Docs, Results)));

bulk_results_to_errors(Docs, {ok, Results}, replicated_changes) ->
    bulk_results_to_errors(Docs, {aborted, Results}, interactive_edit);

bulk_results_to_errors(_Docs, {aborted, Results}, interactive_edit) ->
    lists:map(
        fun({{Id, _Rev}, Err}) ->
            {_, Error, _Reason} = couch_httpd:error_info(Err),
            {[{<<"id">>, Id}, {<<"error">>, Error}]}
        end,
        Results);

bulk_results_to_errors(_Docs, Results, remote) ->
    lists:reverse(lists:foldl(
        fun({Props}, Acc) ->
            case get_value(<<"error">>, Props, get_value(error, Props)) of
            undefined ->
                Acc;
            Error ->
                Id = get_value(<<"id">>, Props, get_value(id, Props)),
                [ {[{<<"id">>, Id}, {<<"error">>, Error}]} | Acc ]
            end
        end,
        [], Results)).
