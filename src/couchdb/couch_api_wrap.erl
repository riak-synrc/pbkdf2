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


-include("couch_db.hrl").
-include("couch_replicate.hrl").
-include("../ibrowse/ibrowse.hrl").

-export([
    db_open/2,
    get_db_info/1,
    open_doc/3,
    update_doc/3,
    ensure_full_commit/1,
    get_missing_revs/2,
    open_doc_revs/6,
    update_doc/4,
    changes_since/5
    ]).

db_open(#httpdb{}=Db, _Options) ->
    {ok, Db};
db_open(DbName, Options) ->
    couch_db:open(DbName,Options).

get_db_info(#httpdb{url=Url,oauth=OAuth,headers=Headers}) ->
    Headers2 = oauth_header(Url, [], get, OAuth) ++ Headers,
    case ibrowse:send_req(Url, Headers2, get, [], [ 
           {response_format,binary}
           ], infinity) of
    {ok, "200", _RespHeaders, Body} ->
        {Props} = ?JSON_DECODE(Body),
       {ok, [{couch_util:to_existing_atom(K), V} || {K,V} <- Props]}
    end;
get_db_info(Db) ->
    couch_db:get_db_info(Db).


open_doc(#httpdb{url=Url,oauth=OAuth,headers=Headers}, DocId, Options) ->
    Url2 = Url ++ couch_util:url_encode(DocId),
    QArgs = options_to_query_args(Options, []),
    Headers2 = oauth_header(Url2, QArgs, get, OAuth) ++ Headers,
    #url{host=Host,port=Port}=ibrowse_lib:parse_url(Url),
    {ok, Worker} = ibrowse:spawn_link_worker_process(Host,Port),
    try ibrowse:send_req_direct(Worker, Url2 ++ query_args_to_string(QArgs, []), 
            Headers2, get, [], [ 
            {response_format,binary}
            ], infinity) of
    {ok, "200", _RespHeaders, Body} ->
        {ok, couch_doc:from_json_obj(?JSON_DECODE(Body))};
    {ok, "404", _RespHeaders, _Body} ->
        {not_found, missing}
    after
        catch ibrowse:stop_worker_process(Worker)
    end;
open_doc(Db, DocId, Options) ->
    couch_db:open_doc(Db, DocId, Options).

update_doc(Db, Doc, Options) ->
    update_doc(Db,Doc,Options,interactive_edit).

ensure_full_commit(#httpdb{url=Url,oauth=OAuth,headers=Headers}) ->
    Headers2 = oauth_header(Url, [], post, OAuth) ++ Headers,
    #url{host=Host,port=Port}=ibrowse_lib:parse_url(Url),
    {ok, Worker} = ibrowse:spawn_link_worker_process(Host,Port),
    case ibrowse:send_req_direct(Worker, Url ++ "_ensure_full_commit", Headers2, post, [], [ 
           {response_format,binary}
           ], infinity) of
    {ok, "201", _RespHeaders, Body} ->
        catch ibrowse:stop_worker_process(Worker),
        {Props} = ?JSON_DECODE(Body),
       {ok, couch_util:get_value(<<"instance_start_time">>,Props)}
    end;
ensure_full_commit(Db) ->
    couch_db:ensure_full_commit(Db).

get_missing_revs(#httpdb{url=Url,oauth=OAuth,headers=Headers}, IdRevs) ->
    Json = [{Id, couch_doc:revs_to_strs(Revs)} || {Id, Revs} <- IdRevs],
    Headers2 = oauth_header(Url, [], post, OAuth) ++ Headers,
    #url{host=Host,port=Port}=ibrowse_lib:parse_url(Url),
    {ok, Worker} = ibrowse:spawn_link_worker_process(Host,Port),
    case ibrowse:send_req_direct(Worker, Url ++ "_revs_diff", Headers2, post,
            ?JSON_ENCODE({Json}), [ 
           {response_format,binary}
           ], infinity) of
    {ok, "200", _RespHeaders, Body} ->
        catch ibrowse:stop_worker_process(Worker),
        {JsonResults} = ?JSON_DECODE(Body),
        ConvertToNativeFun = fun({Id, {Result}}) ->
                {Id,
                couch_doc:parse_revs(couch_util:get_value(<<"missing">>,Result)),
                couch_doc:parse_revs(
                    couch_util:get_value(<<"possible_ancestors">>, Result, []))}
            end,
        {ok, lists:map(ConvertToNativeFun,JsonResults)}
    end;
get_missing_revs(Db, IdRevs) ->
    couch_db:get_missing_revs(Db, IdRevs).


options_to_query_args([], Acc) ->
    lists:reverse(Acc);
options_to_query_args([delay_commit|Rest], Acc) ->
    options_to_query_args(Rest, Acc);
options_to_query_args([{atts_since,[]}|Rest], Acc) ->
    options_to_query_args(Rest, Acc);
options_to_query_args([{atts_since,PossibleAncestors}|Rest], Acc) ->
    options_to_query_args(Rest, [{"atts_since",?JSON_ENCODE(
            couch_doc:revs_to_strs(PossibleAncestors))} | Acc]).

query_args_to_string([], []) ->
    "";
query_args_to_string([], Acc) ->
    "?" ++ string:join(lists:reverse(Acc), "&");
query_args_to_string([{K,V}|Rest], Acc) ->
    query_args_to_string(Rest, [(K ++ "=" ++ V) | Acc]).

open_doc_revs(#httpdb{url=Url,oauth=OAuth,headers=Headers}, Id, Revs, 
        Options, Fun, Acc) ->
    Self = self(),
    QArgs = [{"revs", "true"},{"open_revs", ?JSON_ENCODE(couch_doc:revs_to_strs(Revs))} | 
        options_to_query_args(Options, [])],
    IdEncoded =
    case Id of
    <<"_design/",RestId/binary>> ->
        "_design/" ++ couch_util:url_encode(RestId);
    _ ->
        couch_util:url_encode(Id)
    end,
    Headers2 = oauth_header(Url ++ IdEncoded, QArgs, get, OAuth) ++ [{"accept", "multipart/mixed"} | Headers],
    Streamer = spawn_link(fun()->
            FullUrl = Url ++ IdEncoded ++ query_args_to_string(QArgs, []),
            #url{host=Host,port=Port}=ibrowse_lib:parse_url(Url),
            {ok, Worker} = ibrowse:spawn_link_worker_process(Host,Port),
            {ibrowse_req_id, ReqId} = ibrowse:send_req_direct(Worker, FullUrl, Headers2, get, [], [
                {response_format,binary},
                {stream_to, {self(), once}}
                ], infinity),
            
            receive
            {ibrowse_async_headers, ReqId, "200", RespHeaders} ->
                CType = couch_util:get_value("Content-Type", RespHeaders),
                couch_httpd:parse_multipart_request(CType, 
                    fun() -> stream_data_self(ReqId) end,
                    fun(Ev) -> mp_parse_mixed(Ev) end)
            end,
            catch ibrowse:stop_worker_process(Worker),
            unlink(Self)
        end),
    receive_docs(Streamer, Fun, Acc);
open_doc_revs(Db, Id, Revs, Options, Fun, Acc) ->
    {ok, Results} = couch_db:open_doc_revs(Db, Id, Revs, Options),
    {ok, lists:foldl(Fun, Acc, Results)}.


receive_docs(Streamer, UserFun, UserAcc) ->
    Streamer ! {get_headers, self()},
    receive
    {headers, Headers} ->    
        case couch_util:get_value("content-type", Headers) of
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
            Rev = couch_util:get_value(<<"missing">>, ErrorProps),
            Result = {{not_founds, missing}, couch_doc:parse_rev(Rev)},
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

update_doc(#httpdb{url=Url,headers=Headers,oauth=OAuth},Doc,Options,Type) ->
    QArgs = if Type == replicated_changes ->
        [{"new_edits", "false"}]; true -> [] end ++ 
        options_to_query_args(Options, []),
    
    Boundary = couch_uuids:random(),
    JsonBytes = ?JSON_ENCODE(couch_doc:to_json_obj(Doc, [revs,attachments,follows|Options])),
    {ContentType, Len} = couch_doc:len_doc_to_multi_part_stream(Boundary,
            JsonBytes, Doc#doc.atts, false),
    Self = self(),
    Headers2 = case lists:member(delay_commit, Options) of 
            true -> [{"X-Couch-Full-Commit", "false"}];
            false ->  []
            end ++ [{"Content-Type", ?b2l(ContentType)}] ++ 
            oauth_header(Url, QArgs, put, OAuth) ++ Headers,
    Ref = make_ref(),
    % this streams the doc data to the ibrowse requester
    DocStreamer = spawn_link(fun() ->
                couch_doc:doc_to_multi_part_stream(Boundary,
                    JsonBytes, Doc#doc.atts,
                    fun(Data) ->
                        receive {get_data, Ref, Pid} ->
                            Pid ! {data, Ref, Data}
                        end
                    end,
                    false),
                unlink(Self)
            end),
    #url{host=Host,port=Port}=ibrowse_lib:parse_url(Url),
    {ok, Worker} = ibrowse:spawn_link_worker_process(Host,Port),
    case ibrowse:send_req_direct(Worker, Url ++ couch_util:url_encode(Doc#doc.id) ++ query_args_to_string(QArgs, []),
            [{"Content-Length",Len}|Headers2], put, 
            {fun(0) ->
                eof;
             (LenLeft) when LenLeft > 0 ->
                DocStreamer ! {get_data, Ref, self()},
                receive {data, Ref, Data} ->
                    {ok, Data, LenLeft - iolist_size(Data)}
                end
            end, Len}, [], infinity) of
    {ok, [$2,$0, _], _RespHeaders, Body} ->
        catch ibrowse:stop_worker_process(Worker),
        {Props} = ?JSON_DECODE(Body),
        {ok, couch_doc:parse_rev(couch_util:get_value(<<"rev">>, Props))}
    end;
update_doc(Db,Doc,Options,Type) ->
    couch_db:update_doc(Db,Doc,Options,Type).

changes_since(#httpdb{url=Url,headers=Headers,oauth=OAuth}, Style,
        StartSeq, UserFun, Acc) ->
    Url2 = Url ++ "_changes",
    QArgs = [{"style", atom_to_list(Style)},
            {"since", integer_to_list(StartSeq)}],
    Headers2 = oauth_header(Url2, QArgs, get, OAuth) ++ Headers,        
    #url{host=Host,port=Port}=ibrowse_lib:parse_url(Url),
    {ok, Worker} = ibrowse:spawn_link_worker_process(Host,Port),
    {ibrowse_req_id, ReqId} = ibrowse:send_req_direct(Worker, Url2 ++ query_args_to_string(QArgs, ""), 
            Headers2, get, [], [
            {response_format,binary},
            {stream_to, {self(), once}}], infinity),
    DataFun = fun() ->
            receive {ibrowse_async_headers, ReqId, "200", _Headers} ->
                stream_data_self(ReqId)
            end
        end,
    EventFun = fun(Ev) ->
            changes_ev1(Ev, UserFun, Acc)
        end,
    try
        json_stream_parse:events(DataFun, EventFun)
    after
        catch ibrowse:stop_worker_process(Worker)
    end;
changes_since(Db, Style, StartSeq, UserFun, Acc) ->
    couch_db:changes_since(Db, Style, StartSeq, UserFun, Acc).

stream_data_self(ReqId) ->
    ibrowse:stream_next(ReqId),
    receive {ibrowse_async_response, ReqId, Data} ->
        {Data, fun() -> stream_data_self(ReqId) end};
    {ibrowse_async_response_end, ReqId} ->
        {<<>>, fun() -> stream_data_self(ReqId) end}
    end.

changes_ev1(object_start, UserFun, UserAcc) ->
    fun(Ev) -> changes_ev2(Ev, UserFun, UserAcc) end.

changes_ev2({key, <<"results">>}, UserFun, UserAcc) ->
    fun(Ev)-> changes_ev3(Ev, UserFun, UserAcc) end;
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

json_to_doc_info({Props}) ->
    Id = couch_util:get_value(<<"id">>, Props),
    Seq = couch_util:get_value(<<"seq">>, Props),
    Changes = couch_util:get_value(<<"changes">>, Props),
    
    RevsInfo = lists:map(
        fun({Change}) ->
            Rev = couch_doc:parse_rev(couch_util:get_value(<<"rev">>, Change)),
            Del = ("true" == couch_util:get_value(<<"deleted">>, Change)),
            #rev_info{rev=Rev,deleted=Del}
        end, Changes),
    #doc_info{id=Id,high_seq=Seq,revs=RevsInfo}.

oauth_header(_Url, _QS, _Action, nil) ->
    [];
oauth_header(Url, QS, Action, OAuth) ->
    Consumer =
            {OAuth#oauth.consumer_key,
            OAuth#oauth.consumer_secret,
            OAuth#oauth.signature_method},
    Method = case Action of
        get -> "GET";
        post -> "POST";
        put -> "PUT";
        head -> "HEAD"
    end,
    Params = oauth:signed_params(Method, Url, QS, Consumer, 
        #oauth.token,
        #oauth.token_secret),
    [{"Authorization", "OAuth " ++ oauth_uri:params_to_header_string(Params)}].


    