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

-module(couch_api_wrap_httpc).

-include("couch_db.hrl").
-include("couch_api_wrap.hrl").
-include("../ibrowse/ibrowse.hrl").

-export([httpdb_setup/1]).
-export([send_req/3, strip_creds/1]).

-import(couch_util, [
    get_value/2,
    get_value/3
    ]).


httpdb_setup(#httpdb{} = Db) ->
    % TODO: setup Ibrowse proxy options
    {ok, Db}.


send_req(#httpdb{headers = BaseHeaders} = HttpDb, Params, Callback) ->
    Method = get_value(method, Params, get),
    Headers = get_value(headers, Params, []),
    Body = get_value(body, Params, []),
    IbrowseOptions = [
        {response_format, binary}, {inactivity_timeout, HttpDb#httpdb.timeout}
        | get_value(ibrowse_options, Params, [])
    ],
    Headers2 = oauth_header(HttpDb, Params) ++ BaseHeaders ++ Headers,
    Url = full_url(HttpDb, Params),
    #url{host = Host, port = Port} = ibrowse_lib:parse_url(Url),
    {ok, Worker} = ibrowse:spawn_link_worker_process(Host, Port),
    Response = ibrowse:send_req_direct(
            Worker, Url, Headers2, Method, Body, IbrowseOptions, infinity),
    process_response(Response, Worker, HttpDb, Params, Callback).


process_response({ibrowse_req_id, ReqId}, Worker, HttpDb, Params, Callback) ->
    process_stream_response(ReqId, Worker, HttpDb, Params, Callback);

process_response(Resp, Worker, HttpDb, Params, Callback) ->
    stop_worker(Worker),
    case Resp of
    {ok, Code, Headers, Body} ->
        case ?l2i(Code) of
        Ok when Ok =:= 200 ; Ok =:= 201 ; (Ok >= 400 andalso Ok < 500) ->
            EJson = case Body of
            <<>> ->
                null;
            Json ->
                ?JSON_DECODE(Json)
            end,
            Callback(Ok, Headers, EJson);
        R when R =:= 301 ; R =:= 302 ->
            do_redirect(Headers, HttpDb, Params, Callback);
        Error ->
            report_error(nil, HttpDb, Params, {code, Error})
        end;
    {error, Reason} ->
        report_error(nil, HttpDb, Params, {reason, Reason})
    end.


process_stream_response(ReqId, Worker, HttpDb, Params, Callback) ->
    receive
    {ibrowse_async_headers, ReqId, Code, Headers} ->
        case ?l2i(Code) of
        Ok when Ok =:= 200 ; Ok =:= 201 ; (Ok >= 400 andalso Ok < 500) ->
            StreamDataFun = fun() ->
                stream_data_self(HttpDb, Params, Worker, ReqId)
            end,
            Ret = Callback(Ok, Headers, StreamDataFun),
            stop_worker(Worker),
            Ret;
        R when R =:= 301 ; R =:= 302 ->
            stop_worker(Worker),
            do_redirect(Headers, HttpDb, Params, Callback);
        Error ->
            report_error(Worker, HttpDb, Params, {code, Error})
        end
    after HttpDb#httpdb.timeout ->
        report_error(Worker, HttpDb, Params, timeout)
    end.


stop_worker(nil) ->
    ok;
stop_worker(Worker) when is_pid(Worker) ->
    unlink(Worker),
    receive {'EXIT', Worker, _} -> ok after 0 -> ok end,
    catch ibrowse:stop_worker_process(Worker).


report_error(Worker, #httpdb{timeout = Timeout} = HttpDb, Params, timeout) ->
    report_error(Worker, HttpDb, Params, {timeout, Timeout});

report_error(Worker, HttpDb, Params, Error) ->
    Method = string:to_upper(atom_to_list(get_value(method, Params, get))),
    Url = strip_creds(full_url(HttpDb, Params)),
    do_report_error(Url, Method, Error),
    stop_worker(Worker),
    exit({http_request_failed, Method, Url, Error}).


do_report_error(FullUrl, Method, {reason, Reason}) ->
    ?LOG_ERROR("Replicator, request ~s to ~p failed due to error ~p",
        [Method, FullUrl, Reason]);

do_report_error(Url, Method, {code, Code}) ->
    ?LOG_ERROR("Replicator, request ~s to ~p failed. The received "
        "HTTP error code is ~p", [Method, Url, Code]);

do_report_error(Url, Method, {timeout, Timeout}) ->
    ?LOG_ERROR("Replicator, request ~s to ~p failed. Inactivity timeout "
        " (~p milliseconds).", [Method, Url, Timeout]).


stream_data_self(HttpDb, Params, Worker, ReqId) ->
    ibrowse:stream_next(ReqId),
    receive
    {ibrowse_async_response, ReqId, {error, Error}} ->
        report_error(Worker, HttpDb, Params, {reason, Error});
    {ibrowse_async_response, ReqId, Data} ->
        {Data, fun() -> stream_data_self(HttpDb, Params, Worker, ReqId) end};
    {ibrowse_async_response_end, ReqId} ->
        {<<>>, fun() -> stream_data_self(HttpDb, Params, Worker, ReqId) end}
    after HttpDb#httpdb.timeout ->
        report_error(Worker, HttpDb, Params, timeout)
    end.


full_url(#httpdb{url = BaseUrl}, Params) ->
    Path = get_value(path, Params, []),
    QueryArgs = get_value(qs, Params, []),
    BaseUrl ++ Path ++ query_args_to_string(QueryArgs, []).


strip_creds(Url) ->
    re:replace(Url,
        "http(s)?://([^:]+):[^@]+@(.*)$",
        "http\\1://\\2:*****@\\3",
        [{return, list}]).


query_args_to_string([], []) ->
    "";
query_args_to_string([], Acc) ->
    "?" ++ string:join(lists:reverse(Acc), "&");
query_args_to_string([{K, V} | Rest], Acc) ->
    query_args_to_string(Rest, [(K ++ "=" ++ V) | Acc]).


oauth_header(#httpdb{oauth = nil}, _ConnParams) ->
    [];
oauth_header(#httpdb{url = BaseUrl, oauth = OAuth}, ConnParams) ->
    Consumer = {
        OAuth#oauth.consumer_key,
        OAuth#oauth.consumer_secret,
        OAuth#oauth.signature_method
    },
    Method = case get_value(method, ConnParams, get) of
    get -> "GET";
    post -> "POST";
    put -> "PUT";
    head -> "HEAD"
    end,
    OAuthParams = oauth:signed_params(Method,
        BaseUrl ++ get_value(path, ConnParams, []),
        get_value(qs, ConnParams, []),
        Consumer, OAuth#oauth.token, OAuth#oauth.token_secret),
    [{"Authorization",
        "OAuth " ++ oauth_uri:params_to_header_string(OAuthParams)}].


do_redirect(RespHeaders, #httpdb{url = OrigUrl} = HttpDb, Params, Callback) ->
    RedirectUrl = redirect_url(RespHeaders, OrigUrl),
    {HttpDb2, Params2} = after_redirect(RedirectUrl, HttpDb, Params),
    send_req(HttpDb2, Params2, Callback).


redirect_url(RespHeaders, OrigUrl) ->
    MochiHeaders = mochiweb_headers:make(RespHeaders),
    RedUrl = mochiweb_headers:get_value("Location", MochiHeaders),
    #url{
        host = Base,
        port = Port,
        path = Path,  % includes query string
        protocol = Proto
    } = ibrowse_lib:parse_url(RedUrl),
    #url{
        username = User,
        password = Passwd
    } = ibrowse_lib:parse_url(OrigUrl),
    Creds = case is_list(User) andalso is_list(Passwd) of
    true ->
        User ++ ":" ++ Passwd ++ "@";
    false ->
        []
    end,
    atom_to_list(Proto) ++ "://" ++ Creds ++ Base ++ ":" ++
        integer_to_list(Port) ++ Path.

after_redirect(RedirectUrl, HttpDb, Params) ->
    Params2 = lists:keydelete(path, 1, lists:keydelete(qs, 1, Params)),
    {HttpDb#httpdb{url = RedirectUrl}, Params2}.
