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

-module(couch_replicator_utils).

-export([parse_rep_doc/2]).

-include("couch_db.hrl").
-include("couch_api_wrap.hrl").
-include("../ibrowse/ibrowse.hrl").

-import(couch_util, [
    get_value/2,
    get_value/3
]).


parse_rep_doc({Props} = RepObj, UserCtx) ->
    ProxyParams = parse_proxy_params(get_value(<<"proxy">>, Props, <<>>)),
    Source = parse_rep_db(get_value(<<"source">>, Props), ProxyParams),
    Target = parse_rep_db(get_value(<<"target">>, Props), ProxyParams),
    Options = convert_options(Props),
    Rep = #rep{
        id = make_replication_id(Source, Target, UserCtx, Options),
        source = Source,
        target = Target,
        options = Options,
        user_ctx = UserCtx,
        doc = RepObj
    },
    {ok, Rep}.


make_replication_id(Source, Target, UserCtx, Options) ->
    %% funky algorithm to preserve backwards compatibility
    {ok, HostName} = inet:gethostname(),
    % Port = mochiweb_socket_server:get(couch_httpd, port),
    Src = get_rep_endpoint(UserCtx, Source),
    Tgt = get_rep_endpoint(UserCtx, Target),
    Base = [HostName, Src, Tgt] ++
        case get_value(filter, Options) of
        undefined ->
            case get_value(doc_ids, Options) of
            undefined ->
                [];
            DocIds ->
                [DocIds]
            end;
        Filter ->
            [filter_code(Filter, Source, UserCtx),
                get_value(query_params, Options, {[]})]
        end,
    Extension = maybe_append_options([continuous, create_target], Options),
    {couch_util:to_hex(couch_util:md5(term_to_binary(Base))), Extension}.


filter_code(Filter, Source, UserCtx) ->
    {match, [DDocName, FilterName]} =
        re:run(Filter, "(.*?)/(.*)", [{capture, [1, 2], binary}]),
    {ok, Db} = couch_api_wrap:db_open(Source, [{user_ctx, UserCtx}]),
    try
        {ok, #doc{body = Body}} =
            couch_api_wrap:open_doc(Db, <<"_design/", DDocName/binary>>, []),
        Code = couch_util:get_nested_json_value(
            Body, [<<"filters">>, FilterName]),
        re:replace(Code, "^\s*(.*?)\s*$", "\\1", [{return, binary}])
    after
        couch_api_wrap:db_close(Db)
    end.


maybe_append_options(Options, RepOptions) ->
    lists:foldl(fun(Option, Acc) ->
        Acc ++
        case get_value(Option, RepOptions, false) of
        true ->
            "+" ++ atom_to_list(Option);
        false ->
            ""
        end
    end, [], Options).


get_rep_endpoint(_UserCtx, #httpdb{url=Url, headers=Headers, oauth=OAuth}) ->
    case OAuth of
    nil ->
        {remote, Url, Headers};
    {OAuth} ->
        {remote, Url, Headers, OAuth}
    end;
get_rep_endpoint(UserCtx, <<DbName/binary>>) ->
    {local, DbName, UserCtx}.


parse_rep_db({Props}, ProxyParams) ->
    Url = maybe_add_trailing_slash(get_value(<<"url">>, Props)),
    {AuthProps} = get_value(<<"auth">>, Props, {[]}),
    {BinHeaders} = get_value(<<"headers">>, Props, {[]}),
    Headers = [{?b2l(K), ?b2l(V)} || {K, V} <- BinHeaders],
    OAuth = case get_value(<<"oauth">>, AuthProps) of
    undefined ->
        nil;
    {OauthProps} ->
        #oauth{
            consumer_key = ?b2l(get_value(<<"consumer_key">>, OauthProps)),
            token = ?b2l(get_value(<<"token">>, OauthProps)),
            token_secret = ?b2l(get_value(<<"token_secret">>, OauthProps)),
            consumer_secret = ?b2l(get_value(<<"consumer_secret">>, OauthProps)),
            signature_method =
                case get_value(<<"signature_method">>, OauthProps) of
                undefined ->        hmac_sha1;
                <<"PLAINTEXT">> ->  plaintext;
                <<"HMAC-SHA1">> ->  hmac_sha1;
                <<"RSA-SHA1">> ->   rsa_sha1
                end
        }
    end,
    #httpdb{
        url = Url,
        oauth = OAuth,
        headers = Headers,
        proxy_options = ProxyParams
    };
parse_rep_db(<<"http://", _/binary>> = Url, ProxyParams) ->
    parse_rep_db({[{<<"url">>, Url}]}, ProxyParams);
parse_rep_db(<<"https://", _/binary>> = Url, ProxyParams) ->
    parse_rep_db({[{<<"url">>, Url}]}, ProxyParams);
parse_rep_db(<<DbName/binary>>, _ProxyParams) ->
    DbName.


maybe_add_trailing_slash(Url) when is_binary(Url) ->
    maybe_add_trailing_slash(?b2l(Url));
maybe_add_trailing_slash(Url) ->
    case lists:last(Url) of
    $/ ->
        Url;
    _ ->
        Url ++ "/"
    end.


convert_options([])->
    [];
convert_options([{<<"cancel">>, V} | R]) ->
    [{cancel, V} | convert_options(R)];
convert_options([{<<"create_target">>, V} | R]) ->
    [{create_target, V} | convert_options(R)];
convert_options([{<<"continuous">>, V} | R]) ->
    [{continuous, V} | convert_options(R)];
convert_options([{<<"filter">>, V} | R]) ->
    [{filter, V} | convert_options(R)];
convert_options([{<<"query_params">>, V} | R]) ->
    [{query_params, V} | convert_options(R)];
convert_options([{<<"doc_ids">>, V} | R]) ->
    [{doc_ids, V} | convert_options(R)];
convert_options([_ | R]) -> % skip unknown option
    convert_options(R).


parse_proxy_params(ProxyUrl) when is_binary(ProxyUrl) ->
    parse_proxy_params(?b2l(ProxyUrl));
parse_proxy_params([]) ->
    [];
parse_proxy_params(ProxyUrl) ->
    #url{
        host = Host,
        port = Port,
        username = User,
        password = Passwd
    } = ibrowse_lib:parse_url(ProxyUrl),
    [{proxy_host, Host}, {proxy_port, Port}] ++
        case is_list(User) andalso is_list(Passwd) of
        false ->
            [];
        true ->
            [{proxy_user, User}, {proxy_password, Passwd}]
        end.

