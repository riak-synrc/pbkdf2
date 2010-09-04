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

-module(couch_httpd_rep).

-include("couch_db.hrl").
-include("couch_api_wrap.hrl").

-import(couch_httpd, [
    send_json/2, send_json/3, send_json/4,
    send/2,
    send_method_not_allowed/2,
    start_response_length/4,
    start_json_response/2, start_json_response/3, end_json_response/1,
    start_chunked_response/3, send_chunk/2, last_chunk/1,
    absolute_uri/2
]).
    
-export([handle_req/1]).


handle_req(#httpd{method='POST'} = Req) ->
    {PostBody} = couch_httpd:json_body_obj(Req),
    SrcDb = parse_rep_db(couch_util:get_value(<<"source">>, PostBody)),
    TgtDb = parse_rep_db(couch_util:get_value(<<"target">>, PostBody)),
    Options = convert_options(PostBody),
    try couch_replicate:replicate(SrcDb, TgtDb, Options, Req#httpd.user_ctx) of
    {error, Reason} ->
        try
            send_json(Req, 500, {[{error, Reason}]})
        catch
        exit:{json_encode, _} ->
            send_json(Req, 500, {[{error, couch_util:to_binary(Reason)}]})
        end;
    {ok, {cancelled, RepId}} ->
        send_json(Req, 200, {[{ok, true}, {<<"_local_id">>, RepId}]});
    {ok, {continuous, RepId}} ->
        send_json(Req, 200, {[{ok, true}, {<<"_local_id">>, RepId}]});
    {ok, {HistoryResults}} ->
        send_json(Req, {[{ok, true} | HistoryResults]})
    catch
    throw:{db_not_found, Msg} ->
        send_json(Req, 404, {[{error, db_not_found}, {reason, Msg}]})
    end;
handle_req(Req) ->
    send_method_not_allowed(Req, "POST").


maybe_add_trailing_slash(Url) when is_binary(Url) ->
    maybe_add_trailing_slash(?b2l(Url));
maybe_add_trailing_slash(Url) ->
    case lists:last(Url) of
    $/ ->
        Url;
    _ ->
        Url ++ "/"
    end.

parse_rep_db({Props}) ->
    Url = maybe_add_trailing_slash(couch_util:get_value(<<"url">>, Props)),
    {AuthProps} = couch_util:get_value(<<"auth">>, Props, {[]}),
    {BinHeaders} = couch_util:get_value(<<"headers">>, Props, {[]}),
    Headers = [{?b2l(K), ?b2l(V)} || {K, V} <- BinHeaders],
    
    case couch_util:get_value(<<"oauth">>, AuthProps) of
    undefined ->
        OAuth = nil;
    {OauthProps} -> 
        OAuth = #oauth{
            consumer_key = 
                ?b2l(couch_util:get_value(<<"consumer_key">>, OauthProps)),
            token = 
                ?b2l(couch_util:get_value(<<"token">>, OauthProps)),
            token_secret = 
                ?b2l(couch_util:get_value(<<"token_secret">>, OauthProps)),
            consumer_secret = 
                ?b2l(couch_util:get_value(<<"consumer_secret">>, OauthProps)),
            signature_method = 
                case couch_util:get_value(<<"signature_method">>, OauthProps) of
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
        headers = Headers
    };
parse_rep_db(<<"http://", _/binary>> = Url) ->
    parse_rep_db({[{<<"url">>, Url}]});
parse_rep_db(<<"https://", _/binary>> = Url) ->
    parse_rep_db({[{<<"url">>, Url}]});
parse_rep_db(<<DbName/binary>>) ->
    DbName.


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


