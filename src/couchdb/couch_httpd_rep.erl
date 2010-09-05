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
    send_json/2,
    send_json/3,
    send_method_not_allowed/2
]).
    
-export([handle_req/1]).


handle_req(#httpd{method = 'POST', user_ctx = UserCtx} = Req) ->
    RepDoc = couch_httpd:json_body_obj(Req),
    {ok, Rep} = couch_replicator_utils:parse_rep_doc(RepDoc, UserCtx),
    case couch_replicate:replicate(Rep) of
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
    end;

handle_req(Req) ->
    send_method_not_allowed(Req, "POST").
