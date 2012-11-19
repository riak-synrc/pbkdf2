#!/usr/bin/env escript
%% -*- erlang -*-

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

main(_) ->
    test_util:init_code_path(),
    etap:plan(5),

    test_invalid_cookie(),
    test_valid_cookie(),
    test_valid_cookie_with_colons_start(),
    test_valid_cookie_with_colons_mid(),
    test_valid_cookie_with_colons_end(),

    etap:end_tests().

test_invalid_cookie() ->
    Fun = fun() ->
        couch_httpd_auth:decode_auth_session(<<"asd">>)
    end,
    Expected = {bad_request, <<"Malformed AuthSession cookie. Please clear your cookies.">>},
    etap:throws_ok(Fun, Expected, "Should throw on invalid cookie").

test_cookie(Cookie, Expected, Message) ->
    Cookie64 = couch_util:encodeBase64Url(Cookie),
    Result = couch_httpd_auth:decode_auth_session(Cookie64),
    etap:is(Result, Expected, Message).

test_valid_cookie() ->
    test_cookie(<<"aaa:bbb:ccc">>, ["aaa", "bbb", "ccc"],
        "Should decode cookie session.").

test_valid_cookie_with_colons_start() ->
    test_cookie(<<"aaa:bbb::cc">>, ["aaa", "bbb", ":cc"],
        "Should decode cookie session start.").

test_valid_cookie_with_colons_mid() ->
    test_cookie(<<"aaa:bbb:c::c">>, ["aaa", "bbb", "c::c"],
        "Should decode cookie session mid.").

test_valid_cookie_with_colons_end() ->
    test_cookie(<<"aaa:bbb:cc:">>, ["aaa", "bbb", "cc:"],
        "Should decode cookie session end.").

