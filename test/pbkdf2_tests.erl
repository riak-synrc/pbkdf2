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

-module(pbkdf2_tests).

-include_lib("eunit/include/eunit.hrl").


-define(TEST_VECTORS, [
	{[sha, <<"password">>, <<"salt">>, 1, 20],
			<<"0c60c80f961f0e71f3a9b524af6012062fe037a6">>},
	{[sha, <<"password">>, <<"salt">>, 2, 20],
			<<"ea6c014dc72d6f8ccd1ed92ace1d41f0d8de8957">>},
	{[sha, <<"password">>, <<"salt">>, 4096, 20],
			<<"4b007901b765489abead49d926f721d065a429c1">>},
	{[sha, <<"passwordPASSWORDpassword">>, <<"saltSALTsaltSALTsaltSALTsaltSALTsalt">>, 4096, 25],
			<<"3d2eec4fe41c849b80c8d83662c0e44a8b291a964cf2f07038">>},
	{[sha, <<"pass\0word">>, <<"sa\0lt">>, 4096, 16],
			<<"56fa6aa75548099dcc37d7f03425e0c3">>},
	{[sha, <<"password">>, <<"salt">>, 16777216, 20],
			<<"eefe3d61cd4da4e4e9945b3d6ba2158c2634e984">>}
]).


pbkdf2_hex(Args) ->
	io:format("Running test with Args = ~p~n", [Args]),
	{ok, Key} = apply(pbkdf2, pbkdf2, Args),
	pbkdf2:to_hex(Key).


correctness_test_() ->
	[
		{timeout, 60,
			?_assertEqual(
				Expected,
				pbkdf2_hex(Args)
				)
			}
		|| {Args, Expected} <- ?TEST_VECTORS
		].
