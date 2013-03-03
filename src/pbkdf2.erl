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

-module(pbkdf2).

-export([pbkdf2/4, pbkdf2/5, compare_secure/2, to_hex/1]).


%-type(hex_char() :: $0 .. $9 | $a .. $f).
-type(hex_char() :: 48 .. 57 | 97 .. 102).
-type(hex_list() :: [hex_char()]).

-type digest_func() :: fun((Data :: binary()) -> Hash :: binary()).

%-type mac_func() :: fun((Key :: binary(), Data :: binary(), MacLength :: integer()) -> Mac :: binary()).
-type mac_func() :: fun((Key :: binary(), Data :: binary()) -> Mac :: binary()).

-type(digest_func_info() ::
	digest_func() | {digest_func(), DigestLength :: integer()}
	| md4 | md5 | ripemd160 | sha | sha224 | sha256 | sha384 | sha512
).

-type(mac_func_info() ::
	mac_func() | {hmac, digest_func_info()}
	| md4 | md5 | ripemd160 | sha | sha224 | sha256 | sha384 | sha512
).


-define(MAX_DERIVED_KEY_LENGTH, (1 bsl 32 - 1)).

%======================================================================================================================
% Public API

-spec pbkdf2(MacFunc, Password, Salt, Iterations) -> {ok, Key} | {error, derived_key_too_long} when
	MacFunc :: mac_func_info(),
	Password :: binary(),
	Salt :: binary(),
	Iterations :: integer(),
	Key :: binary().

pbkdf2(MacFunc, Password, Salt, Iterations) ->
	MacFunc1 = resolve_mac_func(MacFunc),
	DerivedLength = byte_size(MacFunc1(<<"test key">>, <<"test data">>)),
    pbkdf2(MacFunc1, Password, Salt, Iterations, DerivedLength, 1, []).

%----------------------------------------------------------------------------------------------------------------------

-spec pbkdf2(MacFunc, Password, Salt, Iterations, DerivedLength) -> {ok, Key} | {error, derived_key_too_long} when
	MacFunc :: mac_func_info(),
	Password :: binary(),
	Salt :: binary(),
	Iterations :: integer(),
	DerivedLength :: integer(),
	Key :: binary().

pbkdf2(_MacFunc, _Password, _Salt, _Iterations, DerivedLength) when DerivedLength > ?MAX_DERIVED_KEY_LENGTH ->
    {error, derived_key_too_long};

pbkdf2(MacFunc, Password, Salt, Iterations, DerivedLength) ->
	MacFunc1 = resolve_mac_func(MacFunc),
    Bin = pbkdf2(MacFunc1, Password, Salt, Iterations, DerivedLength, 1, []),
    {ok, Bin}.

%======================================================================================================================
% Internal Functions

-spec pbkdf2(MacFunc, Password, Salt, Iterations, DerivedLength, BlockIndex, Acc) -> Key when
	MacFunc :: mac_func_info(),
	Password :: binary(),
	Salt :: binary(),
	Iterations :: integer(),
	DerivedLength :: integer(),
	BlockIndex :: integer(),
	Acc :: iolist(),
	Key :: binary().

pbkdf2(_MacFunc, _Password, _Salt, _Iterations, DerivedLength, _BlockIndex, Acc) when length(Acc) > DerivedLength ->
	<<Bin:DerivedLength/binary, _/binary>> = iolist_to_binary(lists:reverse(Acc)),
	Bin;

pbkdf2(MacFunc, Password, Salt, Iterations, DerivedLength, BlockIndex, Acc) ->
    Block = pbkdf2(MacFunc, Password, Salt, Iterations, BlockIndex, 1, <<>>, <<>>),
    pbkdf2(MacFunc, Password, Salt, Iterations, DerivedLength, BlockIndex + 1, [Block | Acc]).

%----------------------------------------------------------------------------------------------------------------------

-spec pbkdf2(MacFunc, Password, Salt, Iterations, BlockIndex, Iteration, Prev, Acc) -> Key when
	MacFunc :: mac_func_info(),
	Password :: binary(),
	Salt :: binary(),
	Iterations :: integer(),
	BlockIndex :: integer(),
	Iteration :: integer(),
	Prev :: binary(),
	Acc :: binary(),
	Key :: binary().

pbkdf2(_MacFunc, _Password, _Salt, Iterations, _BlockIndex, Iteration, _Prev, Acc) when Iteration > Iterations ->
    Acc;

pbkdf2(MacFunc, Password, Salt, Iterations, BlockIndex, 1, _Prev, _Acc) ->
    InitialBlock = MacFunc(Password, <<Salt/binary, BlockIndex:32/integer>>),
    pbkdf2(MacFunc, Password, Salt, Iterations, BlockIndex, 2, InitialBlock, InitialBlock);

pbkdf2(MacFunc, Password, Salt, Iterations, BlockIndex, Iteration, Prev, Acc) ->
    Next = MacFunc(Password, Prev),
    pbkdf2(MacFunc, Password, Salt, Iterations, BlockIndex, Iteration + 1, Next, crypto:exor(Next, Acc)).

%----------------------------------------------------------------------------------------------------------------------

%-type digest_func_info() :: digest_func() | {digest_func(), DigestLength}
%	| md4 | md5 | ripemd160 | sha | sha224 | sha256 | sha384 | sha512.

resolve_digest_func({DigestFunc, DigestLength}) when is_function(DigestFunc) ->
	{DigestFunc, DigestLength};

resolve_digest_func(md4) -> {crypto, md4};
resolve_digest_func(md5) -> {crypto, md5};
resolve_digest_func(ripemd160) -> {crypto, ripemd160};
resolve_digest_func(sha) -> {crypto, sha};
resolve_digest_func(sha224) -> {crypto, sha224};
resolve_digest_func(sha256) -> {crypto, sha256};
resolve_digest_func(sha384) -> {crypto, sha384};
resolve_digest_func(sha512) -> {crypto, sha512}.

%----------------------------------------------------------------------------------------------------------------------

%-type mac_func_info() :: mac_func() | {hmac, digest_func_info()}
%	| md4 | md5 | ripemd160 | sha | sha224 | sha256 | sha384 | sha512.

resolve_mac_func({hmac, DigestFunc}) ->
	DigestFunc1 = resolve_digest_func(DigestFunc),
	fun(Key, Data) ->
		crypto:hmac(DigestFunc1, Key, Data)
	end;

%	fun(Key, Data, MacLength) ->
%		crypto:hmac(DigestFunc1, Key, Data, MacLength)
%	end;

resolve_mac_func(MacFunc) when is_function(MacFunc) ->
	MacFunc;

resolve_mac_func(md4) -> resolve_mac_func({hmac, md4});
resolve_mac_func(md5) -> resolve_mac_func({hmac, md5});
resolve_mac_func(ripemd160) -> resolve_mac_func({hmac, ripemd160});
resolve_mac_func(sha) -> resolve_mac_func({hmac, sha});
resolve_mac_func(sha224) -> resolve_mac_func({hmac, sha224});
resolve_mac_func(sha256) -> resolve_mac_func({hmac, sha256});
resolve_mac_func(sha384) -> resolve_mac_func({hmac, sha384});
resolve_mac_func(sha512) -> resolve_mac_func({hmac, sha512}).

%----------------------------------------------------------------------------------------------------------------------

%% Compare two strings or binaries for equality without short-circuits to avoid timing attacks.

-spec compare_secure(First, Second) -> boolean() when
	First :: binary() | string(),
	Second :: binary() | string().

compare_secure(<<X/binary>>, <<Y/binary>>) ->
    compare_secure(binary_to_list(X), binary_to_list(Y));

compare_secure(X, Y) when is_list(X) and is_list(Y) ->
    case length(X) == length(Y) of
        true ->
            compare_secure(X, Y, 0);
        false ->
            false
    end;

compare_secure(_X, _Y) -> false.

-spec compare_secure(First, Second, Accum) -> boolean() when
	First :: string(),
	Second :: string(),
	Accum :: integer().

compare_secure([X|RestX], [Y|RestY], Result) ->
    compare_secure(RestX, RestY, (X bxor Y) bor Result);

compare_secure([], [], Result) ->
    Result == 0.

%----------------------------------------------------------------------------------------------------------------------

-spec to_hex(Data) -> HexData when
	Data :: binary() | list(),
	HexData :: hex_list().

to_hex([]) ->
    [];

to_hex(Bin) when is_binary(Bin) ->
    to_hex(binary_to_list(Bin));

to_hex([H|T]) ->
    [to_hex_digit(H div 16), to_hex_digit(H rem 16) | to_hex(T)].

%----------------------------------------------------------------------------------------------------------------------

-spec to_hex_digit(Nyble) -> hex_char() when
	Nyble :: 0 .. 15.

to_hex_digit(N) when N < 10 ->
	$0 + N;

to_hex_digit(N) ->
	$a + N - 10.
