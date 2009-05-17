% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_file).
-behaviour(gen_server).

-include("couch_db.hrl").

-define(SIZE_BLOCK, 4096).

-export([open/1, open/2, close/1, bytes/1, sync/1, append_binary/2]).
-export([append_term/2, pread_term/2, pread_iolist/2, write_header/2]).
-export([pread_binary/2, read_header/1, truncate/2]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, code_change/3, handle_info/2]).

%%----------------------------------------------------------------------
%% Args:   Valid Options are [create] and [create,overwrite].
%%  Files are opened in read/write mode.
%% Returns: On success, {ok, Fd}
%%  or {error, Reason} if the file could not be opened.
%%----------------------------------------------------------------------

open(Filepath) ->
    open(Filepath, []).
    
open(Filepath, Options) ->
    case gen_server:start_link(couch_file,
            {Filepath, Options, self(), Ref = make_ref()}, []) of
    {ok, Fd} ->
        {ok, Fd};
    ignore ->
        % get the error
        receive
        {Ref, Pid, Error} ->
            case process_info(self(), trap_exit) of
            {trap_exit, true} -> receive {'EXIT', Pid, _} -> ok end;
            {trap_exit, false} -> ok
            end,
            Error
        end;
    Error ->
        Error
    end.


%%----------------------------------------------------------------------
%% Purpose: To append an Erlang term to the end of the file.
%% Args:    Erlang term to serialize and append to the file.
%% Returns: {ok, Pos} where Pos is the file offset to the beginning the
%%  serialized  term. Use pread_term to read the term back.
%%  or {error, Reason}.
%%----------------------------------------------------------------------

append_term(Fd, Term) ->
    append_binary(Fd, term_to_binary(Term, [compressed])).


%%----------------------------------------------------------------------
%% Purpose: To append an Erlang binary to the end of the file.
%% Args:    Erlang term to serialize and append to the file.
%% Returns: {ok, Pos} where Pos is the file offset to the beginning the
%%  serialized  term. Use pread_term to read the term back.
%%  or {error, Reason}.
%%----------------------------------------------------------------------
    
append_binary(Fd, Bin) ->
    Size = iolist_size(Bin),
    SizePrependedBin = iolist_to_binary([<<Size:32/integer>>, Bin]),
    gen_server:call(Fd, {append_bin, SizePrependedBin}, infinity).


%%----------------------------------------------------------------------
%% Purpose: Reads a term from a file that was written with append_term
%% Args:    Pos, the offset into the file where the term is serialized.
%% Returns: {ok, Term}
%%  or {error, Reason}.
%%----------------------------------------------------------------------

pread_term(Fd, Pos) ->
    {ok, Bin} = pread_binary(Fd, Pos),
    {ok, binary_to_term(Bin)}.


%%----------------------------------------------------------------------
%% Purpose: Reads a binrary from a file that was written with append_binary
%% Args:    Pos, the offset into the file where the term is serialized.
%% Returns: {ok, Term}
%%  or {error, Reason}.
%%----------------------------------------------------------------------

pread_binary(Fd, Pos) ->
    {ok, L} = pread_iolist(Fd, Pos),
    {ok, iolist_to_binary(L)}.

pread_iolist(Fd, Pos) ->
    {ok, LenIolist, NextPos} =read_raw_iolist(Fd, Pos, 4),
    <<Len:32/integer>> = iolist_to_binary(LenIolist),
    {ok, Iolist, _} = read_raw_iolist(Fd, NextPos, Len),
    {ok, Iolist}.

read_raw_iolist(Fd, Pos, Len) when (Pos rem ?SIZE_BLOCK) == 0 ->
    read_raw_iolist(Fd, Pos + 1, Len);
read_raw_iolist(Fd, Pos, Len) ->
    BlockOffset = Pos rem ?SIZE_BLOCK,
    TotalBytes = calculate_total_read_len(BlockOffset, Len),
    {ok, <<RawBin:TotalBytes/binary>>} = gen_server:call(Fd, {pread, Pos, TotalBytes}, infinity),
    {ok, remove_block_prefixes(BlockOffset, RawBin), Pos + TotalBytes}.

%%----------------------------------------------------------------------
%% Purpose: The length of a file, in bytes.
%% Returns: {ok, Bytes}
%%  or {error, Reason}.
%%----------------------------------------------------------------------

% length in bytes
bytes(Fd) ->
    gen_server:call(Fd, bytes, infinity).

%%----------------------------------------------------------------------
%% Purpose: Truncate a file to the number of bytes.
%% Returns: ok
%%  or {error, Reason}.
%%----------------------------------------------------------------------

truncate(Fd, Pos) ->
    gen_server:call(Fd, {truncate, Pos}, infinity).

%%----------------------------------------------------------------------
%% Purpose: Ensure all bytes written to the file are flushed to disk.
%% Returns: ok
%%  or {error, Reason}.
%%----------------------------------------------------------------------

sync(Fd) ->
    gen_server:call(Fd, sync, infinity).

%%----------------------------------------------------------------------
%% Purpose: Close the file. Is performed asynchronously.
%% Returns: ok
%%----------------------------------------------------------------------
close(Fd) ->
    Result = gen_server:cast(Fd, close),
    catch unlink(Fd),
    Result.

read_header(Fd) ->
    case gen_server:call(Fd, find_header, infinity) of
    {ok, Bin} ->
        {ok, binary_to_term(Bin)};
    Else ->
        Else
    end.
    
write_header(Fd, Data) ->
    Bin = term_to_binary(Data),
    Md5 = erlang:md5(Bin),
    % now we assemble the final header binary and write to disk
    FinalBin = <<Md5/binary, Bin/binary>>,
    gen_server:call(Fd, {write_header, FinalBin}, infinity).
    



init_status_error(ReturnPid, Ref, Error) ->
    ReturnPid ! {Ref, self(), Error},
    ignore.

% server functions

init({Filepath, Options, ReturnPid, Ref}) ->
    case lists:member(create, Options) of
    true ->
        filelib:ensure_dir(Filepath),
        case file:open(Filepath, [read, write, raw, binary]) of
        {ok, Fd} ->
            {ok, Length} = file:position(Fd, eof),
            case Length > 0 of
            true ->
                % this means the file already exists and has data.
                % FYI: We don't differentiate between empty files and non-existant
                % files here.
                case lists:member(overwrite, Options) of
                true ->
                    {ok, 0} = file:position(Fd, 0),
                    ok = file:truncate(Fd),
                    couch_stats_collector:track_process_count(
                            {couchdb, open_os_files}),
                    {ok, Fd};
                false ->
                    ok = file:close(Fd),
                    init_status_error(ReturnPid, Ref, file_exists)
                end;
            false ->
                couch_stats_collector:track_process_count(
                        {couchdb, open_os_files}),
                {ok, Fd}
            end;
        Error ->
            init_status_error(ReturnPid, Ref, Error)
        end;
    false ->
        % open in read mode first, so we don't create the file if it doesn't exist.
        case file:open(Filepath, [read, raw]) of
        {ok, Fd_Read} ->
            {ok, Fd} = file:open(Filepath, [read, write, raw, binary]),
            ok = file:close(Fd_Read),
            couch_stats_collector:track_process_count({couchdb, open_os_files}),
            {ok, Fd};
        Error ->
            init_status_error(ReturnPid, Ref, Error)
        end
    end.


terminate(_Reason, _Fd) ->
    ok.


handle_call({pread, Pos, Bytes}, _From, Fd) ->
    {reply, file:pread(Fd, Pos, Bytes), Fd};
handle_call(bytes, _From, Fd) ->
    {reply, file:position(Fd, eof), Fd};
handle_call(sync, _From, Fd) ->
    {reply, file:sync(Fd), Fd};
handle_call({truncate, Pos}, _From, Fd) ->
    {ok, Pos} = file:position(Fd, Pos),
    {reply, file:truncate(Fd), Fd};
handle_call({append_bin, Bin}, _From, Fd) ->
    {ok, Pos} = file:position(Fd, eof),
    Blocks = make_blocks(Pos rem ?SIZE_BLOCK, Bin),
    case file:pwrite(Fd, Pos, Blocks) of
    ok ->
        {reply, {ok, Pos}, Fd};
    Error ->
        {reply, Error, Fd}
    end;
handle_call({write_header, Bin}, _From, Fd) ->
    {ok, Pos} = file:position(Fd, eof),
    BinSize = size(Bin),
    case Pos rem ?SIZE_BLOCK of
    0 ->
        io:format("Writing header at block:~p~n", [(Pos div ?SIZE_BLOCK)]),
        Padding = <<>>;
    BlockOffset ->
        io:format("Writing header at block:~p~n", [(Pos div ?SIZE_BLOCK) + 1]),
        Padding = <<0:(8*(?SIZE_BLOCK-BlockOffset))>>
    end,
    FinalBin = [Padding, <<1, BinSize:32/integer>> | make_blocks(1, Bin)],
    {reply, file:pwrite(Fd, Pos, FinalBin), Fd};
handle_call(find_header, _From, Fd) ->
    {ok, Pos} = file:position(Fd, eof),
    {reply, find_header(Fd, Pos div ?SIZE_BLOCK), Fd}.
    
    
    
handle_cast(close, Fd) ->
    {stop,normal,Fd}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info({'EXIT', _, Reason}, Fd) ->
    {stop, Reason, Fd}.


find_header(_Fd, -1) ->
    no_valid_header;
find_header(Fd, Block) ->
    case (catch load_header(Fd, Block)) of
    {ok, Bin} ->
        io:format("Found header at block:~p~n", [Block]),
        {ok, Bin};
    _Error ->
        find_header(Fd, Block -1)
    end.
    
load_header(Fd, Block) ->
    {ok, <<1>>} = file:pread(Fd, Block*?SIZE_BLOCK, 1),
    {ok, <<HeaderLen:32/integer>>} = file:pread(Fd, (Block*?SIZE_BLOCK) + 1, 4),
    TotalBytes = calculate_total_read_len(1, HeaderLen),
    {ok, <<RawBin:TotalBytes/binary>>} = 
            file:pread(Fd, (Block*?SIZE_BLOCK) + 5, TotalBytes),
    io:format("Foo:~p~n", [RawBin]),
    <<Md5Sig:16/binary, HeaderBin/binary>> = 
        iolist_to_binary(remove_block_prefixes(1, RawBin)),
    Md5Sig = erlang:md5(HeaderBin),
    {ok, HeaderBin}.


calculate_total_read_len(BlockOffset, FinalLen) ->
    case ?SIZE_BLOCK - BlockOffset of
    BlockLeft when BlockLeft >= FinalLen ->
        FinalLen;
    BlockLeft ->
        FinalLen + ((FinalLen - BlockLeft) div (?SIZE_BLOCK -1)) +
            if ((FinalLen - BlockLeft) rem (?SIZE_BLOCK -1)) == 0 -> 0;
                true -> 1 end
    end.

remove_block_prefixes(_BlockOffset, <<>>) ->
    [];
remove_block_prefixes(0, <<_BlockPrefix,Rest/binary>>) ->
    remove_block_prefixes(1, Rest);
remove_block_prefixes(BlockOffset, Bin) ->
    BlockBytesAvailable = ?SIZE_BLOCK - BlockOffset,
    case size(Bin) of
    Size when Size > BlockBytesAvailable ->
        <<DataBlock:BlockBytesAvailable/binary,Rest/binary>> = Bin,
        [DataBlock | remove_block_prefixes(0, Rest)];
    _Size ->
        [Bin]
    end.

make_blocks(_BlockOffset, <<>>) ->
    [];
make_blocks(0, Bin) ->
    [<<0>> | make_blocks(1, Bin)];
make_blocks(BlockOffset, Bin) when size(Bin) =< (?SIZE_BLOCK - BlockOffset) ->
    [Bin];
make_blocks(BlockOffset, Bin) ->
    BlockBytes = (?SIZE_BLOCK - BlockOffset),
    <<BlockBin:BlockBytes/binary, Rest/binary>> = Bin,
    [BlockBin | make_blocks(0, Rest)].

