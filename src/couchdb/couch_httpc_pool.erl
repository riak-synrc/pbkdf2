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

-module(couch_httpc_pool).
-behaviour(gen_server).

% public API
-export([start_link/2, stop/1]).
-export([get_worker/1]).
-export([release_worker/2]).

% gen_server API
-export([init/1, handle_call/3, handle_info/2, handle_cast/2]).
-export([code_change/3, terminate/2]).

-include("couch_db.hrl").

-record(state, {
    host,
    port,
    free = [],
    busy = [],
    shutdown = false
}).


start_link(Host, Port) ->
    gen_server:start_link(?MODULE, {Host, Port}, []).


stop(Pool) ->
    ok = gen_server:call(Pool, stop, infinity).


get_worker(Pool) ->
    gen_server:call(Pool, get_worker, infinity).


release_worker(Pool, Worker) ->
    ok = gen_server:call(Pool, {release_worker, Worker}, infinity).


init({Host, Port}) ->
    process_flag(trap_exit, true),
    {ok, #state{host = Host, port = Port}}.


handle_call(get_worker, _From, #state{shutdown = true} = State) ->
    {reply, {error, shutting_down}, State};

handle_call(get_worker, _From,
    #state{free = [], busy = Busy, host = Host, port = Port} = State) ->
    {ok, Worker} = ibrowse_http_client:start_link({Host, Port}),
    {reply, {ok, Worker}, State#state{busy = [Worker | Busy]}};

handle_call(get_worker, _From,
    #state{free = [Worker | RestFree], busy = Busy} = State) ->
    {reply, {ok, Worker}, State#state{free = RestFree, busy = [Worker | Busy]}};

handle_call({release_worker, Worker}, _From,
    #state{free = Free, busy = Busy, shutdown = Shutdown} = State) ->
    case Busy -- [Worker] of
    Busy ->
        {reply, ok, State};
    [] when Shutdown =:= true ->
        {stop, normal, ok, State};
    Busy2 ->
        {reply, ok, State#state{free = [Worker | Free], busy = Busy2}}
    end;

handle_call(stop, _From, #state{shutdown = true} = State) ->
    {reply, ok, State};

handle_call(stop, _From, #state{free = Free, busy = []} = State) ->
    lists:foreach(fun ibrowse_http_client:stop/1, Free),
    {stop, normal, ok, State};

handle_call(stop, _From, #state{free = Free} = State) ->
    lists:foreach(fun ibrowse_http_client:stop/1, Free),
    {reply, ok, State#state{free = [], shutdown = true}}.

handle_cast(Msg, State) ->
    {stop, {unexpected_cast, Msg}, State}.


handle_info({'EXIT', From, Reason},
    #state{free = Free, busy = Busy, shutdown = Shutdown} = State) ->
    case Busy -- [From] of
    Busy ->
        case Free -- [From] of
        Free ->
            {stop, {unknown_process_died, {From, Reason}}, State};
        Free2 ->
            {noreply, State#state{free = Free2}}
        end;
    [] when Shutdown =:= true ->
        {stop, normal, State};
    Busy2 ->
        {noreply, State#state{busy = Busy2}}
    end.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


terminate(_Reason, _State) ->
    ok.
