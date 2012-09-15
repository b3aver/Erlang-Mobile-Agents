%%%-------------------------------------------------------------------
%%% @author Paolo C Sberna <reliablebeaver86-cs@yahoo.it>
%%% @copyright (C) 2012, Paolo C Sberna
%%% @doc
%%%
%%% @end
%%% Created : 21 Aug 2012 by Paolo C Sberna <reliablebeaver86-cs@yahoo.it>
%%%-------------------------------------------------------------------
-module(tester_agent).

-behaviour(agent).

%% agent callbacks
-export([used_modules/0, init/1, handle_migration/1, handle_crash/1]).

%% API
-export([start_link/2,
         wait/1]).

-define(AGENT_NAME, tester).


%%%===================================================================
%%% API
%%%===================================================================

start_link(Name, Args) ->
    agent:start_link(Name, ?MODULE, Args).
    

wait(Sec) ->
    receive
    after (1000*Sec) ->
            ok
    end.


%%%===================================================================
%%% agent callbacks
%%%===================================================================

used_modules() ->
    [erlang].


init([Sec]) ->
    ?MODULE:wait(Sec);
init([]) ->
    ?MODULE:wait(10).


handle_migration([wait, Sec]) ->
    ?MODULE:wait(Sec).


handle_crash(_State) ->
    ok.
