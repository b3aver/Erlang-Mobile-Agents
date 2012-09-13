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
-export([used_modules/0, init/1, handle_migrate/1, handle_crash/1]}.

-export([wait/1]).

-define(AGENT_NAME, tester).


%%%===================================================================
%%% API
%%%===================================================================

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
    wait(Sec);
init([]) ->
    wait(10).


handle_migrate(State) ->
    ok.


handle_crash(State) ->
    ok.
