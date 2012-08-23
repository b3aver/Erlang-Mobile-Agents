%%%-------------------------------------------------------------------
%%% @author Paolo C Sberna <reliablebeaver86-cs@yahoo.it>
%%% @copyright (C) 2012, Paolo C Sberna
%%% @doc
%%%
%%% @end
%%% Created : 21 Aug 2012 by Paolo C Sberna <reliablebeaver86-cs@yahoo.it>
%%%-------------------------------------------------------------------
-module(tester_agent).

-export([wait/1]).

-define(AGENT_NAME, tester).


wait(Sec) ->
    receive
    after (1000*Sec) ->
            ok
    end.



init() ->
    manager:start_agent(manager, ?AGENT_NAME).
