%%%-------------------------------------------------------------------
%%% @author Paolo C Sberna <reliablebeaver86-cs@yahoo.it>
%%% @copyright (C) 2012, Paolo C Sberna
%%% @doc
%%%
%%% @end
%%% Created : 21 Aug 2012 by Paolo C Sberna <reliablebeaver86-cs@yahoo.it>
%%%-------------------------------------------------------------------
-module(demo_agent).

-behaviour(agent).

%% agent callbacks
-export([used_modules/0, init/1, handle_migration/1, handle_crash/1]).

%% API
-export([start_link/2]).

-define(AGENT_NAME, demo).
-define(MAX_JUMPS, 10).
-define(MIN_JUMPS, 4).
-define(MAX_WAIT, 5).

-record(state, {name, nodes, jumps, logger}).


%%%===================================================================
%%% API
%%%===================================================================

start_link(Name, Args) ->
    agent:start_link(Name, ?MODULE, Args).
    

%%%===================================================================
%%% agent callbacks
%%%===================================================================

used_modules() ->
    [].


init({Name, [{nodes, Nodes}, {logger, Logger}, {leader, Leader}]}) ->
    Jumps = ?MIN_JUMPS+random:uniform(?MAX_JUMPS-?MIN_JUMPS),
    group_leader(Leader, self()),
    do_something(#state{name=Name, jumps=Jumps, nodes=Nodes, logger=Logger}).


handle_migration(State) ->
    do_something(State).


handle_crash(_State) ->
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================


do_something(#state{name=Name, jumps=Jumps, nodes=Nodes}=State) ->
    wait(random:uniform(?MAX_WAIT)),
    report(State),
    Node = lists:nth(random:uniform(length(Nodes)), Nodes),
    agent:migrate(Name, Node, State#state{jumps=Jumps-1}).


wait(Sec) ->
    receive
    after (1000*Sec) ->
            ok
    end.


report(#state{logger = Logger} = State) ->
    %% Logger ! io_lib:format("~p#~p#~p",
    %%                        [State#state.name, node(), State#state.jumps]).
    io:format("~p#~p#~p",
              [State#state.name, node(), State#state.jumps]).

