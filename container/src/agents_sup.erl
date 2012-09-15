%%%-------------------------------------------------------------------
%%% @author Paolo C Sberna <reliablebeaver86-cs@yahoo.it>
%%% @copyright (C) 2012, Paolo C Sberna
%%% @doc
%%%
%%% @end
%%% Created : 17 Jul 2012 by Paolo C Sberna <reliablebeaver86-cs@yahoo.it>
%%%-------------------------------------------------------------------
-module(agents_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_agent/3,
         host_agent/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


start_agent(Agent, Module, Arguments) ->
    start_agent_fun(Agent, Module, Arguments, start_link).


host_agent(Agent, Module, State) ->
    start_agent_fun(Agent, Module, State, reactivate).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    %% Restart = permanent,
    %% Shutdown = 2000,
    %% Type = worker,

    %% AChild = {'AName', {'AModule', start_link, []},
    %%           Restart, Shutdown, Type, ['AModule']},

    {ok, {SupFlags, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_agent_fun(Agent, Module, State, Fun) ->
    Restart = temporary, %% never restart
    Shutdown = 2000,
    Type = worker,
    supervisor:start_child(?SERVER, {Agent,
                                     {agent, Fun,
                                      [Agent, Module, State]
                                     },
                                     Restart, Shutdown, Type, 
                                     [agent, Module | Module:used_modules()]}).
