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
-export([start_link/0]).
-export([start_agent/4]).

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


start_agent(Agent, Module, Function, Arguments) ->
    Restart = temporary, %% never restart
    Shutdown = 2000,
    Type = worker,
    %% supervisor:start_child(?SERVER, {Agent, {Module, Function, Arguments},
    %%                                  Restart, Shutdown, Type, [Module]}).
    supervisor:start_child(?SERVER, {Agent,
                                     {agent, start_link,
                                      [Agent, Module, Function, Arguments]
                                     },
                                     Restart, Shutdown, Type, [agent, Module]}).



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
