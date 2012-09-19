%%%-------------------------------------------------------------------
%%% @author Paolo C Sberna <reliablebeaver86-cs@yahoo.it>
%%% @copyright (C) 2012, Paolo C Sberna
%%% @doc
%%%
%%% @end
%%% Created : 19 Jul 2012 by Paolo C Sberna <reliablebeaver86-cs@yahoo.it>
%%%-------------------------------------------------------------------
-module(agent).

-behaviour(gen_server).

%% behavour definition
-export([behaviour_info/1]).

%% API
-export([start_link/3,
         reactivate/3,
         introduce/1,
         stop/1,
         migrate/3]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-include("agent.hrl").


%%%===================================================================
%%% Behaviour definition
%%%===================================================================

behaviour_info(callbacks) ->
    [{used_modules,0},
     {init, 1},
     {handle_migration, 1},
     {handle_crash, 1}];
behaviour_info(_Other) ->
    undefined.


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Name, Module, Arguments) ->
    gen_server:start_link({local, Name}, ?MODULE,
                          [start, Name, Module, Arguments], []).


reactivate(Name, Module, State) ->
    gen_server:start_link({local, Name}, ?MODULE,
                          [reactivate, Name, Module, State], []).


introduce(Name) ->
    gen_server:call(Name, introduce).


stop(Name) ->
    gen_server:cast(Name, stop).


migrate(Name, Node, State) ->
    gen_server:call(Name, {migrate, Node, State}).
    

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------

init([start, Name, Module, Arguments]) ->
    process_flag(trap_exit, true),
    Pid = spawn_link(Module, init, [{Name, Arguments}]),
    {ok, #state{name=Name, module=Module, arguments=Arguments, pid=Pid}};

init([reactivate, Name, Module, State]) ->
    process_flag(trap_exit, true),
    Pid = spawn_link(Module, handle_migration, [{Name, State}]),
    {ok, #state{name=Name, module=Module, pid=Pid}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |

%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(introduce, _From, State) ->
    {reply, self(), State};


handle_call({migrate, Node, MigState}, _From, State) ->
    %% retrieve the name of the current agent
    Agent = State#state.name,
    %% ask to the manager to migrate
    case manager:migrate(Agent, Node, MigState) of
        ok ->
            {stop, migrated, ok, State};
        Reply ->
            {reply, Reply, State}
    end;


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};


handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, normal=Reason}, #state{pid=Pid}=State) ->
    NewState = State#state{pid=undefined},
    {stop, Reason, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

