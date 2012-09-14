%%%-------------------------------------------------------------------
%%% @author Paolo C Sberna <reliablebeaver86-cs@yahoo.it>
%%% @copyright (C) 2012, Paolo C Sberna
%%% @doc
%%%
%%% @end
%%% Created : 17 Jul 2012 by Paolo C Sberna <reliablebeaver86-cs@yahoo.it>
%%%-------------------------------------------------------------------
-module(manager).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([start_agent/3,
         start_agent/4,
         host_agent/6,
         migrate/4,
         get_module/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-include("manager.hrl").

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
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%% start_agent(Agent, Module, Function, Arguments, Dependencies) ->
%%     gen_server:call(?SERVER, {start_agent, Agent, Module, Function, Arguments,
%%                               Dependencies}).
%% start_agent(ServerNode, Agent, Module, Function, Arguments, Dependencies) ->
%%     gen_server:call({?SERVER, ServerNode}, {start_agent, Agent, Module, Function,
%%                                             Arguments, Dependencies}).
start_agent(Agent, Module, Arguments) ->
    gen_server:call(?SERVER, {start_agent, Agent, Module, Arguments}).
start_agent(ServerNode, Agent, Module, Arguments) ->
    gen_server:call({?SERVER, ServerNode},
                    {start_agent, Agent, Module, Arguments}).


migrate(Agent, Node, Function, Arguments) ->
    gen_server:call(?SERVER, {migrate, Agent, Node, Function, Arguments}).


host_agent(ServerNode, Agent, Module, Function, Arguments, Dependencies) ->
    gen_server:call({?SERVER, ServerNode}, {host_agent, Agent, Module, Function,
                                            Arguments, Dependencies}).


get_module(Manager, Module) when is_pid(Manager) ->
    gen_server:call(Manager, {get_module, Module});
get_module(ServerNode, Module) ->
    gen_server:call({?SERVER, ServerNode}, {get_module, Module}).


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
init([]) ->
    {ok, #state{agents=[]}}.

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
handle_call({start_agent, Agent, Module, Arguments}, _From, OldState) ->
    %% Function = start_link,
    %% Dependencies = Module:used_modules(),
    %% {Reply, NewState} = bootstrap_agent(Agent, Module, Function, Arguments,
    %%                                     Dependencies, OldState),
    {Reply, NewState} = bootstrap_agent(Agent, Module, Arguments, OldState),

    {reply, Reply, NewState};


handle_call({host_agent, _A, _M, _F, _A, Dependencies}, _From, OldState)
  when not(is_list(Dependencies)) ->
    {reply, {error, bad_args}, OldState};
handle_call({host_agent, Agent, Module, Function, Arguments, Dependencies},
            {From, _Tag},
            OldState) ->
    case load_modules(From, [Module|Dependencies]) of
        ok ->
            {Reply, NewState}
                %% = bootstrap_agent(Agent, Module, Function, Arguments,
                %%                   Dependencies, OldState);
                = bootstrap_agent(Agent, Module, Arguments, OldState);
        Error ->
            Reply = Error,
            NewState = OldState
    end,
    
    {reply, Reply, NewState};


handle_call({migrate, Agent, Node, Function, Arguments},
            From,
            OldState = #state{agents=OldAgents}) ->
    %% lookfor agent stored informations: module, pid
    case lists:keysearch(Agent, #agent.name, OldAgents) of
        {value, AgentInfo} ->
            AgentPid = AgentInfo#agent.pid,
            FromPid = element(1, From),
            %% check the Pid
            if 
                AgentPid == FromPid ->
                    Module = AgentInfo#agent.module,
                    Dependencies = AgentInfo#agent.dependencies,
                    %% start Agent in Node
                    case manager:host_agent(Node, Agent, Module, 
                                            Function, Arguments, Dependencies) of
                        {ok, _NewPid} ->
                            Reply = ok,
                            %% change agent stored informations
                            NewState = change_state_migrated(OldState, Agent),
                            ok;
                        {ok, _NewPid, _Info} ->
                            Reply = ok,
                            %% change agent stored informations
                            NewState = change_state_migrated(OldState, Agent),
                            ok;
                        {error, _Error} ->
                            Reply = {error, restart_error},
                            NewState = OldState
                    end;
                true ->
                    Reply = {error, forbidden},
                    NewState = OldState
            end;
        false ->
            Reply = {error, wrong_agent},
            NewState = OldState 
    end,

    {reply, Reply, NewState};


handle_call({get_module, Module}, _From, State) ->
    case code:get_object_code(Module) of
        error ->
            Reply = error;
        {Module, Code, _Filename} ->
            Reply = Code
    end,
    
    {reply, Reply, State};


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
handle_info({'DOWN', _Reference, process, {Agent, _Node}, normal},
            OldState = #state{agents=OldAgents}) ->
    case lists:keysearch(Agent, #agent.name, OldAgents) of
        {value, _AgentInfo} ->
            NewState = change_state_terminated(OldState, Agent);
        false ->
            NewState = OldState
    end,
    {noreply, NewState};

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
change_state_terminated(State, Agent) ->
    change_state(State, Agent, terminated).

change_state_migrated(State, Agent) ->
    change_state(State, Agent, migrated).

change_state(#state{agents=OldAgents}, Agent, Status) ->
    {value, AgentInfo} = lists:keysearch(Agent, #agent.name, OldAgents),
    NewAgents = lists:keyreplace(Agent, #agent.name, OldAgents,
                                 AgentInfo#agent{state=Status,
                                                 pid=undefined}),
    _NewState = #state{agents=NewAgents}.



%% bootstrap_agent(Agent, Module, Function, Arguments, Dependencies,
%%                 OldState = #state{agents=OldAgents}) ->
bootstrap_agent(Agent, Module, Arguments, OldState = #state{agents=OldAgents}) ->
    Function = start_link,
    Dependencies = Module:used_modules(),
    case lists:keysearch(Agent, #agent.name, OldAgents) of
        {value, #agent{state=running}} ->
            %% there is an agent with the same name still_running
            {{error, still_running}, #state{agents=OldAgents}};
        {value, _AgentRecord} ->
            %% there is an agent with the same name but isn't running
            %% start the agent
            Reply = agents_sup:start_agent(Agent, Module, Arguments),
            %% rebuild the gen_server state
            NewState =
                case Reply of
                    {ok, AgentPid} ->
                        _Reference = erlang:monitor(process, Agent),
                        OldAgentsPurged = lists:keydelete(Agent,
                                                          #agent.name,
                                                          OldAgents),
                        #state{agents=[#agent{name=Agent,
                                              pid=AgentPid,
                                              module=Module,
                                              function=Function,
                                              arguments=Arguments,
                                              dependencies=Dependencies,
                                              state=running}
                                       |OldAgentsPurged]};
                    {ok, AgentPid, _Info} ->
                        _Reference = erlang:monitor(process, Agent),
                        OldAgentsPurged = lists:keydelete(Agent,
                                                          #agent.name,
                                                          OldAgents),
                        #state{agents=[#agent{name=Agent,
                                              pid=AgentPid,
                                              module=Module,
                                              function=Function,
                                              arguments=Arguments,
                                              dependencies=Dependencies,
                                              state=running}
                                       |OldAgentsPurged]};
                    {error, _Error} ->
                        OldState
                end,
            {Reply, NewState};
        false ->
            %% there aren't agents with this name
            %% start the agent
            Reply = agents_sup:start_agent(Agent, Module, Arguments),
            %% rebuild the gen_server state
            NewState =
                case Reply of
                    {ok, AgentPid} ->
                        #state{agents=[#agent{name=Agent,
                                              pid=AgentPid,
                                              module=Module,
                                              function=Function,
                                              arguments=Arguments,
                                              dependencies=Dependencies,
                                              state=running}
                                       |OldAgents]};
                    {ok, AgentPid, _Info} ->
                        #state{agents=[#agent{name=Agent,
                                              pid=AgentPid,
                                              module=Module,
                                              function=Function,
                                              arguments=Arguments,
                                              dependencies=Dependencies,
                                              state=running}
                                       |OldAgents]};
                    {error, _Error} ->
                        OldState
                end,
            {Reply, NewState}
    end.


load_modules(_From, []) ->
    ok;
load_modules(From, [Module|Rest]) ->
    case code:load_file(Module) of
        {module, Module} ->
            Reply = load_modules(From, Rest);
        {error, sticky_directory} ->
            Reply = load_modules(From, Rest);
        {error, nofile} ->
            case  manager:get_module(From, Module) of
                error ->
                    Reply = {error, load_module_error};
                Binary ->
                    case code:load_binary(Module, "./tmp/"++atom_to_list(Module),
                                          Binary) of
                        {module, Module} ->
                            Reply = load_modules(From, Rest);
                        Error ->
                            Reply = Error
                    end
            end
    end,
        
    Reply.
