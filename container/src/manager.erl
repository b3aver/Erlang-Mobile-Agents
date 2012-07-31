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
-export([start_agent/4,
         start_agent/5,
         migrate/4]).

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


start_agent(Agent, Module, Function, Arguments) ->
    gen_server:call(?SERVER, {start_agent, Agent, Module, Function, Arguments}).
start_agent(ServerNode, Agent, Module, Function, Arguments) ->
    gen_server:call({?SERVER, ServerNode},
                    {start_agent, Agent, Module, Function, Arguments}).


migrate(Agent, Node, Function, Arguments) ->
    gen_server:call(?SERVER, {migrate, Agent, Node, Function, Arguments}).


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
handle_call({start_agent, Agent, Module, Function, Arguments},
            _From,
            OldState = #state{agents=OldAgents}) ->
    case lists:keysearch(Agent, #agent.name, OldAgents) of
        {value, #agent{state=running}} ->
            %% there is an agent with the same name still_running
            {reply, {error, still_running}, #state{agents=OldAgents}};
        {value, _AgentRecord} ->
            %% there is an agent with the same name but isn't running
            %% start the agent
            Reply = agents_sup:start_agent(Agent, Module, Function, Arguments),
            %% rebuild the gen_server state
            NewState =
                case Reply of
                    {ok, AgentPid} ->
                        OldAgentsPurged = lists:keydelete(Agent,
                                                          #agent.name,
                                                          OldAgents),
                        #state{agents=[#agent{name=Agent,
                                              pid=AgentPid,
                                              module=Module,
                                              function=Function,
                                              arguments=Arguments,
                                              state=running}
                                       |OldAgentsPurged]};
                    {ok, AgentPid, _Info} ->
                        OldAgentsPurged = lists:keydelete(Agent,
                                                          #agent.name,
                                                          OldAgents),
                        #state{agents=[#agent{name=Agent,
                                              pid=AgentPid,
                                              module=Module,
                                              function=Function,
                                              arguments=Arguments,
                                              state=running}
                                       |OldAgentsPurged]};
                    {error, _Error} ->
                        OldState
                end,
            {reply, Reply, NewState};
        false ->
            %% there aren't agents with this name
            %% start the agent
            Reply = agents_sup:start_agent(Agent, Module, Function, Arguments),
            %% rebuild the gen_server state
            NewState =
                case Reply of
                    {ok, AgentPid} ->
                        #state{agents=[#agent{name=Agent,
                                              pid=AgentPid,
                                              module=Module,
                                              function=Function,
                                              arguments=Arguments,
                                              state=running}
                                       |OldAgents]};
                    {ok, AgentPid, _Info} ->
                        #state{agents=[#agent{name=Agent,
                                              pid=AgentPid,
                                              module=Module,
                                              function=Function,
                                              arguments=Arguments,
                                              state=running}
                                       |OldAgents]};
                    {error, _Error} ->
                        OldState
                end,
            {reply, Reply, NewState}
    end;


handle_call({migrate, Agent, Node, Function, Arguments},
            From,
            OldState = #state{agents=OldAgents}) ->
    %% lookfor agent stored informations: module, pid
    case lists:keysearch(Agent, #agent.name, OldAgents) of
        {value, AgentInfo} ->
            AgentPid = AgentInfo#agent.pid,
            FromPid = element(1, From),
            %%io:format("DEBUG manager: ~p ~p ~p~n", [AgentInfo, AgentPid, FromPid]),
            %% check the Pid
            if 
                AgentPid == FromPid ->
                    Module = AgentInfo#agent.module,
                    %% start Agent in Node
                    case manager:start_agent(Node, Agent, Module,
                                             Function, Arguments) of
                        {ok, _NewPid} ->
                            Reply = ok,
                            %% change agent stored informations
                            NewState = change_state_migrated(OldState, Agent),
                            %% exit agent
                            %% exit(AgentPid, migrated),
                            ok;
                        {ok, _NewPid, _Info} ->
                            Reply = ok,
                            %% change agent stored informations
                            NewState = change_state_migrated(OldState, Agent),
                            %% exit agent
                            %% exit(AgentPid, migrated),
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


change_state_migrated(#state{agents=OldAgents}, Agent) ->
    {value, AgentInfo} = lists:keysearch(Agent, #agent.name, OldAgents),
    NewAgents = lists:keyreplace(Agent, #agent.name, OldAgents,
                                 AgentInfo#agent{state=migrated,
                                                 pid=undefined}),
    _NewState = #state{agents=NewAgents}.
