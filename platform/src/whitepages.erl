%%%-------------------------------------------------------------------
%%% @author Paolo C Sberna <reliablebeaver86-cs@yahoo.it>
%%% @copyright (C) 2012, Paolo C Sberna
%%% @doc
%%%
%%% @end
%%% Created : 23 Jul 2012 by Paolo C Sberna <reliablebeaver86-cs@yahoo.it>
%%%-------------------------------------------------------------------
-module(whitepages).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([register/2,
         register/3,
         containers/0,
         containers/1,
         lookup_container/1,
         lookup_container/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-include("whitepages.hrl").

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


register(Container, ContainerNode) ->
    gen_server:call(?SERVER, {register, Container, ContainerNode}).
register(ServerNode, Container, ContainerNode) ->
    gen_server:call({?SERVER, ServerNode}, {register, Container, ContainerNode}).


containers() ->
    gen_server:call(?SERVER, containers).
containers(ServerNode) ->
    gen_server:call({?SERVER, ServerNode}, containers).


lookup_container(Node) ->
    gen_server:call(?SERVER, {lookup_container, Node}).
lookup_container(ServerNode, Node) ->
    gen_server:call({?SERVER, ServerNode}, {lookup_container, Node}).



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
    {ok, #state{containers=[]}}.

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
handle_call({register, Container, Node}, _From, #state{containers=ContList}) ->
    %% TODO check that From is the Container's pid
    case lists:keysearch(Container, #container.name, ContList) of
        {value, _ContainerSaved} ->
            %% Container is already present in the ContList
            Reply = {error, already_present},
            State = #state{containers=ContList},
            {reply, Reply, State};
        false ->
            %% Container is not in the ContList
            State = #state{containers=[#container{name=Container, node=Node}
                                       |ContList]},
            Reply = ok,
            {reply, Reply, State}
    end;


handle_call(containers, _From, State) ->
    {reply, State#state.containers, State};


handle_call({lookup_container, Node}, _From, State) ->
    case lists:keysearch(Node, #container.node, State#state.containers) of
        {value, Reply} ->
            ok;
        false ->
            Reply = {error, not_found}
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
