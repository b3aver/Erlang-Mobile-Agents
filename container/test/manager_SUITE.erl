%%%-------------------------------------------------------------------
%%% @author Paolo C Sberna <reliablebeaver86-cs@yahoo.it>
%%% @copyright (C) 2012, Paolo C Sberna
%%% @doc
%%%
%%% @end
%%% Created : 19 Jul 2012 by Paolo C Sberna <reliablebeaver86-cs@yahoo.it>
%%%-------------------------------------------------------------------
-module(manager_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-include("../src/manager.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%%  Returns list of tuples to set default properties
%%  for the suite.
%%
%% Function: suite() -> Info
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Note: The suite/0 function is only meant to be used to return
%% default data values, not perform any other operations.
%%
%% @spec suite() -> Info
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{minutes,10}}].

%%--------------------------------------------------------------------
%% @doc
%% Initialization before the whole suite
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the suite.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%
%% @spec init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Cleanup after the whole suite
%%
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% @spec end_per_suite(Config) -> _
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Initialization before each test case group.
%%
%% GroupName = atom()
%%   Name of the test case group that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%% Reason = term()
%%   The reason for skipping all test cases and subgroups in the group.
%%
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Cleanup after each test case group.
%%
%% GroupName = atom()
%%   Name of the test case group that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%%
%% @spec end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Initialization before each test case
%%
%% TestCase - atom()
%%   Name of the test case that is about to be run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Cleanup after each test case
%%
%% TestCase - atom()
%%   Name of the test case that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% @spec end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Returns a list of test case group definitions.
%%
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%%   The name of the group.
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%%   Group properties that may be combined.
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%%   The name of a test case.
%% Shuffle = shuffle | {shuffle,Seed}
%%   To get cases executed in random order.
%% Seed = {integer(),integer(),integer()}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%%   To get execution of cases repeated.
%% N = integer() | forever
%%
%% @spec: groups() -> [Group]
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @doc
%%  Returns the list of groups and test cases that
%%  are to be executed.
%%
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%%   Name of a test case group.
%% TestCase = atom()
%%   Name of a test case.
%% Reason = term()
%%   The reason for skipping all groups and test cases.
%%
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% @end
%%--------------------------------------------------------------------
all() -> 
    [start_stop_case,
     %% change_state_migrated_case,
     init_case,
     handle_call_unknown_command_case,
     handle_call_start_agent_running_case,
     handle_call_start_agent_present_case,
     handle_call_start_agent_new_case,
     handle_call_host_agent_module_present_case,
     handle_call_host_agent_transfer_error_case,
     handle_call_host_agent_no_errors_case,
     handle_call_migrate_not_present_case,
     handle_call_migrate_different_pid_case,
     handle_call_migrate_start_error_case,
     handle_call_migrate_no_errors_case,
     handle_call_get_module_local_case,
     handle_cast_case,
     handle_info_case,
     start_agent4_case,
     start_agent5_case,
     host_agent_case,
     migrate_case,
     get_module_case,
     get_module_pid_case].


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc 
%%  Test case info function - returns list of tuples to set
%%  properties for the test case.
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Note: This function is only meant to be used to return a list of

%% values, not perform any other operations.
%%
%% @spec TestCase() -> Info 
%% @end
%%--------------------------------------------------------------------
my_test_case() -> 
    [].

%%--------------------------------------------------------------------
%% @doc Test case function. (The name of it must be specified in
%%              the all/0 list or in a test case group for the test case
%%              to be executed).
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%% Comment = term()
%%   A comment about the test case that will be printed in the html log.
%%
%% @spec TestCase(Config0) ->
%%           ok | exit() | {skip,Reason} | {comment,Comment} |
%%           {save_config,Config1} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
my_test_case(_Config) -> 
    ok.


%%
%% Start and stop test
%%
start_stop_case(_Config) ->
    process_flag(trap_exit, true),

    %% check start
    {ok, Pid} = manager:start_link(),
    Pid = whereis(manager),
    true = erlang:is_process_alive(whereis(manager)),

    %% stop the process
    exit(Pid, kill),
    receive 
        {'EXIT', Pid, killed} ->
            undefined = whereis(manager)
    after
        1000 -> exit(shutdown_error)
    end,

    ok.


%%
%% Internal functions' tests
%% (useless because aren't exported)
%%
change_state_migrated_case(_Config) ->
    Agent = agent,
    AgentInfo = #agent{name=Agent,
                       pid=self(),
                       module=?MODULE,
                       function=hello_worlod,
                       arguments=nothing,
                       state=runnning},
    AgentList = [#agent{name=agent2}, AgentInfo, #agent{name=agent3}],
    OldState = #state{agents=AgentList},
    NewState = #state{agents=[#agent{name=agent2},
                              AgentInfo#agent{state=migrated, pid=undefined},
                              #agent{name=agent3}]},
    
    case manager:change_state_migrated(OldState, Agent) of
        NewState ->
            ok;
        _ -> 
            exit(return_value_error)
    end,
    
    ok.


%%
%% Callback functions' tests
%%
init_case(_Config) ->
    {ok, #state{agents=[]}} = manager:init([]),

    ok.


handle_call_unknown_command_case(_Config) ->
    %% fake_command
    {reply, ok, fake_state} = manager:handle_call(fake_command, from, fake_state),

    ok.

%%
%% start_agent
%%
handle_call_start_agent_running_case(_Config) ->
    %% start_agent
    %% Agent present in the state and running
    Agent = agent,
    Module = agent,
    Function = start_link,
    Arguments = [],
    Dependencies = [gen_server],
    State = #state{agents=[#agent{name=Agent,
                                     state=running}]},
    {reply, {error, still_running}, State}
        = manager:handle_call({start_agent, Agent, Module, Function, Arguments,
                               Dependencies}, from, State),
    
    ok.
    

handle_call_start_agent_present_case(_Config) ->
    %% start_agent
    %% Agent present in the state but not running
    process_flag(trap_exit, true),
    Agent = agent,
    Module = tester_agent,
    Function = wait,
    Arguments = [10],
    Dependencies = [],
    OldState = #state{agents=[#agent{name=Agent,
                                    state=terminated}]},
    {ok, SupPid} = agents_sup:start_link(),

    {reply, Ret, #state{agents=[#agent{name=Agent,
                                       pid=AgentPid,
                                       module=Module,
                                       function=Function,
                                       arguments=Arguments,
                                       dependencies=Dependencies,
                                       state=running}]}
    } = manager:handle_call({start_agent, Agent, Module, Function, Arguments,
                             Dependencies}, from, OldState),
    case Ret of
        {ok, AgentPid} ->
            ok;
        {ok, AgentPid, _Info} ->
            ok;
        {error, _Error} ->
            exit(start_agent_error)
    end,

    exit(SupPid, kill),
    receive
        {'EXIT', SupPid, killed } ->
            ok
    after
        1000 -> exit(shutdown_error)
    end,

    ok.



handle_call_start_agent_new_case(_Config) ->
    %% start_agent
    process_flag(trap_exit, true),
    Agent = agent,
    Module = tester_agent,
    Function = wait,
    Arguments = [10],
    Dependencies = [],
    OldState = #state{agents=[anything]},
    {ok, SupPid} = agents_sup:start_link(),

    {reply, Ret, #state{agents=[#agent{name=Agent,
                                       pid=AgentPid,
                                       module=Module,
                                       function=Function,
                                       arguments=Arguments,
                                       dependencies=Dependencies,
                                       state=running}|[anything]]}
    } = manager:handle_call({start_agent, Agent, Module, Function, Arguments,
                             Dependencies}, from, OldState),
    case Ret of
        {ok, AgentPid} ->
            ok;
        {ok, AgentPid, _Info} ->
            ok;
        {error, _Error} ->
            exit(start_agent_error)
    end,

    exit(SupPid, kill),
    receive
        {'EXIT', SupPid, killed } ->
            ok
    after
        1000 -> exit(shutdown_error)
    end,

    ok.


%%
%% host_agent
%%
handle_call_host_agent_module_present_case(_Config) ->
    %% host_agent
    %% Module in the code search path
    %% Agent present in the state and running
    Agent = agent,
    Module = agent,
    Function = start_link,
    Arguments = [],
    Dependencies = [erlang],
    State = #state{agents=[#agent{name=Agent,
                                     state=running}]},

    {reply, {error, still_running}, State}
        = manager:handle_call({host_agent, Agent, Module, Function, Arguments,
                               Dependencies}, {from, tag}, State),

    ok.


handle_call_host_agent_transfer_error_case(_Config) ->
    %% host_agent
    %% Module is not in the code search path
    %% nether in the manager that requires to host the agent.
    Agent = agent,
    Module = foo,
    Function = bar,
    Arguments = [],
    Dependencies = [],

    %% start a manager simulating the one that requires to host the agent
    {ok, ManagerPid} = manager:start_link(),
    State = #state{},

    {reply, {error, load_module_error}, State}
        = manager:handle_call({host_agent, Agent, Module, Function, Arguments,
                               Dependencies}, {ManagerPid, tag}, State),

    ok.


handle_call_host_agent_no_errors_case(_Config) ->
    %% host_agent
    %% Module is not in the code search path
    %% but it is in the manager that requires to host the agent.
    Agent = agent,
    Module = tester_agent,
    Function = wait,
    Arguments = [10],
    Dependencies = [],
    Node = node,
    NodeL = list_to_atom("node@"++net_adm:localhost()),     

    ok = application:start(container),

    %% save the original code search path
    CodePath = code:get_path(),
    %% remove the current directory
    code:del_path(test),
    
    %% create another node running the container application
    %% simulating the one that requires to host the agent
    Args = "-pa "++lists:nth(3, CodePath)
        ++" "++lists:nth(2, CodePath)
        ++" "++lists:nth(1, CodePath),
    {ok, NodeL} = slave:start(net_adm:localhost(), Node, Args),
    pong = net_adm:ping(NodeL),
    %% start the container application
    ok = rpc:call(NodeL, application, start, [container]),
    
    %% retrieve the manager's pid
    ManagerPid = rpc:call(NodeL, erlang, whereis, [manager]),
    
    %% do the call
    State = #state{agents=[]},    
    {reply, Ret, #state{agents=[#agent{name=Agent, pid=AgentPid, module=Module,
                                       function=Function, arguments=Arguments,
                                       dependencies=Dependencies,
                                       state=running}]}}
        = manager:handle_call({host_agent, Agent, Module, Function, Arguments,
                               Dependencies}, {ManagerPid, tag}, State),
    case Ret of
        {ok, AgentPid} ->
            ok;
        {ok, AgentPid, _Info} ->
            ok;
        {error, _Error} ->
            exit(start_agent_error)
    end,
    
    Children = supervisor:which_children(agents_sup),
    {value, {Agent, AgentPid, worker, [agent, Module]}}
        = lists:keysearch(Agent, 1, Children),

    %% restore the original code search path
    code:add_pathsa(lists:reverse(CodePath)),
    CodePath = code:get_path(),

    %% stop the container application
    ok = application:stop(container),
    ok = rpc:call(NodeL, application, stop, [container]),
    %% stop the container node
    ok = slave:stop(NodeL),

    ok.


%%
%% migrate
%%
handle_call_migrate_not_present_case(_Config) ->
    %% migrate
    %% Agent not present in the state of the manager.
    Agent = agent,
    Node = node,
    Function = function,
    Arguments = args,
    From = {self(), ref},
    State = #state{agents=[#agent{name=agent1}, #agent{name=agent2}]},

    {reply, {error, wrong_agent}, State} = manager:handle_call({migrate, Agent,
                                                                Node, Function,
                                                                Arguments},
                                                               From, State),

    ok.
    

handle_call_migrate_different_pid_case(_Config) ->
    %% migrate
    %% Agent present in the state of the manager, but with different pid.
    Agent = agent,
    Node = node,
    Function = function,
    Arguments = args,
    From = {list_to_pid("<0.1.0>"), ref},
    AgentInfo = #agent{name=Agent, pid=list_to_pid("<0.2.0>")},
    State = #state{agents=[#agent{name=agent1}, #agent{name=agent2}, AgentInfo]},

    {reply, {error, forbidden}, State} = manager:handle_call({migrate, Agent,
                                                              Node, Function,
                                                              Arguments},
                                                             From, State),
    
    ok.


handle_call_migrate_start_error_case(_Config) ->
    %% migrate
    %% try to start the agent in the other node, but occur an error
    Node = node,
    NodeL = list_to_atom("node@"++net_adm:localhost()), 

    Agent = agent,
    Module = tester_agent,
    Function = wait,
    Arguments = [10],
    Dependencies = [],
    FromPid = list_to_pid("<0.1.0>"),
    From = {FromPid, ref},
    AgentInfo = #agent{name=Agent, pid=FromPid, module=Module, function=Function, 
                       arguments=Arguments, dependencies=Dependencies,
                       state=running},
    State = #state{agents=[#agent{name=agent1}, #agent{name=agent2}, AgentInfo]},

    %% create another node running the container application
    CodePath = code:get_path(),
    Args = "-pa "++lists:nth(3, CodePath)
        ++" "++lists:nth(2, CodePath)
        ++" "++lists:nth(1, CodePath),
    {ok, NodeL} = slave:start(net_adm:localhost(), Node, Args),
    pong = net_adm:ping(NodeL),
    %% start the container application
    ok = rpc:call(NodeL, application, start, [container]),

    %% start an agent with the same name
    case manager:start_agent(NodeL, Agent, tester_agent, wait, [10], []) of
        {ok, _AgentPid} -> ok;
        {ok, _AgentPid, _Info} -> ok;
        {error, _Error} -> exit(start_agent_error)
    end,

    {reply, {error, restart_error}, State}
        = manager:handle_call({migrate, Agent, NodeL, Function, Arguments},
                              From, State),
    
    %% stop the container application
    ok = rpc:call(NodeL, application, stop, [container]),
    %% stop the container node
    ok = slave:stop(NodeL),

    ok.


handle_call_migrate_no_errors_case(_Config) ->
    %% migrate
    %% start the agent in the other node
    Node = node,
    NodeL = list_to_atom("node@"++net_adm:localhost()), 

    Agent = agent,
    Module = tester_agent,
    Function = wait,
    Arguments = [10],
    Dependencies = [],
    FromPid = list_to_pid("<0.1.0>"),
    From = {FromPid, ref},
    AgentInfo = #agent{name=Agent, pid=FromPid, module=Module, function=Function, 
                       arguments=Arguments, dependencies=Dependencies,
                       state=running},
    State = #state{agents=[#agent{name=agent1}, #agent{name=agent2}, AgentInfo]},

    %% create another node running the container application
    CodePath = code:get_path(),
    Args = "-pa "++lists:nth(3, CodePath)
        ++" "++lists:nth(2, CodePath)
        ++" "++lists:nth(1, CodePath),
    {ok, NodeL} = slave:start(net_adm:localhost(), Node, Args),
    pong = net_adm:ping(NodeL),
    %% start the container application
    ok = rpc:call(NodeL, application, start, [container]),

    %% start an agent with a different name
    case manager:start_agent(NodeL, agent2, tester_agent, wait, [10], []) of
        {ok, _AgentPid} -> ok;
        {ok, _AgentPid, _Info} -> ok;
        {error, _Error} -> exit(start_agent_error)
    end,

    NewAgentInfo = AgentInfo#agent{pid=undefined, state=migrated},
    NewState = #state{agents=[#agent{name=agent1},
                              #agent{name=agent2},
                              NewAgentInfo]},
    
    {reply, ok, NewState} = manager:handle_call({migrate, Agent, NodeL, Function,
                                              Arguments}, From, State),
    
    Pid = gen_server:call({agent, NodeL}, introduce),
    true = is_pid(Pid),
    
    Children = supervisor:which_children({agents_sup, NodeL}),
    {value, {Agent, Pid, worker, [agent, Module]}} = lists:keysearch(Agent, 1, Children),
    
    %% stop the container application
    ok = rpc:call(NodeL, application, stop, [container]),
    %% stop the container node
    ok = slave:stop(NodeL),

    ok.


handle_call_get_module_local_case(_Config) ->
    %% get_module
    %% locally in the current node

    %% nonexistent module
    {reply, error, state}
        = manager:handle_call({get_module, fake_module}, from, state),

    %% exitent module
    Module = tester_agent,
    {Module, Code, _Filename} = code:get_object_code(Module),
    {reply, Code, state}
        = manager:handle_call({get_module, Module}, from, state),
    
    ok.

    
handle_cast_case(_Config) ->
    {noreply, fake_state} = manager:handle_cast(fake_msg, fake_state),

    ok.


handle_info_case(_Config) ->
    %% process termination case
    Agent = agent,
    Node = node(),
    Reference = ref,

    AgentInfo = #agent{name=Agent, state=running},
    OldState = #state{agents=[AgentInfo]},
    NewState = #state{agents=[AgentInfo#agent{state=terminated}]},
        
    %% agent present
    Msg1 = {'DOWN', Reference, process, {Agent, Node}, normal},
    {noreply, NewState} = manager:handle_info(Msg1, OldState),
    
    %% agent not present, no changes
    Msg2 = {'DOWN', Reference, process, {fake_agent, Node}, normal},
    {noreply, OldState} = manager:handle_info(Msg2, OldState),

    %% default case
    {noreply, fake_state} = manager:handle_info(fake_info, fake_state),

    ok.


%%
%% API functions' tests
%%
start_agent4_case(_Config) ->
    Agent = agent,
    process_flag(trap_exit, true),
    {ok, ManPid} = manager:start_link(),
    {ok, SupPid} = agents_sup:start_link(),
    Ret = manager:start_agent(Agent, tester_agent, wait, [10], []),
    case Ret of
        {ok, AgentPid} ->
            true = erlang:is_process_alive(AgentPid),
            AgentPid = whereis(Agent),
            AgentPid = agent:introduce(Agent),
            ok;
        {ok, AgentPid, _Info} ->
            true = erlang:is_process_alive(AgentPid),
            AgentPid = whereis(Agent),
            AgentPid = agent:introduce(Agent),
            ok;
        {error, _Error} ->
            exit(start_agent_error)
    end,

    
    agent:stop(Agent),
    exit(SupPid, kill),
    receive
        {'EXIT', SupPid, killed } ->
            ok
    after
        1000 -> exit(shutdown_error)
    end,
    exit(ManPid, kill),
    receive
        {'EXIT', ManPid, killed } ->
            ok
    after
        1000 -> exit(shutdown_error)
    end,

    ok.


start_agent5_case(_Config) ->
    Agent = agent,
    process_flag(trap_exit, true),
    {ok, ManPid} = manager:start_link(),
    {ok, SupPid} = agents_sup:start_link(),
    Ret = manager:start_agent(node(), Agent, tester_agent, wait, [10], []),
    case Ret of
        {ok, AgentPid} ->
            true = erlang:is_process_alive(AgentPid),
            AgentPid = whereis(Agent),
            AgentPid = agent:introduce(Agent),
            ok;
        {ok, AgentPid, _Info} ->
            true = erlang:is_process_alive(AgentPid),
            AgentPid = whereis(Agent),
            AgentPid = agent:introduce(Agent),
            ok;
        {error, _Error} ->
            exit(start_agent_error)
    end,

    
    agent:stop(Agent),
    exit(SupPid, kill),
    receive
        {'EXIT', SupPid, killed } ->
            ok
    after
        1000 -> exit(shutdown_error)
    end,
    exit(ManPid, kill),
    receive
        {'EXIT', ManPid, killed } ->
            ok
    after
        1000 -> exit(shutdown_error)
    end,

    ok.


host_agent_case(_Config) ->
    Agent = agent,
    Module = tester_agent,
    Function = wait,
    Arguments = [10],
    Dependencies = [erlang, code],
    Node = node,
    NodeL = list_to_atom("node@"++net_adm:localhost()),     

    %% create another node running the container application
    %% simulating the one that requires to host the agent
    CodePath = code:get_path(),
    Args = "-pa "++lists:nth(3, CodePath)
        ++" "++lists:nth(2, CodePath)
        ++" "++lists:nth(1, CodePath),
    {ok, NodeL} = slave:start(net_adm:localhost(), Node, Args),
    pong = net_adm:ping(NodeL),
    %% start the container application
    ok = rpc:call(NodeL, application, start, [container]),
    
    %% do the call
    Ret = manager:host_agent(NodeL, Agent, Module, Function, Arguments,
                             Dependencies),
    Children = supervisor:which_children({agents_sup, NodeL}),
    case Ret of
        {ok, AgentPid} ->
            {value, {Agent, AgentPid, worker, [agent, Module]}}
                = lists:keysearch(Agent, 1, Children);
        {ok, AgentPid, _Info} ->
            {value, {Agent, AgentPid, worker, [agent, Module]}}
                = lists:keysearch(Agent, 1, Children);
        {error, _Error} ->
            exit(start_agent_error)
    end,
    
    %% stop the container application
    ok = rpc:call(NodeL, application, stop, [container]),
    %% stop the container node
    ok = slave:stop(NodeL),
    
    ok.


migrate_case(_Config) ->
    %% migrate
    %% start the agent in the other node
    Node = node,
    NodeL = list_to_atom("node@"++net_adm:localhost()), 

    Agent = agent,
    Module = tester_agent,
    Function = wait,
    Arguments = [10],

    %% start the application container locally
    ok = application:start(container),
    %% start an agent in the current node
    AgentPid = case manager:start_agent(Agent, Module, Function, Arguments, []) of
                   {ok, APid} -> APid;
                   {ok, APid, _Info} -> APid;
                   {error, _Error} -> exit(start_agent_error)
               end,
    AgentPid = agent:introduce(Agent),
    
    %% create another node running the container application
    CodePath = code:get_path(),
    Args = "-pa "++lists:nth(3, CodePath)
        ++" "++lists:nth(2, CodePath)
        ++" "++lists:nth(1, CodePath),
    {ok, NodeL} = slave:start(net_adm:localhost(), Node, Args),
    pong = net_adm:ping(NodeL),
    %% start the container application in the other node
    ok = rpc:call(NodeL, application, start, [container]),

    %% migrate the node
    ok = agent:migrate(Agent, NodeL),

    Pid = gen_server:call({agent, NodeL}, introduce),
    true = is_pid(Pid),
    Children = supervisor:which_children({agents_sup, NodeL}),
    {value, {Agent, Pid, worker, [agent, Module]}}
        = lists:keysearch(Agent, 1, Children),

    %% the local agent is really terminated
    false = is_process_alive(AgentPid),
   
    %% stop the container application
    ok = application:stop(container),
    ok = rpc:call(NodeL, application, stop, [container]),
    %% stop the container node
    ok = slave:stop(NodeL),
    
    ok.


get_module_case(_Config) ->
    %% get_module
    %% with a remote node
    Node = node,
    NodeL = list_to_atom("node@"++net_adm:localhost()), 
    %% create another node running the container application
    CodePath = code:get_path(),
    Args = "-pa "++lists:nth(3, CodePath)
        ++" "++lists:nth(2, CodePath)
        ++" "++lists:nth(1, CodePath),
    {ok, NodeL} = slave:start(net_adm:localhost(), Node, Args),
    pong = net_adm:ping(NodeL),
    %% start the container application
    ok = rpc:call(NodeL, application, start, [container]),

    %% load the binary of the module
    Module = tester_agent,
    {Module, Code, _Filename} = code:get_object_code(Module),
    %% ask the module to the remote node
    Code = manager:get_module(NodeL, Module),
    
    %% stop the container application
    ok = rpc:call(NodeL, application, stop, [container]),
    %% stop the container node
    ok = slave:stop(NodeL),
    
    ok.

    
get_module_pid_case(_Config) ->
    %% get_module
    %% with a remote node
    %% passing the manager pid
    Node = node,
    NodeL = list_to_atom("node@"++net_adm:localhost()), 
    %% create another node running the container application
    CodePath = code:get_path(),
    Args = "-pa "++lists:nth(3, CodePath)
        ++" "++lists:nth(2, CodePath)
        ++" "++lists:nth(1, CodePath),
    {ok, NodeL} = slave:start(net_adm:localhost(), Node, Args),
    pong = net_adm:ping(NodeL),
    %% start the container application
    ok = rpc:call(NodeL, application, start, [container]),

    %% retrieve the manager's pid
    ManagerPid = rpc:call(NodeL, erlang, whereis, [manager]),
    true = is_pid(ManagerPid),

    %% load the binary of the module
    Module = tester_agent,
    {Module, Code, _Filename} = code:get_object_code(Module),
    %% ask the module to the remote node
    Code = manager:get_module(ManagerPid, Module),

    %% stop the container application
    ok = rpc:call(NodeL, application, stop, [container]),
    %% stop the container node
    ok = slave:stop(NodeL),
    
    ok.
