%%%-------------------------------------------------------------------
%%% @author Paolo C Sberna <reliablebeaver86-cs@yahoo.it>
%%% @copyright (C) 2012, Paolo C Sberna
%%% @doc
%%%
%%% @end
%%% Created : 19 Jul 2012 by Paolo C Sberna <reliablebeaver86-cs@yahoo.it>
%%%-------------------------------------------------------------------
-module(agent_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-include("../src/agent.hrl").

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
     init_case,
     handle_call_case,
     handle_cast_unknown_command_case,
     handle_cast_stop_case,
     handle_info_exit_case,
     start_case,
     introduce_case,
     stop_case,
     migrate_case].


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


start_stop_case(_Config) -> 
    % check start
    {ok, Pid} = agent:start_link(agent),
    Pid = whereis(agent),
    true = erlang:is_process_alive(whereis(agent)),
    
    % check start of another agent with an internal process
    Time_to_live = 1,
    {ok, Pid2} = agent:start_link(agent2, tester_agent, wait, [Time_to_live]),
    Pid2 = whereis(agent2),
    true = erlang:is_process_alive(whereis(agent2)),
    receive after (1000*(Time_to_live+1)) -> nil end,
    false = erlang:is_process_alive(Pid2),
   
    % stop the processes
    agent:stop(agent),
    receive after 100 -> nil end,
    undefined = whereis(agent),
    %% true = erlang:is_process_alive(whereis(agent2)),
    %% agent:stop(agent2),
    %% receive after 100 -> nil end,
    %% undefined = whereis(agent),
    %% undefined = whereis(agent2),

    ok.


init_case(_Config) ->
    {ok, #state{}} = agent:init([]),

    Module = tester_agent,
    Function = wait,
    Arguments = [10],
    {ok, #state{module=Module, function=Function, arguments=Arguments, pid=Pid}} =
        agent:init([Module, Function, Arguments]),
    true = is_process_alive(Pid),

    ok.

    

handle_call_case(_Config) ->
    Self = self(),
    {reply, Self, fake_state} = agent:handle_call(introduce, from, fake_state),
    {reply, ok, fake_state} = agent:handle_call(fake_command, from, fake_state),
    

    Module = tester_agent,
    Function = wait,
    Arguments = [10],
    State = #state{module=Module},
    %% process already running
    {reply,
     {error, already_running},
     State
    } = agent:handle_call({start, Module, Function, Arguments}, from, State),

    %% start a new process
    {reply,
     ok,
     #state{module=Module, function=Function, arguments=Arguments, pid=Pid}
    } = agent:handle_call({start, Module, Function, Arguments}, from, #state{}),
    true = is_process_alive(Pid),
    
    ok.


handle_cast_unknown_command_case(_Config) ->
    {noreply, fake_state} = agent:handle_cast(fake_msg, fake_state),

    ok.

    
handle_cast_stop_case(_Config) ->
    {stop, normal, fake_state} = agent:handle_cast(stop, fake_state),

    ok.


handle_info_exit_case(_Config) ->
    Pid = pid,
    Reason = normal,
    State = #state{pid=Pid},
    NewState = State#state{pid=undefined},
    {stop, Reason, NewState} = agent:handle_info({'EXIT', Pid, Reason}, State),
    
    ok.


%%
%% API functions' tests
%%
start_case(_Config) ->
    Agent = agent,
    {ok, _AgentPid} = agent:start_link(Agent),
    agent:start(Agent, tester_agent, wait, [10]),
    agent:stop(Agent),

    ok.


introduce_case(_Config) ->
    Agent = agent,
    {ok, AgentPid} = agent:start_link(Agent),
    AgentPid = agent:introduce(Agent),
    AgentPid = agent:introduce({Agent, node()}),
    agent:stop(Agent),

    ok.

    
stop_case(_Config) ->
    Agent = agent,
    {ok, AgentPid} = agent:start_link(Agent),
    ok = agent:stop(Agent),
    %% ok = agent:stop({Agent, node()}),

    receive after 100 -> nill end,
    undefined = whereis(Agent),
    false = is_process_alive(AgentPid),
    
    ok.


migrate_case(_Config) ->
    %% migrate
    Node = node,
    NodeL = list_to_atom("node@"++net_adm:localhost()), 
    Agent = agent,

    %% start the container application locally
    ok = application:start(container),
    %% start an agent locally
    {ok, AgentPid} = manager:start_agent(Agent, tester_agent, wait, [10], []),
    AgentPid = agent:introduce(Agent),


    %% create another node running the container application
    CodePath = code:get_path(),
    Args = "-pa "++lists:nth(3, CodePath)
        ++" "++lists:nth(2, CodePath)
        ++" "++lists:nth(1, CodePath),
    {ok, NodeL} = slave:start(net_adm:localhost(), Node, Args),
    pong = net_adm:ping(NodeL),
    %% start the container application
    ok = rpc:call(NodeL, application, start, [container]),

    %% appmon:start(),
    %% AppmonPid = rpc:call(NodeL, appmon, start, []),
    %% receive after 5000 -> ok end,

    %% migrate the Agent to Node
    ok = agent:migrate(Agent, NodeL),
    receive after 100 -> ok end,
    false = is_process_alive(AgentPid),

    %% receive after 5000 -> ok end,     
    
    Children = supervisor:which_children({agents_sup, NodeL}),
    io:format("~p", [Children]),
    NewAgentPid = agent:introduce({Agent, NodeL}),
    true = is_pid(NewAgentPid),    
    {value, {Agent, NewAgentPid, worker, [agent, tester_agent]}}
        = lists:keysearch(Agent, 1, Children),
    
    %% stop the container application
    ok = application:stop(container),
    ok = rpc:call(NodeL, application, stop, [container]),
    %% stop the container node
    ok = slave:stop(NodeL),

    ok.
    

