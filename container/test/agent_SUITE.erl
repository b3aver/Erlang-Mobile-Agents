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
    [init_case,
     handle_call_case,
     handle_cast_unknown_command_case,
     handle_cast_stop_case,
     handle_info_exit_case,
     start_link_case,
     reactivate_case,
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


init_case(_Config) ->
    Name = agent1,
    Module = tester_agent,
    Arguments = [10],
    State = [wait, 10],

    %% start case
    {ok, #state{name = Name, module=Module, arguments=Arguments, pid=Pid}} =
        agent:init([start, Name, Module, Arguments]),
    true = is_process_alive(Pid),

    %% reactivate case
    {ok, #state{name = Name, module=Module, pid=Pid2}} =
        agent:init([reactivate, Name, Module, State]),
    true = is_process_alive(Pid2),

    ok.
    

handle_call_case(_Config) ->
    Self = self(),
    {reply, Self, fake_state} = agent:handle_call(introduce, from, fake_state),
    {reply, ok, fake_state} = agent:handle_call(fake_command, from, fake_state),

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
start_link_case(_Config) -> 
    % check start
    Agent = agent,
    Module = tester_agent,
    Time_to_live = 1,
    Arguments = [Time_to_live],
    {ok, Pid} = agent:start_link(Agent, Module, Arguments),
    Pid = whereis(Agent),
    true = erlang:is_process_alive(whereis(Agent)),
    
    %% wait time to live
    receive after (1000*(Time_to_live+1)) -> nil end,
    undefined = whereis(Agent),
    false = erlang:is_process_alive(Pid),
   
    ok.


reactivate_case(_Config) -> 
    % check start
    Agent = agent,
    Module = tester_agent,
    Time_to_live = 1,
    State = [wait, Time_to_live],
    {ok, Pid} = agent:reactivate(Agent, Module, State),
    Pid = whereis(Agent),
    true = erlang:is_process_alive(whereis(Agent)),
    
    %% wait time to live
    receive after (1000*(Time_to_live+1)) -> nil end,
    undefined = whereis(Agent),
    false = erlang:is_process_alive(Pid),
   
    ok.


introduce_case(_Config) ->
    Agent = agent,
    Module = tester_agent,
    Arguments = [10],
    {ok, AgentPid} = agent:start_link(Agent, Module, Arguments),
    AgentPid = agent:introduce(Agent),
    AgentPid = agent:introduce({Agent, node()}),
    agent:stop(Agent),

    ok.

    
stop_case(_Config) ->
    % check stop
    Agent = agent,
    Module = tester_agent,
    Time_to_live = 10,
    Arguments = [Time_to_live],
    {ok, Pid} = agent:start_link(Agent, Module, Arguments),
    Pid = whereis(Agent),
    true = erlang:is_process_alive(whereis(Agent)),
      
    % stop the agent
    agent:stop(Agent),
    receive after 100 -> nil end,
    undefined = whereis(Agent),
    false = erlang:is_process_alive(Pid),

    ok.


migrate_case(_Config) ->
    %% migrate
    Node = node,
    NodeL = list_to_atom("node@"++net_adm:localhost()), 
    Agent = agent,

    Module = tester_agent,
    Arguments = [10],

    %% start the container application locally
    ok = application:start(container),
    %% start an agent locally
    {ok, AgentPid} = manager:start_agent(Agent, Module, Arguments),
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
    MigState = {wait, [10]},
    ok = agent:migrate(Agent, NodeL, MigState),
    receive after 100 -> ok end,
    undefined = whereis(Agent),
    false = is_process_alive(AgentPid),

    %% receive after 5000 -> ok end,     
    
    Children = supervisor:which_children({agents_sup, NodeL}),
    NewAgentPid = agent:introduce({Agent, NodeL}),
    true = is_pid(NewAgentPid),    
    {value, {Agent, NewAgentPid, worker, [agent, Module]}}
        = lists:keysearch(Agent, 1, Children),
    
    %% stop the container application
    ok = application:stop(container),
    ok = rpc:call(NodeL, application, stop, [container]),
    %% stop the container node
    ok = slave:stop(NodeL),

    ok.
    

