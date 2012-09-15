%%%-------------------------------------------------------------------
%%% @author Paolo C Sberna <reliablebeaver86-cs@yahoo.it>
%%% @copyright (C) 2012, Paolo C Sberna
%%% @doc
%%%
%%% @end
%%% Created : 19 Jul 2012 by Paolo C Sberna <reliablebeaver86-cs@yahoo.it>
%%%-------------------------------------------------------------------
-module(agents_sup_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

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
     start_stop_case,
     start_agent_case,
     host_agent_case].


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


%% ----------------
%% White box tests
%% ----------------
init_case(_Config) ->
    %% check init
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    ExpectedSupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    ExpectedChildSpecList = [],

    {ok, {ExpectedSupFlags, ExpectedChildSpecList}} = agents_sup:init([]),

    %% check ChildSpec
    ok = supervisor:check_childspecs(ExpectedChildSpecList),

    ok.


%% ----------------
%% Black box tests
%% ----------------
start_stop_case(_Config) ->
    process_flag(trap_exit, true),

    undefined = whereis(agents_sup),

    %% check start
    {ok, AgentsPid} = agents_sup:start_link(),
    receive after 100 -> nil end,
    AgentsPid = whereis(agents_sup),
    [] = supervisor:which_children(agents_sup),

    %% check stop
    exit(AgentsPid, shutdown),
    
    receive
        {'EXIT', _Pid, _Reason} ->
            false = erlang:is_process_alive(AgentsPid)
    after
        2000 -> exit(shutdown_error)
    end,

    ok.


start_agent_case(_Config) ->
    process_flag(trap_exit, true),
    Module = tester_agent,
    Arguments = [10],
    ModList = [agent, Module | Module:used_modules()],
    
    undefined = whereis(agents_sup),

    %% start supervisor
    {ok, AgentsPid} = agents_sup:start_link(),
    receive after 100 -> nil end,
    AgentsPid = whereis(agents_sup),
    [] = supervisor:which_children(agents_sup),

    % check start_agent
    agents_sup:start_agent(agent1, Module, Arguments),
    agents_sup:start_agent(agent2, Module, Arguments),

    Children = supervisor:which_children(agents_sup),
    {agent1, Agent1Pid, worker, ModList} = lists:keyfind(agent1, 1, Children),
    true = erlang:is_process_alive(Agent1Pid),
    {agent2, Agent2Pid, worker, ModList} = lists:keyfind(agent2, 1, Children),
    true = erlang:is_process_alive(Agent2Pid),

    % check stop of the agent
    gen_server:cast(agent1, stop),
    receive after 100 -> nil end, % wait a while
    % check if old process are dead
    false = erlang:is_process_alive(Agent1Pid),
    true = erlang:is_process_alive(Agent2Pid),
    % check if agents restart (the don't)
    Children2 = supervisor:which_children(agents_sup),
    false = lists:keyfind(agent1, 1, Children2),
    {agent2, Agent2Pid, worker, ModList} = lists:keyfind(agent2, 1, Children2),
    
    %% check stop of the agents when the supervisor is shutdown
    exit(AgentsPid, shutdown),
    receive
        {'EXIT', _Pid, _Reason} ->
            false = erlang:is_process_alive(AgentsPid),
            false = erlang:is_process_alive(Agent2Pid)
    after
        2000 -> exit(shutdown_error)
    end,

    ok.


host_agent_case(_Config) ->
    process_flag(trap_exit, true),
    Module = tester_agent,
    State = [wait, 10],
    ModList = [agent, Module | Module:used_modules()],
    
    undefined = whereis(agents_sup),

    %% start supervisor
    {ok, AgentsPid} = agents_sup:start_link(),
    receive after 100 -> nil end,
    AgentsPid = whereis(agents_sup),
    [] = supervisor:which_children(agents_sup),

    % check start_agent
    agents_sup:host_agent(agent1, Module, State),
    agents_sup:host_agent(agent2, Module, State),

    Children = supervisor:which_children(agents_sup),
    {agent1, Agent1Pid, worker, ModList} = lists:keyfind(agent1, 1, Children),
    true = erlang:is_process_alive(Agent1Pid),
    {agent2, Agent2Pid, worker, ModList} = lists:keyfind(agent2, 1, Children),
    true = erlang:is_process_alive(Agent2Pid),

    % check stop of the agent
    gen_server:cast(agent1, stop),
    receive after 100 -> nil end, % wait a while
    % check if old process are dead
    false = erlang:is_process_alive(Agent1Pid),
    true = erlang:is_process_alive(Agent2Pid),
    % check if agents restart (the don't)
    Children2 = supervisor:which_children(agents_sup),
    false = lists:keyfind(agent1, 1, Children2),
    {agent2, Agent2Pid, worker, ModList} = lists:keyfind(agent2, 1, Children2),
    
    %% check stop of the agents when the supervisor is shutdown
    exit(AgentsPid, shutdown),
    receive
        {'EXIT', _Pid, _Reason} ->
            false = erlang:is_process_alive(AgentsPid),
            false = erlang:is_process_alive(Agent2Pid)
    after
        2000 -> exit(shutdown_error)
    end,

    ok.
