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
     init_case,
     handle_call_unknown_command_case,
     handle_call_start_agent_running_case,
     handle_call_start_agent_present_case,
     handle_call_start_agent_new_case,
     handle_cast_case,
     start_agent4_case,
     start_agent5_case].


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


init_case(_Config) ->
    {ok, #state{agents=[]}} = manager:init([]),

    ok.


handle_call_unknown_command_case(_Config) ->
    %% fake_command
    {reply, ok, fake_state} = manager:handle_call(fake_command, from, fake_state),

    ok.


handle_call_start_agent_running_case(_Config) ->
    %% start_agent
    %% Agent present in the state and running
    Agent = agent,
    Module = agent,
    Function = start_link,
    Arguments = [],
    State = #state{agents=[#agent{name=Agent,
                                     state=running}]},
    {reply, {error, still_running}, State} = manager:handle_call({start_agent,
                                                                  Agent,
                                                                  Module,
                                                                  Function,
                                                                  Arguments},
                                                                 from, State),
    
    ok.
    

handle_call_start_agent_present_case(_Config) ->
    %% start_agent
    %% Agent present in the state but not running
    process_flag(trap_exit, true),
    Agent = agent,
    Module = agent,
    Function = start_link,
    Arguments = [],
    OldState = #state{agents=[#agent{name=Agent,
                                    state=terminated}]},
    {ok, SupPid} = agents_sup:start_link(),

    {reply, Ret, #state{agents=[#agent{name=Agent,
                                       pid=AgentPid,
                                       module=Module,
                                       function=Function,
                                       arguments=Arguments,
                                       state=running}]}
    } = manager:handle_call({start_agent, Agent,
                             Module, Function,Arguments},
                            from, OldState),
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
    Module = agent,
    Function = start_link,
    Arguments = [],
    OldState = #state{agents=[anything]},
    NewState = #state{agents=[#agent{name=Agent,
                                     module=Module,
                                     function=Function,
                                     arguments=Arguments,
                                     state=running}|[anything]]},
    {ok, SupPid} = agents_sup:start_link(),
    {reply, Ret, #state{agents=[#agent{name=Agent,
                                       pid=AgentPid,
                                       module=Module,
                                       function=Function,
                                       arguments=Arguments,
                                       state=running}|[anything]]}
    } = manager:handle_call({start_agent, Agent,
                             Module, Function,Arguments},
                            from, OldState),
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

    
handle_cast_case(_Config) ->
    {noreply, fake_state} = agent:handle_cast(fake_msg, fake_state),

    ok.


start_agent4_case(_Config) ->
    Agent = agent,
    process_flag(trap_exit, true),
    {ok, ManPid} = manager:start_link(),
    {ok, SupPid} = agents_sup:start_link(),
    Ret = manager:start_agent(Agent, agent, start_link, []),
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
    Ret = manager:start_agent(node(), Agent, agent, start_link, []),
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


