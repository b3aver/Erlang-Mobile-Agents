%%%-------------------------------------------------------------------
%%% @author Paolo C Sberna <reliablebeaver86-cs@yahoo.it>
%%% @copyright (C) 2012, Paolo C Sberna
%%% @doc
%%%
%%% @end
%%% Created : 24 Jul 2012 by Paolo C Sberna <reliablebeaver86-cs@yahoo.it>
%%%-------------------------------------------------------------------
-module(whitepages_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-include("../src/whitepages.hrl").

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
init_per_testcase(TestCase, Config) ->
    case lists:any(fun(E) -> E == TestCase end, api_testcases()) of
        true ->
            process_flag(trap_exit, true),
            {ok, _WhPid} = whitepages:start_link(),    
            
            ok;
        false ->
            ok
    end,
    
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
end_per_testcase(TestCase, _Config) ->
    case lists:any(fun(E) -> E == TestCase end, api_testcases()) of
        true ->
            WhPid = whereis(whitepages),
            exit(WhPid, kill),
            receive
                {'EXIT', WhPid, killed } ->
                    ok
            after
                1000 -> exit(shutdown_error)
            end,

            ok;
        false ->
            ok
    end,
    
    
    

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
     handle_call_register_present_case,
     handle_call_register_new_case,
     handle_call_containers_case,
     handle_call_lookup_container_case,
     handle_cast_case,
     register2_case,
     register3_case,
     containers0_case,
     containers1_case,
     lookup_container1_case,
     lookup_container2_case].

api_testcases() ->
    [register2_case,
     register3_case,
     containers0_case,
     containers1_case,
     lookup_container1_case,
     lookup_container2_case].


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
    {ok, Pid} = whitepages:start_link(),
    Pid = whereis(whitepages),
    true = erlang:is_process_alive(whereis(whitepages)),

    %% stop the process
    exit(Pid, kill),
    receive 
        {'EXIT', Pid, killed} ->
            undefined = whereis(whitepages)
    after
        1000 -> exit(shutdown_error)
    end,

    ok.


init_case(_Config) ->
    {ok, #state{containers=[]}} = whitepages:init([]),

    ok.


handle_call_unknown_command_case(_Config) ->
    %% fake_command
    {reply, ok, fake_state} = whitepages:handle_call(fake_command, from,
                                                     fake_state),

    ok.


handle_call_register_present_case(_Config) ->
    %% register
    %% Container already present in the state
    Container = container,
    Node = node,
    State = #state{containers=[#container{name=Container, node=Node}]},

    {reply, {error, already_present}, State} = whitepages:handle_call({register,
                                                                       Container,
                                                                       Node},
                                                                      from, State),
    
    ok.


handle_call_register_new_case(_Config) ->
    %% register
    Container = container,
    Node = node,
    OldState = #state{containers=[#container{name=container2, node=node2}]},
    NewState = #state{containers=[#container{name=Container, node=Node}
                                  |[#container{name=container2, node=node2}]]},

    {reply, ok, NewState} = whitepages:handle_call({register, Container, Node},
                                                   from, OldState),

    ok.


handle_call_containers_case(_Config) ->
    ContList = [hello, world],
    State = #state{containers=ContList},
    {reply, ContList, State} = whitepages:handle_call(containers, from, State),

    ok.


handle_call_lookup_container_case(_Config) ->
    Container1 = #container{name=container1, node=node1},
    Container2 = #container{name=container2, node=node2},
    ContList = [Container1, Container2],
    State = #state{containers=ContList},

    {reply, Container1, State} = whitepages:handle_call({lookup_container, node1},
                                                        from,
                                                        State),
    
    {reply, Container2, State} = whitepages:handle_call({lookup_container, node2},
                                                        from,
                                                        State),

    {reply, {error, not_found}, State} = whitepages:handle_call({lookup_container,
                                                                 node3},
                                                                from,
                                                                State),
    
    ok.
    
    
handle_cast_case(_Config) ->
    {noreply, fake_state} = whitepages:handle_cast(fake_msg, fake_state),

    ok.


register2_case(_Config) ->
    Container = container,
    Node = node,
    
    case whitepages:register(Container, Node) of
        ok ->
            ok;
        already_present ->
            exit(register_error)
    end,
    
    case whitepages:register(Container, Node) of
        ok ->
            exit(register_error);
        {error, already_present} ->
            ok
    end,
    
    ok.


register3_case(_Config) ->
    Container = container,
    Node = node,
    
    case whitepages:register(node(), Container, Node) of
        ok ->
            ok;
        already_present ->
            exit(register_error)
    end,
    
    case whitepages:register(node(), Container, Node) of
        ok ->
            exit(register_error);
        {error, already_present} ->
            ok
    end,
    
    ok.


containers0_case(_Config) ->
    Container = container,
    Node = node,

    case whitepages:register(Container, Node) of
        ok ->
            ok;
        already_present ->
            exit(register_error)
    end,
    
    case whitepages:containers() of
        [#container{name=Container, node=Node}] ->
            ok;
        _ ->
            exit(containers_error)
    end,

    ok.


containers1_case(_Config) ->
    Container = container,
    Node = node,

    case whitepages:register(node(), Container, Node) of
        ok ->
            ok;
        already_present ->
            exit(register_error)
    end,
    
    case whitepages:containers(node()) of
        [#container{name=Container, node=Node}] ->
            ok;
        _ ->
            exit(containers_error)
    end,

    ok.


lookup_container1_case(_Config) ->
    Container = container,
    Node = node,
    case whitepages:register(Container, Node) of
        ok ->
            ok;
        already_present ->
            exit(register_error)
    end,
    
    case whitepages:lookup_container(Node) of
        #container{name=Container, node=Node} ->
            ok;
        _ ->
            exit(lookup_container_error)
    end,
    case whitepages:lookup_container(node1) of
        {error, not_found} ->
            ok;
        _ ->
            exit(lookup_container_error)
    end,

    ok.


lookup_container2_case(_Config) ->
    Container = container,
    Node = node,

    case whitepages:register(node(), Container, Node) of
        ok ->
            ok;
        already_present ->
            exit(register_error)
    end,
    case whitepages:containers(node()) of
        [#container{name=Container, node=Node}] ->
            ok;
        _ ->
            exit(containers_error)
    end,

    
    case whitepages:lookup_container(node(), Node) of
        #container{name=Container, node=Node} ->
            ok;
        _ ->
            exit(lookup_container_error)
    end,
    
    case whitepages:lookup_container(node(), node1) of
        {error, not_found} ->
            ok;
        _ ->
            exit(lookup_container_error)
    end,

    ok.
    
