-module(erlmachine_network_model_http_SUITE).

-export([suite/0]).

-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_group/2, end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([groups/0]).

-export([all/0]).

-export([get/1, post/1, put/1, patch/1, delete/1]).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

suite() ->
    [{timetrap,{seconds,10}}].

init_per_suite(Config) ->
    application:ensure_all_started(erlmachine),
    application:ensure_all_started(gun),

    Path = erlmachine_network:filename("datasheets/http_ct.yaml"),
    {ok, T} = erlmachine_graph:template(Path),

    {ok, Pid} = erlmachine_network_ct:start(T), true = is_pid(Pid),

    Setup = [],
    lists:concat([Setup, Config]).

end_per_suite(Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: groups() -> [Group]
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
%% Description: Returns a list of test case group definitions.
%%--------------------------------------------------------------------
groups() ->
    [{'api', [parallel], [get, post, put, patch, delete]}].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%%
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%%   Name of a test case group.
%% TestCase = atom()
%%   Name of a test case.
%% Reason = term()
%%   The reason for skipping all groups and test cases.
%%
%% Description: Returns the list of groups and test cases that
%%              are to be executed.
%%--------------------------------------------------------------------
all() ->
    [{group, 'api'}].

%%--------------------------------------------------------------------
%% Function: TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%% Comment = term()
%%   A comment about the test case that will be printed in the html log.
%%
%% Description: Test case function. (The name of it must be specified in
%%              the all/0 list or in a test case group for the test case
%%              to be executed).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

get(_Config) ->
    Command = erlmachine:command('get', #{ path => "/get", query => [{<<"foo">>, <<"bar1">>}]}),

    Res = erlmachine_network_ct:call(Command, _Vertex = <<"http">>), true = is_map(Res),
    ok = assert(Res).

post(_Config) ->
    Command = erlmachine:command('post', #{ path => "/post", body => <<"payload">> }),

    Res = erlmachine_network_ct:call(Command, _Vertex = <<"http">>), true = is_map(Res),
    ok = assert(Res).

put(_Config) ->
    Command = erlmachine:command('put', #{ path => "/put", body => <<"payload">> }),

    Res = erlmachine_network_ct:call(Command, _Vertex = <<"http">>), true = is_map(Res),
    ok = assert(Res).

patch(_Config) ->
    Command = erlmachine:command('patch', #{ path => "/patch", body => <<"payload">> }),

    Res = erlmachine_network_ct:call(Command, _Vertex = <<"http">>), true = is_map(Res),
    ok = assert(Res).

delete(_Config) ->
    Command = erlmachine:command('delete', #{ path => "/delete", body => <<"payload">> }),

    Res = erlmachine_network_ct:call(Command, _Vertex = <<"http">>), true = is_map(Res),
    ok = assert(Res).

-spec assert(Res::term()) -> ok.
assert(Res) ->
    Body = erlmachine:body(Res), true = is_binary(Body),

    Header = erlmachine:header(Res),
    Status = maps:get(status, Header), Status = 200,
    ok.

