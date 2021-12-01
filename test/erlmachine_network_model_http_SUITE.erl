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

    File = erlmachine_network:filename("datasheets/http_ct.yaml"),
    {ok, T} = erlmachine_graph:template(File),

    {ok, Pid} = erlmachine_network_ct:start(T), true = is_pid(Pid),
    Config.

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

groups() ->
    [{'api', [parallel], [get, post, put, patch, delete]}].

all() ->
    [{group, 'api'}].

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

