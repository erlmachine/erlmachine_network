-module(erlmachine_network).

-export([host/1, port/1]).
-export([path/1]).

-type host() :: list().
-type path() :: list().

%%% Options
-spec path(Opt::[term()]) -> path().
path(Opt) ->
    Path = proplists:get_value(<<"path">>, Opt, <<"/">>),
    binary_to_list(Path).

%%% Env
-spec host(Env::map()) -> host().
host(Env) ->
    Host = maps:get(<<"host">>, Env, <<"localhost">>), true = is_binary(Host),
    binary_to_list(Host).

-spec port(Env::map()) -> integer().
port(Env) ->
    Port = maps:get(<<"port">>, Env, 80), true = is_integer(Port),
    Port.

