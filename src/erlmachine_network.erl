-module(erlmachine_network).

-export([priv_dir/0, filename/1]).

-export([transport/1]).

-export([host/1, port/1]).
-export([path/1]).

-export([debug/1]).

-include_lib("erlmachine/include/erlmachine_system.hrl").

-type host() :: list().
-type path() :: list().

%%% Library API

-spec priv_dir() -> file:filename().
priv_dir() ->
    code:priv_dir(?MODULE).

-spec filename(Path::list()) -> list().
filename(Path) ->
    filename:join(priv_dir(), Path).

%%% Opt

-spec transport(Opt::map()) -> tcp | tls.
transport(Opt) ->
    Transport = maps:get(<<"transport">>, Opt, <<"tcp">>), true = is_binary(Transport),

    binary_to_atom(Transport).

%%% Env
-spec path(Env::map()) -> path().
path(Env) ->
    Path = maps:get(<<"path">>, Env, <<"/">>), true = is_binary(Path),
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

-spec debug(Env::map()) -> boolean().
debug(Env) ->
    Debug = maps:get(<<"debug">>, Env, false), true = is_boolean(Debug),
    Debug.
