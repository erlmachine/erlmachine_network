-module(erlmachine_network).

-export([priv_dir/0, filename/1]).

-export([transport/1]).
-export([retry/1]).
-export([retry_timeout/1]).
-export([trace/1]).

-export([host/1, port/1]).
-export([path/1]).

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
    Def = <<"tcp">>,

    T = maps:get(<<"transport">>, Opt, Def), true = is_binary(T),
    binary_to_atom(T).

-spec retry(Opt::map()) -> integer().
retry(Opt) ->
    Def = 25,

    Retry = maps:get(<<"retry">>, Opt, Def),
    Retry.

-spec retry_timeout(Opt::map()) -> integer().
retry_timeout(Opt) ->
    Def = 1000,

    Timeout = maps:get(<<"retry_timeout">>, Opt, Def),
    Timeout.

-spec trace(Opt::map()) -> boolean().
trace(Opt) ->
    Def = false,

    Trace = maps:get(<<"trace">>, Opt, Def),
    Trace.

%%% Env

-spec path(Env::map()) -> path().
path(Env) ->
    Def = <<"/">>,

    Path = maps:get(<<"path">>, Env, Def), true = is_binary(Path),
    binary_to_list(Path).

-spec host(Env::map()) -> host().
host(Env) ->
    Def = <<"localhost">>,

    Host = maps:get(<<"host">>, Env, Def), true = is_binary(Host),
    binary_to_list(Host).

-spec port(Env::map()) -> integer().
port(Env) ->
    Def = 80,

    Port = maps:get(<<"port">>, Env, Def), true = is_integer(Port),
    Port.
