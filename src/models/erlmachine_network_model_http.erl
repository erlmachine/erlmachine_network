-module(erlmachine_network_model_http).
%% NOTE: https://ninenines.eu/docs/en/gun/2.0/guide/

-behaviour(erlmachine_worker_model).

-export([startup/4]).

-export([process/3, execute/3]).
-export([pressure/3]).

-export([shutdown/3]).

-include_lib("erlmachine/include/erlmachine_user.hrl").
-include_lib("erlmachine/include/erlmachine_system.hrl").

-record (stream, {
                  ref::reference(),

                  headers = []::[tuple()], body = <<>>::binary(),
                  status::integer(),

                  motion::term()
                }).

-type stream() :: #stream{}.

-type state() :: map().

-spec startup(UID::uid(), State::state(), Opt::map(), Env::map()) ->
                  success(state()).
startup(_UID, State, Opt, Env) ->
    Host = erlmachine_network:host(Env),
    Port = erlmachine_network:port(Env),

    T = erlmachine_network:transport(Opt),
    Trace = erlmachine_network:trace(Opt),
    %Protocols = protocols(Opt),

    {ok, Pid} = gun:open(Host, Port, #{
                                       'transport' => T, 'protocols' => [http],
                                       'trace' => Trace
                                      }),
    {ok, _} = gun:await_up(Pid),

    Tid = ets:new(?MODULE, [{'keypos', #stream.ref}, {'write_concurrency', true}, {'read_concurrency', true}]),

    erlmachine:success(State#{
                              'pid' => Pid, 'tid' => Tid,

                              'host' => Host,
                              'port' => Port
                             }).

-spec process(UID::uid(), Motion::term(), State::state()) ->
                     success(state()) | failure(term(), term(), state()).
process(_UID, Motion, State) ->
    Pid = maps:get(pid, State),
    Tid = maps:get(tid, State),

    Command = erlmachine:command_name(Motion),
    Args = erlmachine:body(Motion),

    Path = path(Args), Headers = headers(Args),
    Body = body(Args),

    try
        Ref = case Command of
                  'get' ->
                      gun:get(Pid, Path, Headers);
                  'post' ->
                      gun:post(Pid, Path, Headers, Body);
                  'head' ->
                      gun:head(Pid, Path, Headers);
                  'put' ->
                      gun:put(Pid, Path, Headers, Body);
                  'delete' ->
                      gun:delete(Pid, Path, Headers);
                  'patch' ->
                      gun:patch(Pid, Path, Headers, Body);
                  'options' ->
                      gun:options(Pid, Path, Headers)
              end,
        true = insert(Tid, Ref, Motion),

        erlmachine:success(State)
    catch E:R ->
            erlmachine:failure(E, R, State)
    end.

-spec execute(UID::uid(), Action::term(), State::state()) ->
                     success(term(), state()) | failure(term(), term(), state()).
execute(_UID, Action, State) ->
    Command = erlmachine:command_name(Action),
    _Args = erlmachine:body(Action),

    try Command of
        'info' ->
            Pid = maps:get(pid, State), Res = gun:info(Pid),

            erlmachine:success(Res, State)
    catch E:R ->
            erlmachine:failure(E, R, State)
    end.

%% TODO: Optional table monitoring;
-spec pressure(UID::uid(), Load::term(), State::state()) ->
                      success(state()) | success(term(), state()).
pressure(_UID, {gun_response, Pid, Ref, IsFin, Status, Headers}, State) ->
    Pid = maps:get(pid, State),
    Tid = maps:get(tid, State),

    case IsFin of
        fin ->
            true = delete(Tid, Ref);
        nofin ->
            true = update_element(Tid, Ref, [{#stream.status, Status}, {#stream.headers, Headers}])
    end,
    erlmachine:success(State);

pressure(_UID, {gun_data, Pid, Ref, IsFin, Data}, State) ->
    Pid = maps:get(pid, State),
    Tid = maps:get(tid, State),

    [Stream] = lookup(Tid, Ref),

    Body = Stream#stream.body, Body2 = <<Body/binary, Data/binary>>,
    case IsFin of
        fin ->
            true = delete(Tid, Ref),

            Motion = Stream#stream.motion,
            Args = erlmachine:body(Motion), Path = maps:get(path, Args),

            Headers = Stream#stream.headers,

            Status = Stream#stream.status,
            Header = #{
                       status => Status,
                       path => Path, headers => Headers
                      },

            Doc = erlmachine:document(Header, _Meta = Path, Body2),
            Reply = erlmachine:reply(Motion, Doc),

            erlmachine:success(Reply, State);
        nofin ->
            true = update_element(Tid, Ref, [{#stream.body, Body2}]),

            erlmachine:success(State)
    end;

pressure(_UID, {gun_up, _Pid, _Proto}, State) ->
    %% TODO: Logging;
    erlmachine:success(State);

pressure(_UID, {gun_down, _Pid, _Proto, _Reason, _Streams}, State) ->
    %% TODO: Logging;
    erlmachine:success(State);

pressure(_UID, {gun_tunnel_up, _Pid, _Ref, _Proto}, State) ->
    %% TODO: Logging;
    erlmachine:success(State);

pressure(_UID, {gun_error, _Pid, _Ref, Reason}, State) ->
    %% TODO: Logging;
    erlmachine:failure(Reason, State);

pressure(_UID, {gun_push, _Pid, _Ref, _NewRef, _Method, _URI, _Headers}, State) ->
    %% TODO: Logging;
    erlmachine:success(State);

pressure(_UID, {gun_inform, _Pid, _Ref, _Status, _Headers}, State) ->
    %% TODO: Logging;
    erlmachine:success(State);

pressure(_UID, {gun_trailers, _Pid, _Ref, _Headers}, State) ->
    %% TODO: Logging;
    erlmachine:success(State);

pressure(_UID, _Load, State) ->
    %% TODO: Logging;
    erlmachine:success(State).

-spec shutdown(UID::uid(), Reason::term(), State::state()) ->
                      success().
shutdown(_UID, _Reason, State) ->
    Tid = maps:get(tid, State), true = ets:delete(Tid),

    erlmachine:success().

%%% Utils

-spec path(Args::map()) -> list().
path(Args) ->
    Path = maps:get(path, Args), true = is_list(Path),
    Query = maps:get(query, Args, []), true = is_list(Query),

    if Query == [] ->
            Path;
        true ->
            Path2 = lists:flatten([Path, "?", cow_qs:qs(Query)]),
            Path2
    end.

-spec headers(Args::map()) -> [term()].
headers(Args) ->
    Headers = maps:get(headers, Args, []), true = is_list(Headers),
    Headers.

-spec body(Args::map()) -> binary().
body(Args) ->
    Body = maps:get(body, Args, <<>>), true = is_binary(Body),
    Body.

%%% Table API

-spec insert(Tid::term(), Ref::reference(), Motion::term()) ->
                    true.
insert(Tid, Ref, Motion) ->
    Stream = #stream{ ref = Ref, motion = Motion },

    ets:insert(Tid, Stream).

-spec delete(Tid::term(), Ref::reference()) ->
                    true.
delete(Tid, Ref) ->
    ets:delete(Tid, Ref).

-spec update_element(Tid::term(), Ref::reference(), ElementSpec::[tuple()]) ->
                            boolean().
update_element(Tid, Ref, ElementSpec) ->
    ets:update_element(Tid, Ref, ElementSpec).

-spec lookup(Tid::term(), Ref::reference()) ->
                    [stream()].
lookup(Tid, Ref) ->
    ets:lookup(Tid, Ref).
