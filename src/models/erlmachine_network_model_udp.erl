-module(erlmachine_network_model_udp).
%% TODO Protocols: tcp, etc.

-behaviour(erlmachine_worker_model).

-export([startup/4]).
-export([process/3, execute/3, pressure/3]).
-export([shutdown/3]).

-include_lib("erlmachine/include/erlmachine_user.hrl").
-include_lib("erlmachine/include/erlmachine_system.hrl").

-type state() :: map().

-spec startup(UID::uid(), State::state(), Opt::map(), Env::map()) ->
                  success(state()).
startup(_UID, State, _Opt, Env) ->
    Host = maps:get(<<"host">>, Env), true = is_binary(Host), Host2 = host(Host),
    Port = maps:get(<<"port">>, Env), true = is_integer(Port),

    {ok, Socket} = gen_udp:open(0, [binary]),

    erlmachine:success(State#{
                              socket => Socket,

                              host => Host2,
                              port => Port
                             }).

-spec process(UID::uid(), Event::term(), State::state()) ->
                     success(state()) | failure(term(), term(), state()).
process(_UID, Event, State) ->
    Packet = erlmachine:body(Event),

    try
        true = is_binary(Packet),

        Socket = maps:get(socket, State),

        Host = maps:get(host, State),
        Port = maps:get(port, State),

        ok = gen_udp:send(Socket, Host, Port, Packet),
        erlmachine:success(State)
    catch E:R ->
            erlmachine:failure(E, R, State)
    end.

-spec execute(UID::uid(), Action::term(), State::state()) ->
                     success(term(), state()).
execute(_UID, _Action, State) ->
    erlmachine:success(ignore, State).

-spec pressure(UID::uid(), Load::term(), State::state()) ->
                      success(term(), state()).
pressure(_UID, {udp, _Pid, Ip, _Port, Packet}, State) ->
    Host = maps:get(host, State),
    Port = maps:get(port, State),

    Header = #{
               host => Host,
               port => Port
              },
    Doc = erlmachine:document(Header, Ip, Packet),
    erlmachine:success(Doc, State);

pressure(_UID, _Load, State) ->
    %% TODO: Logging;
    erlmachine:success(State).

-spec shutdown(UID::uid(), Reason::term(), State::state()) ->
                      success().
shutdown(_UID, _Reason, State) ->
    Socket = maps:get(socket, State), ok = gen_udp:close(Socket),

    erlmachine:success().

%%% utils

-spec host(Host::binary()) -> list().
host(Host) ->
    binary_to_list(Host).
