-module(erlmachine_network_model_udp).
%% TODO: To implement tcp, etc.
-behaviour(erlmachine_worker_model).

-export([startup/4]).
-export([process/3, execute/3, pressure/3]).
-export([shutdown/3]).

-include_lib("erlmachine/include/erlmachine_user.hrl").
-include_lib("erlmachine/include/erlmachine_system.hrl").

-type state() :: map().

-spec startup(UID::uid(), State::state(), Opt::[term()], Env::map()) ->
                  success(state()).
startup(UID, State, Opt, Env) ->
    %% TODO: To provide test cases parametrization through Env;
    io:format("~n~p:startup(~p, ~p, ~p, ~p)~n", [?MODULE, UID, State, Opt, Env]),

    {ok, Socket} = gen_udp:open(0, [binary]),

    Host = maps:get(<<"host">>, Env), true = is_binary(Host), Host2 = host(Host),
    Port = maps:get(<<"port">>, Env), true = is_integer(Port),

    erlmachine:success(State#{ socket => Socket, host => Host2, port => Port }).

-spec process(UID::uid(), Event::term(), State::state()) ->
                     success(state()) | failure(term(), term(), state()).
process(_UID, Event, State) ->
    Packet = erlmachine:body(Event),

    try
        true = is_binary(Packet),

        Socket = maps:get(socket, State),
        Host = maps:get(host, State), Port = maps:get(port, State),

        ok = gen_udp:send(Socket, Host, Port, Packet),

        erlmachine:success(State)
    catch E:R ->
            erlmachine:failure(E, R, State)
    end.

-spec execute(UID::uid(), Action::term(), State::state()) ->
                     success(term(), state()).
execute(UID, Action, State) ->
    io:format("~n~p:execute(~p, ~p, ~p)~n", [?MODULE, UID, Action, State]),

    erlmachine:success(ignore, State).

-spec pressure(UID::uid(), Load::term(), State::state()) ->
                      success(term(), state()).
pressure(_UID, {udp, _Pid, Ip, _Port, Packet}, State) ->
    Doc = erlmachine:document(Ip, Packet),

    erlmachine:success(Doc, State);

pressure(UID, Load, State) ->
    io:format("~n~p:pressure(~p, ~p, ~p)~n", [?MODULE, UID, Load, State]),

    erlmachine:success(State).

-spec shutdown(UID::uid(), Reason::term(), State::state()) ->
                      success().
shutdown(UID, Reason, State) ->
    io:format("~n~p:shutdown(~p, ~p, ~p)~n", [?MODULE, UID, State, Reason]),
    Socket = maps:get(socket, State), ok = gen_udp:close(Socket),

    erlmachine:success().


%%% utils

-spec host(Host::binary()) -> list().
host(Host) ->
    binary_to_list(Host).
