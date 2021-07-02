-module(erlmachine_network_ct).

-behaviour(gen_server).

%% API.
-export([start/1, stop/0]).
-export([call/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).

-include_lib("erlmachine/include/erlmachine_factory.hrl").
-include_lib("erlmachine/include/erlmachine_assembly.hrl").
-include_lib("erlmachine/include/erlmachine_graph.hrl").
-include_lib("erlmachine/include/erlmachine_system.hrl").

-type datasheet() :: erlmachine_datasheet:datasheet().

id() ->
    ?MODULE.

-spec start(Datasheet::datasheet()) -> success(pid()) | ingnore | failure(term()).
start(Datasheet) ->
    gen_server:start({local, id()}, ?MODULE, Datasheet, []).

-record(call, { command::term(), vertex::vertex() }).

-spec call(Command::term(), Vertex::vertex()) ->
                  success(map() | [map()]) | failure(term(), term()).
call(Command, Vertex) ->
    gen_server:call(id(), #call{ command=Command, vertex=Vertex }).

-spec stop() -> success().
stop() ->
    gen_server:stop(id()).

%%%===================================================================
%%% gen_server behaviour
%%%===================================================================

-record(state, { graph::graph(), root::vertex() }).

init(Datasheet) ->
    Graph = erlmachine_factory:graph(Datasheet),

    {ok, Pid} = erlmachine:startup(Graph), true = is_pid(Pid),

    {ok, #state{ graph = Graph }}.

handle_call(#call{ command=Command, vertex=Vertex }, From, #state{ graph=Graph }=State) ->

    erlmachine:process(Graph, Vertex, erlmachine:request(Command, From)),
    {noreply, State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
    ok.
