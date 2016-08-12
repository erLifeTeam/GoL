-module(cache).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([send_to_persister/4, print_stats/1]).

start_link({MaxX, MaxY, StartValues}) ->
    gen_server:start_link({global, cache}, ?MODULE, {MaxX, MaxY, StartValues}, []).

init({MaxX, MaxY, StartValues}) ->
    Values = [ {0, Value}  || Rows <- StartValues, Value <- Rows],
    Coords = [ {X, Y} || X <- lists:seq(0, MaxX), Y <- lists:seq(0, MaxY)],
    NewState = maps:from_list(lists:zip(Coords, Values)),
    % key - {X, Y, Gen} - value - value
    stats_request(),
    {ok, NewState}.

handle_call({get_state, X, Y}, _From, State) ->
    Answer = maps:get({X, Y}, State), %Answer in format {Generation, Value}
    {reply, Answer, State};

handle_call(show_state, _From, State) ->
    {reply, State, State};

handle_call(_Req, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({update_state, X, Y, Gen, Value}, State) ->
    NewState = maps:update({X, Y}, {Gen, Value}, State),
    spawn(?MODULE, send_to_persister, [X, Y, Gen, Value]),
    {noreply, NewState};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(stats_request, State) ->
    spawn(?MODULE, print_stats, [State]),
    stats_request(),
    {noreply, State};

handle_info(_Request, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.


%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%

stats_request() ->
    timer:send_after(10000, stats_request).

print_stats(State) ->
    PredAlive = fun (K, {Gen, V}) -> V == 1 end,
    PredDead = fun (K, {Gen, V}) -> V == 0 end,
    AliveCount = maps:size(maps:filter(PredAlive, State)),
    DeadCount = maps:size(maps:filter(PredDead, State)),

    [
     lager:info("State ~p", [X])
     || {_, X} <- maps:to_list(State) ],
    lager:info("Dead cells ~p", [DeadCount]),
    lager:info("Alive cells ~p", [AliveCount]),
    lager:info("Total cells ~p", [AliveCount+DeadCount]).


send_to_persister(X, Y, Gen, Value) ->
    % PidOfGen = gen_server:call({global, persister}, {get_pid, Gen}),
    % PidOfRow = gen_server:call(PidOfGen, {get_pid, Y}),
    % gen_server_cast(PidOfRow, {save_state, X, Value}),
    % PidOfGen = gen_server:call({global, persister}, {get_pid, Gen}),
    ok.
