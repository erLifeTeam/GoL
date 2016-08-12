-module(main_indexer).
-behaviour(gen_server).

-export([start_link/1, get_row_indexer_pid/1, register_cell_pid/3, unregister_cell_pid/2,
         register_row/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

%%%
% External functions
%%%

start_link({Y}) ->
	gen_server:start_link({global, ?MODULE}, ?MODULE, {Y}, []).

get_row_indexer_pid(Y) ->
    gen_server:call({global, ?MODULE}, {get_row_indexer_pid, Y}).

register_cell_pid(PID, X, Y) ->
    gen_server:call({global, ?MODULE}, {register_cell, PID, X, Y}).

unregister_cell_pid(X, Y) ->
    gen_server:call({global, ?MODULE}, {unregister_cell, X, Y}).

register_row(Pid, Y) ->
	gen_server:call({global, ?MODULE}, {register_row, Pid, Y}).

%%%
% Callback functions
%%%

init({Y}) ->
	NewState = maps:from_list(
		     [ {N, unregistered} || N <- lists:seq(0, Y) ]),
	{ok, NewState}.

handle_call({get_row_indexer_pid, Y}, _From, State) ->
	Reply = maps:get(Y, State, {error, badkey}),
	{reply, Reply, State};

handle_call({register_row, Pid, Y}, _From, State) ->
	NewState = maps:update(Y, Pid, State),
	{reply, {ok, indexer_updated}, NewState};

handle_call({register_cell, CellPid, X, Y}, _From, State) ->
	RowIndexerPid = maps:get(Y, State),
	Answer =  row_indexer:register_cell_pid(RowIndexerPid, CellPid, X),
	{reply, Answer, State};

handle_call({unregister_cell, X, Y}, _From, State) ->
	RowIndexerPid = maps:get(Y, State),
	Answer = row_indexer:unregister_cell_pid(RowIndexerPid, X),
	{reply, Answer, State};

handle_call(show_state, _From, State) ->
	{reply, State, State};

handle_call(_Request, _From, State) ->
	{reply, {error, unknown_request}, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info(_Request, State) ->
	{noreply, State}.

code_change(_OldSvn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, _State) ->
	ok.
