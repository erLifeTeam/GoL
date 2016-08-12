-module(row_indexer).
-behaviour(gen_server).

-export([start_link/1, register_cell_pid/3, unregister_cell_pid/2, get_cell_pid/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {map,
                my_row}).
%%%
% External functions
%%%

start_link({X, Y}) ->
	gen_server:start_link(?MODULE, {X, Y}, []).

register_cell_pid(RowIndexerPid, CellPid, X) ->
	gen_server:call(RowIndexerPid, {register_cell, CellPid, X}).

unregister_cell_pid(RowIndexerPid, X) ->
    gen_server:call(RowIndexerPid, {unregister_cell, X}).

get_cell_pid(RowIndexerPid, X) ->
    gen_server:call(RowIndexerPid, {get_pid, X}).

%%%
% Callback functions
%%%

% We do not perform initialization here, as supervisor waits for 
% return from this functions. This means, all row indexers are
% started one by one. If we initialize later, we can do it parllelly
% on several row_indexers => faster startup.
init({X, Y}) ->
    self() ! {initialize, X, Y}, 
    {ok, []}.

handle_call({get_pid, X}, _From, State = #state{map = Map}) ->
    Reply = case maps:get(X, Map, {error, badkey}) of
                {error, _} -> {error, undef};
                Value -> {ok, Value}
		end,
	{reply, Reply, State};

handle_call({register_cell, CellPid, X}, _From,
            State = #state{map = Map}) ->
	NewMap = maps:put(X, CellPid, Map),
	{reply, {ok, cell_updated}, State#state{map = NewMap}};

handle_call({unregister_cell, X}, _From,
            State = #state{map = Map}) ->
	NewMap = maps:remove(X, Map),
	{reply, {ok, cell_unregistered}, State#state{map = NewMap}};

handle_call(show_state, _From, State) ->
	{reply, State, State};

handle_call(_Request, _From, State) ->
	{reply, {error, unknown_request}, State}.

handle_cast(_Request,State) ->
	{noreply, State}.

handle_info({initialize, _X, Y}, []) ->
	NewState = #state{map = #{},
                      my_row = Y},
    {ok, indexer_updated} = main_indexer:register_row(self(), Y),
    {noreply, NewState};

handle_info(_Request, State) ->
	{noreply, State}.

code_change(_OldSvn, State, _Extra) ->
	{ok, State}.

terminate(_Reason,_State) ->
	ok.

