-module(main_indexer).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2]).

start_link({Y}) ->
	gen_server:start_link({global,main_indexer},?MODULE,{Y},[]).
 
init({Y}) ->
	NewState = maps:from_list(
		     [ {N,unregistered} || N <- lists:seq(0,Y) ]
		    ),
	{ok,NewState}.

handle_call({get_pid,X,Y},_From,State) ->
	Reply = case maps:get(Y,State,{error,badkey}) of
			{error,_} -> {error,badkey};
			unregistered -> {error,no_row_sup};
			Pid -> gen_server:call(Pid,{get_pid,X})

		end,
	{reply,Reply,State};

handle_call({register_row,Y},From,State) ->
	{Pid,_} = From,
	NewState = maps:update(Y,Pid,State),
	{reply,{ok,indexer_updated},NewState};

handle_call({register_cell,X,Y},From,State) ->
	{CellPid,_} = From,
	RowIndexerPid = maps:get(Y,State),
	Answer = gen_server:call(RowIndexerPid,{register_cell,CellPid,X}),
	{reply,Answer,State};

handle_call({unregister_cell,X,Y},_From,State) ->
	RowIndexerPid = maps:get(Y,State),
	Answer = gen_server:call(RowIndexerPid,{unregister_cell,X}),
	{reply,Answer,State};

handle_call(show_state,_From,State) ->
	{reply,State,State};

handle_call(_Request,_From,State) ->
	{reply,{error,unknown_request},State}.

handle_cast(_Request,State) ->
	{noreply,State}.

