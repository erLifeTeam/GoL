-module(row_indexer).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2]).

start_link({X,Y}) ->
	gen_server:start_link(?MODULE,{X,Y},[]).
 
init({X,Y}) ->
	NewState = maps:from_list(
		     [ {N,unregistered} || N <- lists:seq(0,X) ]
		    ),
	gen_server:call({global,main_indexer},{register_row,Y}),
	{ok,NewState}.

handle_call({get_pid,X},_From,State) ->
	Reply = case maps:get(X,State,{error,badkey}) of
			{error,_} -> {error,badkey};
			unregistered -> {error,unregistered};
			Value -> {ok,Value}

		end,
	{reply,Reply,State};

handle_call({register_cell,CellPid,X},_From,State) ->
	NewState = maps:update(X,CellPid,State),
	{reply,{ok,cell_updated},NewState};

handle_call({unregister_cell,X},_From,State) ->
	NewState = maps:update(X,unregisterd,State),
	{reply,{ok,cell_unregistered},NewState};

handle_call(show_state,_From,State) ->
	{reply,State,State};

handle_call(_Request,_From,State) ->
	{reply,{error,unknown_request},State}.

handle_cast(_Request,State) ->
	{noreply,State}.

terminate(_Reason,_State) ->
	ok.


