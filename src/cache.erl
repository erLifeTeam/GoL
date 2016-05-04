-module(cache).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-export([send_to_persister/4]).

start_link({MaxX,MaxY,StartValues}) ->
	gen_server:start_link({global,cache},?MODULE,{MaxX,MaxY,StartValues},[]).

init({MaxX,MaxY,StartValues}) ->
	Values = [ {0,Value}  || Rows <- StartValues, Value <- Rows],
	Coords = [ {X,Y} || X <- lists:seq(0,MaxX), Y <- lists:seq(0,MaxY)],
	NewState = maps:from_list(lists:zip(Coords,Values)),
	% key - {X,Y,Gen} - value - value
	{ok,NewState}.

handle_call({get_state,X,Y},_From,State) ->
	Answer = maps:get({X,Y},State), %Answer in format {Generation,Value}
	{reply,Answer,State};

handle_call(show_state,_From,State) ->
	{reply,State,State};

handle_call(_Req,_From,State) ->
	{reply,{error,unknown_call},State}.

handle_cast({update_state,X,Y,Gen,Value},State) ->
	NewState = maps:update({X,Y},{Gen,Value},State),
	spawn(?MODULE,send_to_persister,[X,Y,Gen,Value]),
	{noreply,NewState};

handle_cast(_Request,State) ->
	{noreply,State}.

terminate(_Reason,_State) ->
	ok.

send_to_persister(X,Y,Gen,Value) ->
	% PidOfGen = gen_server:call({global,persister},{get_pid,Gen}),
	% PidOfRow = gen_server:call(PidOfGen,{get_pid,Y}),
	% gen_server_cast(PidOfRow,{save_state,X,Value}),
	ok.
