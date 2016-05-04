-module(cell).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-export([get_neightbours/2]).

-record (cell_state, {
	   value,
	   generation,
	   xcord,
	   ycord,
	   my_neightbours
	  }).
 
start_link({MaxX,MaxY,X,Y}) ->
	gen_server:start_link(?MODULE,{MaxX,MaxY,X,Y},[]).
 
init({MaxX,MaxY,X,Y}) ->
	{Gen,StartValue} = gen_server:call({global,cache},{get_state,X,Y}),
	NewState = #cell_state{value = #{Gen => StartValue}, % key - generation, value = value
			       generation = Gen,
				xcord = X,
				ycord = Y,
				my_neightbours = 
					[{X1,Y1,undefined} || 
					 X1 <- lists:seq(X-1,X+1),
					 Y1 <- lists:seq(Y-1,Y+1),
					 X1 >= 0, X1 =< MaxX,
					 (X1 =/= X) or (Y1 =/= Y), 
					 Y1 >= 0, Y1 =< MaxY  ]
%				my_neightbours= [ {X,Y+1,undefined},
%						  {X+1,Y+1,undefined},
%						  {X+1,Y,undefined},
%						  {X+1,Y-1,undefined},
%						  {X,Y-1,undefined},
%						  {X-1,Y-1,undefined},
%						  {X-1,Y,undefined},
%						  {X-1,Y+1,undefined}]
			       },

	gen_server:call({global,main_indexer},{register_cell,X,Y}),
	spawn(?MODULE,
	        get_neightbours,
		[StartValue,
		{	self(),
			Gen,
			NewState#cell_state.my_neightbours
		}]),
	{ok,NewState}.

handle_call({get_state,Generation},_From,State) ->
	{reply,{ok,maps:get(Generation,State#cell_state.value)},State};

handle_call(show_state,_From,State) ->
	{reply,State,State};

handle_call(_Request,_From,State) ->
	{reply,{error,unknown_request},State}.
	  
handle_cast({update_state,NewValue,NewNeightbours},State) ->

	Pred = fun(K,_V) -> %it is used to delete history older than 3 gens
			       (K - State#cell_state.generation) < 3
	       end,

	NewState =  State#cell_state{value=maps:put(
					     State#cell_state.generation+1,
					     NewValue,
					     maps:filter(Pred,State#cell_state.value)
					    ),
				     generation=State#cell_state.generation+1,
				     my_neightbours=NewNeightbours},
	gen_server:cast({global,cache},{update_state,
					State#cell_state.xcord,
					State#cell_state.ycord,
					State#cell_state.generation+1,
					NewValue
				       }),
	spawn(?MODULE,
		get_neightbours,
		[NewValue,
		{	self(),
			NewState#cell_state.generation,
			NewState#cell_state.my_neightbours
		}]),
	
	{noreply,NewState};

handle_cast(_Request,State) ->
	{noreply,State}.

terminate(_Reason,State) ->
	X = State#cell_state.xcord,
	Y = State#cell_state.ycord,
	gen_server:call({global,main_indexer},{unregister_cell,X,Y}),
	ok.

ask_for_pid(Known,[]) ->
	Known;

ask_for_pid(Known,[{X,Y,undefined}|Tail]) ->
		Entry = case gen_server:call({global,main_indexer},{get_pid,X,Y}) of
				{ok,PID} -> {X,Y,PID};
				{error,unregistered} -> {X,Y,undef};
				{error,badkey} -> []
			end,
		ask_for_pid([Entry|Known], Tail).

ask_for_value(_Generation, Result,[]) ->
	Result;

ask_for_value(Generation, {Values,List},[{X,Y,PID}|Tail]) ->

	{NewValue,NewEntry} = case gen_server:call(PID,{get_state,Generation}) of
					  {ok,Value} -> {Value, {X,Y,PID}};
					  {noproc,_} -> {undef, {X,Y,undef}};
						%  [_,_,NewPID] = ask_for_pid([],[{X,Y,undefined}]),
						%  ask_for_value({[],[]}, [X,Y,NewPID]);
				  	  _ -> {error,unkown}
				  end,
	ask_for_value(Generation, { [NewValue|Values], [NewEntry|List]  },Tail).


get_neightbours(MyValue,{PID,Generation,Neightbours}) ->
	timer:sleep(2000),
	Pred1 = fun({_X,_Y,P}) -> P == undefined end,
	Pred2 = fun({_X,_Y,P}) -> P =/= undefined end,
	Unknown = lists:filter(Pred1,Neightbours),
	NewList = [lists:filter(Pred2,Neightbours) | ask_for_pid([],Unknown)],
	

	{Values,NewNeightbours} = ask_for_value(Generation, {[],[]}, lists:flatten(NewList)),

	Sum = lists:foldl(fun(A,B) -> A+B end,0,Values),
	NewValue = case {MyValue,Sum} of
			   {1,Sum} when Sum > 3 or (Sum < 2)-> 0;
			   {0,Sum} when Sum > 2 -> 1;
			   _ -> 0
		   end,

	gen_server:cast(PID,{update_state,NewValue,NewNeightbours}).

