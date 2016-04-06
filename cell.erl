-module(cell).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-record (cell_state, {
	   value,
	   xcord,
	   ycord,
	   my_neightbours
	  }).
 
start_link({X,Y,StartValue}) ->
	gen_server:start_link(?MODULE,{X,Y,StartValue},[]).
 
init({X,Y,StartValue}) ->
	NewState = #cell_state{value=StartValue,
				xcord=X,
				ycord=Y,
				my_neightbours= [ {X,Y+1,undefined},
						  {X+1,Y+1,undefined},
						  {X+1,Y,undefined},
						  {X+1,Y-1,undefined},
						  {X,Y-1,undefined},
						  {X-1,Y-1,undefined},
						  {X-1,Y,undefined},
						  {X-1,Y+1,undefined}]
			       },
%	spawn(?MODULE,get_neightbours,[StartValue,{self(),0,NewState#cell_state.my_neightbours}]),
	{ok,NewState}.

handle_call(get_state,_From,State) ->
	{reply,{ok,State#cell_state.value},State};

handle_call(_Request,_From,State) ->
	{reply,{error,unknown_request},State}.
	  
handle_cast({update_state,NewValue,NewNeightbours},State) ->
	NewState =  State#cell_state{value=NewValue,my_neightbours=NewNeightbours},
			%% do zrobienia, nie wiadomo jakie wiadomosci  gen_server:cast(persistor,{save,state}),
	%spawn(?MODULE,get_neightbours,[StartValue,{self(),0,NewState#cell_state.my_neightbours}]),
	{noreply,NewState};

handle_cast(_Request,State) ->
	{noreply,State}.

ask_for_pid(Known,[]) ->
	Known;

ask_for_pid(Known,[{X,Y,undefined}|Tail]) ->
		PID = gen_server:call(indexer,{X,Y}),
		ask_for_pid([{X,Y,PID}|Known], Tail).

ask_for_value(Result,[]) ->
	Result;

ask_for_value({Values,List},[X,Y,PID|Tail]) ->

	{[NewValue], [NewEntry]} = case gen_server:call(PID,get_state) of
					  {ok,Value} -> {Value, {X,Y,PID}};
					  {noproc,_} -> 
						  [_,_,NewPID] = ask_for_pid([],[{X,Y,undefined}]),
						  ask_for_value({[],[]}, [X,Y,NewPID]);
				  	  _ -> err
				  end,
	ask_for_value( { [NewValue|Values], [NewEntry|List]  },Tail).


get_neightbours(MyValue,{PID,_Generation,Neightbours}) ->
	Pred1 = fun({_X,_Y,P}) -> P == undefined end,
	Pred2 = fun({_X,_Y,P}) -> P =/= undefined end,
	Unknown = lists:filter(Pred1,Neightbours),
	NewList = [lists:filter(Pred2,Neightbours)|ask_for_pid([],Unknown)],

	{Values,NewNeightbours}=ask_for_value({[],[]},NewList),

	Sum = lists:foldl(fun(A,B) -> A+B end,0,Values),
	NewValue = case {MyValue,Sum} of
			   {1,Sum} when Sum > 3 or (Sum < 2)-> 0;
			   {0,Sum} when Sum > 2 -> 1;
			   _ -> 0
		   end,

	gen_server:cast(PID,{update_state,NewValue,NewNeightbours}).

