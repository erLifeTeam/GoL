-module(row_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).
 
start_link({MaxY,X,Y,StartValues}) ->
	supervisor:start_link(?MODULE, {MaxY,X,Y,StartValues} ).
 
init({MaxY,X,Y,StartValues}) ->
	SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
	ChildSpecs = [ #{id => list_to_atom("cell" ++ integer_to_list(N)),
			start => {cell, start_link, [{X,MaxY,N,Y,StartValue}]},
			restart => transient,
			shutdown => infinity,
			type => worker,
			modules => [row_sup]} || {StartValue,N} <- lists:zip(StartValues, lists:seq(0,X)) ],
	{ok, {SupFlags, ChildSpecs}}.
	  
