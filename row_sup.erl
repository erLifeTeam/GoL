-module(row_sup).
-behaviour(supervisor).

-export([start_link/2]).
-export([init/1]).
 
start_link(X,Y) ->
	supervisor:start_link(?MODULE, {X,Y} ).
 
init({X,Y}) ->
	SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
	ChildSpecs = [ #{id => list_to_atom("cell" ++ integer_to_list(N)),
			start => {cell, start_link, [{X,Y,0}]},
			restart => transient,
			shutdown => infinity,
			type => worker,
			modules => [row_sup]} || N <- lists:seq(1,X)],
	{ok, {SupFlags, ChildSpecs}}.
	  
