-module(register_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).
 
start_link({X,Y}) ->
	supervisor:start_link({global,register_sup}, ?MODULE, {X,Y} ).
 
init({X,Y}) ->
	SupFlags = #{strategy => one_for_all, intensity => 1, period => 5},
	ChildSpecs = [
		      #{id => main_indexer,
			start => {main_indexer, start_link, [{Y}]},
			restart => transient,
			shutdown => infinity,
			type => worker,
			modules => [main_indexer]},

		     #{id => register_row_sup,
			start => {register_row_sup, start_link, [{X,Y}]},
			restart => transient,
			shutdown => infinity,
			type => supervisor,
			modules => [register_row_sup]}
    		     ],

	{ok, {SupFlags, ChildSpecs}}.
	  
