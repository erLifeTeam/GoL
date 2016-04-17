-module(persistance_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).
 
start_link({X,Y}) ->
	supervisor:start_link({global,persistance_sup}, ?MODULE, {X,Y} ).
 
init({X,Y}) ->
	SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
	ChildSpecs = [
		      #{id => main_persister,
			start => {main_persister, start_link, [{X,Y}]},
			restart => transient,
			shutdown => infinity,
			type => worker,
			modules => [main_persister]} |
		     [ #{id => list_to_atom("step_persistance_sup" ++ integer_to_list(N)),
			start => {step_persistence_sup, start_link, [{X,Y,100}]},
			restart => transient,
			shutdown => infinity,
			type => supervisor,
			modules => [step_persistence_sup]} 
		       || N <- lists:seq(0,100) ]
		     ],

	{ok, {SupFlags, ChildSpecs}}.
	  
