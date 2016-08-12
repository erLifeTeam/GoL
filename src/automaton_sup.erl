-module(automaton_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link({X, Y}) ->
    supervisor:start_link({global, automaton_sup}, ?MODULE, {X, Y} ).

init({X, Y}) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [ #{id => list_to_atom("row_sup" ++ integer_to_list(N)),
                     start => {row_sup, start_link, [{X, Y, N}]},
                     restart => transient,
                     shutdown => infinity,
                     type => supervisor,
                     modules => [row_sup]} || N <- lists:seq(0, Y) ],
    {ok, {SupFlags, ChildSpecs}}.

