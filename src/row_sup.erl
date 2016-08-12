-module(row_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link({X, MaxY, Y}) ->
    supervisor:start_link(?MODULE, {X, MaxY, Y} ).

init({X, MaxY, Y}) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [#{id => list_to_atom("cell" ++ integer_to_list(N)),
                    start => {cell, start_link, [{X, MaxY, N, Y}]},
                    restart => transient,
                    shutdown => infinity,
                    type => worker,
                    modules => [row_sup]} || N <- lists:seq(0, X)],
    {ok, {SupFlags, ChildSpecs}}.

