-module(register_row_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link({X, Y}) ->
    supervisor:start_link(?MODULE, {X, Y}).

init({X, Y}) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [ #{id => list_to_atom("row_indexer" ++ integer_to_list(N)),
                     start => {row_indexer, start_link, [{X, N}]},
                     restart => transient,
                     shutdown => infinity,
                     type => worker,
                     modules => [row_indexer]} || N <- lists:seq(0, Y)],
    {ok, {SupFlags, ChildSpecs}}. 
