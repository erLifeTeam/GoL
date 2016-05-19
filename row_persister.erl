-module(row_persister).
-author("Michal Zagorski").

-behaviour(gen_server).

%% API
-export([start_link/1]).


-export([init/1,
  handle_call/3,
  handle_cast/2,
  terminate/2]).


start_link({X, Size, Generation, GenPersister}) ->
          gen_server:start_link(?MODULE, {X, Size, Generation, GenPersister}, []).

init({X, Size, Generation, GenPersister}) ->
  State = {#{}, X, Size, Size, Generation, GenPersister},
  gen_server:call(GenPersister, {register_row, X}),
  {ok, State}.

handle_cast({put_value,X, Y, Val, Generation}, {Map, X, Size, 1, Generation, GenPersister}) ->
  NewMap = maps:put(Y, Val, Map),
  gen_server:call(GenPersister,
                  {write_line, X,
                  maps:fold(
                      fun(K, V, Acc) when is_list(K) -> 10*Acc + V end,
                      0,
                      NewMap)}),
  {stop, normal, {NewMap, X, Size, Size, Generation, GenPersister}};

handle_cast({put_value,X, Y, Val, Generation},
            {Map, X, Size, Counter, Generation, GenPersister}) ->
  NewMap = maps:put(Y, Val, Map),
  NewCounter = Counter - 1,
  NewState = {NewMap, X, Size, NewCounter, Generation, GenPersister},
  {noreply, NewState}.

handle_call(_Request, _From, State) ->
  {reply, {error, unknown_request}, State}.

terminate(normal, _State) ->
  ok.
