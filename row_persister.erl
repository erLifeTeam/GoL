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
  State = {#{}, X, Size, Generation, GenPersister},
  gen_server:call(GenPersister, {register_row, X, self()}),
  {ok, State}.

handle_cast({put_value,X, Y, Val, Generation}, 
            {Map, X, 1, Generation, GenPersister}) ->
  NewMap = maps:put(Y, Val, Map),

  error_logger:info_msg("Updated map: ~p~n", [NewMap]),
  error_logger:info_msg("Call content: ~p, ~p, ~p~n", [write_line, X, maps:values(NewMap)]),

  Reply = gen_server:call(GenPersister,
                  {write_line, X, maps:values(NewMap)}),

  error_logger:info_msg("State sended ~p ~n", [Reply]),

 {stop, normal, {NewMap, X, 0, Generation, GenPersister}};
 %%gen_server:stop(self());

handle_cast({put_value,X, Y, Val, Generation},
            {Map, X, Counter, Generation, GenPersister}) ->
  NewMap = maps:put(Y, Val, Map),

  error_logger:info_msg("Updated map: ~p~n", [NewMap]),
  
  NewCounter = Counter - 1,
  NewState = {NewMap, X, NewCounter, Generation, GenPersister},
  {noreply, NewState}.

handle_call(_Request, _From, State) ->
  {reply, {error, unknown_request}, State}.

terminate(normal, _State) ->
  ok.
