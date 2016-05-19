-module(persister).
-author("Michal Zagorski").

-behaviour(gen_server).

%% API
-export([start_link/1]).


-export([init/1,
  handle_call/3,
  handle_cast/2,
  terminate/2]).


start_link({RowCount, SizeY, Generation}) ->
          gen_server:start_link(?MODULE, {RowCount, SizeY, Generation}, []).

init({RowCount, SizeY, Generation}) ->
  State = {#{}, RowCount, SizeY, Generation, #{}},
  %%%Reply = gen_server:call({global, main_peresister},
  %%%                        {register_generation, Generation}),
  {ok, State}.

handle_call({register_row, X},
            From,
            {Rows, RowCount, SizeY, Generation, Value}) ->
  NewRows = maps:put(X, From, Rows),
  {reply, {NewRows, RowCount, SizeY, Generation, Value}};

handle_call({write_line, X, Content},
              From,
              {Rows, 1, SizeY, Generation, Value}) ->
  NewValue = maps:put(X, Content, Value),


  gen_server:reply(From,  ok),
  Device = io:
  spawn_link(?MODULE, prepare_to_save, [Generation, NewValue]),
  gen_server:stop(normal, {Rows, 0, SizeY, Generation, NewValue});

handle_call({write_line, X, Content},
            _From,
            {Rows, RowCount, SizeY, Generation, Value}) ->
  NewValue = maps:put(X, Content, Value),
  {reply, ok, {Rows, RowCount - 1, SizeY, Generation, NewValue}};

handle_call(_Request, _From, _State) ->
  {reply, {error, unknown_request}}.

handle_cast(_Msg, State) ->
  {noreply, State}.


terminate(normal, _State) ->
  ok.

prepare_to_save(Generation, Value) ->
  FileName = io_lib:format("result_~p.txt", [Generation]),
  {ok, Device} = io:open(FileName, [write]),
  Rows = maps:values(Value),
  try save_lines(Device, Rows)
    after io:close(Device)
  end,
  ok.

save_lines(Device, []) -> ok;

save_lines(Device, [Row | Rest]) ->
  io:fwrite("~p~n", [Row]),
  save_lines(Device, Rest).
