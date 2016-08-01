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

handle_call({register_row, X, Pid},
            From,
            {Rows, RowCount, SizeY, Generation, Value}) ->
  NewRows = maps:put(X, Pid, Rows),
  error_logger:info_msg("Call register_row from ~p .~n", [Pid]),
  {reply, ok, {NewRows, RowCount, SizeY, Generation, Value}};



handle_call({write_line, X, Content},
              From,
              {Rows, 1, SizeY, Generation, Value}) ->
  error_logger:info_msg("Received row state (last row) ~p ~p ~n", [X, Content]),

  NewValue = maps:put(X, Content, Value),
  gen_server:reply(From,  ok),

  error_logger:info_msg("Last call of register write. ~n Save all to the file. ~n"),
  error_logger:info_msg("Content of map: ~n ~p ~n", [NewValue]),
  %%spawn_link(?MODULE, prepare_to_save, [Generation, NewValue]),
  
  prepare_to_save(Generation, NewValue),
  error_logger:info_msg("Finish the server."),
  {stop, normal, {Rows, 0, SizeY, Generation, NewValue}};
  %%gen_server:stop(self());

handle_call({write_line, X, Content},From,
            {Rows, RowCount, SizeY, Generation, Value}) ->
  error_logger:info_msg("Received row state ~p ~p ~n", [X, Content]),
  NewValue = maps:put(X, Content, Value),
  {reply, ok, {Rows, RowCount - 1, SizeY, Generation, NewValue}};



handle_call(_Request, _From, _State) ->
  {reply, {error, unknown_request}}.

handle_cast(_Msg, State) ->
  {noreply, State}.


terminate(normal, _State) ->
  ok.

prepare_to_save(Generation, Value) ->
  error_logger:info_msg("Call of prepare_to_save fdunction.~n"),
  error_logger:info_msg("Args: ~p ~n ~p ~n", [Generation, Value]),
  FileName = lists:flatten(io_lib:format("result_~p.txt", [Generation])),
  error_logger:info_msg("FileName: ~p ~n", [FileName]),
  Rows = maps:values(Value),
  error_logger:info_msg("Values: ~n ~p ~n", [Rows]),
  {ok, Device} = file:open(FileName, [write]),
  try save_lines(Device, Rows)
    after file:close(Device)
  end,
  error_logger:info_msg("Saving to file completed ~p.~n", [Generation]),
  ok.

save_lines(Device, []) -> ok;

save_lines(Device, [Row | Rest]) ->
  io:fwrite(Device, "~s~n", [lists:concat(Row)]),
  error_logger:info_msg("Next line: ~s~n", [lists:concat(Row)]),
  save_lines(Device, Rest).

