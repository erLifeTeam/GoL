-module(persister).
-author("Michal Zagorski").

-behaviour(gen_server).

%% API
-export([start_link/1]).


-export([init/1,
  handle_call/3,
  handle_cast/2,
  terminate/2]).

-record(gen_state, {rows_data, rows_count = #{}, size_Y = 0, generation = 0, value = #{}}).

start_link({RowCount, SizeY, Generation}) ->
          gen_server:start_link(?MODULE, {RowCount, SizeY, Generation}, []).

init({RowCount, SizeY, Generation}) ->
  State = #gen_state{rows_data = #{}, rows_count = RowCount, size_Y = SizeY, generation = Generation, value = #{}},
  %%%Reply = gen_server:call({global, main_peresister},
  %%%                        {register_generation, Generation}),
  {ok, State}.

handle_call({register_row, X, Pid},
            From,
            OldState) ->
  NewRows = maps:put(X, Pid, OldState#gen_state.rows_data),
  error_logger:info_msg("Call register_row from ~p .~n", [Pid]),
  {reply, ok, OldState#gen_state{rows_data = NewRows}};



handle_call({write_line, X, Content},
              From,
              OldState = #gen_state{rows_count = 1, value = Value, generation = Generation}) ->
              %%{Rows, 1, SizeY, Generation, Value}) ->
  error_logger:info_msg("Received row state (last row) ~p ~p ~n", [X, Content]),

  NewValue = maps:put(X, Content, Value),
  gen_server:reply(From,  ok),

  error_logger:info_msg("Last call of register write. ~n Save all to the file. ~n"),
  error_logger:info_msg("Content of map: ~n ~p ~n", [NewValue]),
  %%Pid2 = spawn_link(?MODULE, prepare_to_save, [Generation, NewValue]),
  prepare_to_save(Generation, NewValue),
  error_logger:info_msg("Finish the server."),
  {stop, normal, OldState#gen_state{rows_count = 0, value = NewValue}};
  %%gen_server:stop(self());

handle_call({write_line, X, Content},From,
            %%{Rows, RowCount, SizeY, Generation, Value}
            OldState = #gen_state{value = Value, rows_count = RowCount}) ->
  error_logger:info_msg("Received row state ~p ~p ~n", [X, Content]),
  NewValue = maps:put(X, Content, Value),
  {reply, ok, OldState#gen_state{rows_count = RowCount -1, value = NewValue}};



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

