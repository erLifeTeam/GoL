-module(gol).
-behaviour(application).
-export([start/2,stop/1]).
-export([parse_input_file/1]).

start(normal,[File]) ->
	StartValues = parse_input_file(File),
	Y = length(StartValues) - 1 ,
	X = length(lists:last(StartValues)) - 1, %% we start counting from 0 ;x
	register_sup:start_link({X,Y}),
	automaton_sup:start_link({X,Y,StartValues}).

stop(_State) ->
	ok.

parse_input_file(File) ->
	{ok,FilePid} = file:open(File,read),
	parse_input_file(FilePid,[]).

parse_input_file(FilePid,Acc) ->
	case io:get_line(FilePid,"") of 
		eof -> lists:reverse(Acc);
		Data -> ProperValues = [ X - 48 || X <-lists:droplast(Data)],
			parse_input_file(FilePid,[ProperValues|Acc])
	end.
