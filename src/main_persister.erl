-module(main_persister).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2]).

start_link({OutFile}) ->
	gen_server:start_link({global,main_persister},?MODULE,{OutFile},[]).

init({OutFile}) ->
	{ok,FilePid} = file:open(OutFile,write),
	{ok,{FilePid,1}}.

handle_call(show_state,_From,State) ->
	{reply,State,State};

handle_call(_Request,_From,State) ->
	{reply,{error,unknown_request},State}.


handle_cast({write_file,Generation,Data},{FilePid,_Generation}) ->
	file:write(FilePid,integer_to_list(Generation)++"\n"),
	ProperDataFormat = lists:map(fun(Row)-> [ X+48 ||  X<-Row ]  end,Data),
	lists:foreach( fun(Row) -> file:write(FilePid,Row++"\n") end, ProperDataFormat),

	{noreply,{FilePid,Generation+1}}.


terminate(_Reason,{FilePid,_Gen}) ->
	file:close(FilePid),
	normal.

