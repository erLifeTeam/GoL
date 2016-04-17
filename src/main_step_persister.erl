-module(main_step_persister).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

start_link({Y}) ->
	gen_server:start_link({global,main_step_persister},?MODULE,{Y},[]).

init({Y}) ->
	NewState = {#{},
		    #{},
		    Y},
	{ok,NewState}.

handle_call(show_state,_From,State) ->
	{reply,State,State};

handle_call(_Request,_From,State) ->
	{reply,{error,unknown_request},State}.


handle_cast({collect_data,{Generation,Row},Data},{DataMap,CounterMap,Y}) ->
	TmpDataMap = maps:put({Generation,Row},Data,DataMap),
	{NewDataMap,
	 NewCounterMap} = case maps:get(Generation,CounterMap,{error,badkey})
			  of
				  Y -> 
					GenMap = maps:filter(
						   fun( {Gen,_}, _) -> Gen == Generation  end,
						   TmpDataMap),
					Message = {write_file,Generation,maps:values(GenMap)},

					gen_server:cast({global,main_persister},Message),

					{
					 maps:filter(
					   fun( {Gen,_}, _) -> Gen =/= Generation end,
					   TmpDataMap),
					 maps:remove(Generation,CounterMap)
					};
				  {error,badkey} -> 
					  {
					   TmpDataMap,
					   maps:put(Generation,1,CounterMap)
					  };
				  Value -> 
					  {
					   TmpDataMap,
					   maps:update(Generation,Value+1,CounterMap)
					  }
			  end,
	{noreply,{NewDataMap,NewCounterMap,Y}};

handle_cast(_Request,State) ->
	{noreply,State}.

terminate(_Reason,State) ->
	normal.

