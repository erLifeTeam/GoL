-module(cell).
-behaviour(gen_server).

-export([start_link/1, get_state/2, update_state/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([get_neightbours/2]).

-record (cell_state, {
           value,
           generation,
           xcord,
           ycord,
           my_neightbours
          }).

%%%
% External functions
%%%

start_link({MaxX, MaxY, X, Y}) ->
    gen_server:start_link(?MODULE, {MaxX, MaxY, X, Y}, []).

get_state(Pid, Generation) ->
    gen_server:call(Pid, {get_state, Generation}).

update_state(Pid, NewValue, NewNeightbours) ->
    gen_server:cast(Pid, {update_state, NewValue, NewNeightbours}).

%%%
% Callback functions
%%%

init({MaxX, MaxY, X, Y}) ->
    {Gen, StartValue} = gen_server:call({global, cache}, {get_state, X, Y}),
    NewState = #cell_state{value = #{Gen => StartValue}, % key - generation, value = value
                           generation = Gen,
                           xcord = X,
                           ycord = Y,
                           my_neightbours = 
                           [{X1, Y1, undefined} || 
                            X1 <- lists:seq(X-1, X+1),
                            Y1 <- lists:seq(Y-1, Y+1),
                            X1 >= 0, X1 =< MaxX,
                            (X1 =/= X) or (Y1 =/= Y),
                            Y1 >= 0, Y1 =< MaxY ]
                           %				my_neightbours= [ {X, Y+1, undefined},
                           %						  {X+1, Y+1, undefined},
                           %						  {X+1, Y, undefined},
                           %						  {X+1, Y-1, undefined},
                           %						  {X, Y-1, undefined},
                           %						  {X-1, Y-1, undefined},
                           %						  {X-1, Y, undefined},
                           %						  {X-1, Y+1, undefined}]
                          },

    gen_server:call({global, main_indexer}, {register_cell, X, Y}),
    spawn(?MODULE,
          get_neightbours,
          [StartValue,
           {	self(),
                Gen,
                NewState#cell_state.my_neightbours
           }]),
    {ok, NewState}.

handle_call({get_state, Generation}, _From, State) ->
    Answer = maps:get(Generation, State#cell_state.value, no_value),
    {reply, {ok, Answer}, State};

handle_call(show_state, _From, State) ->
    {reply, State, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({update_state, NewValue, NewNeightbours}, State) ->

    Pred = fun(K, _V) -> %it is used to delete history older than 3 gens
                   (K - State#cell_state.generation) < 3
           end,

    NewState =  State#cell_state{value=maps:put(
                                         State#cell_state.generation+1,
                                         NewValue,
                                         maps:filter(Pred, State#cell_state.value)
                                        ),
                                 generation=State#cell_state.generation+1,
                                 my_neightbours=NewNeightbours},
    gen_server:cast({global, cache}, {update_state,
                                      State#cell_state.xcord,
                                      State#cell_state.ycord,
                                      State#cell_state.generation+1,
                                      NewValue
                                     }),
    spawn(?MODULE,
          get_neightbours,
          [NewValue,
           {	self(),
                NewState#cell_state.generation,
                NewState#cell_state.my_neightbours
           }]),

    {noreply, NewState};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

code_change(_OldSvn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, State) ->
    X = State#cell_state.xcord,
    Y = State#cell_state.ycord,
    gen_server:call({global, main_indexer}, {unregister_cell, X, Y}),
    ok.

%%%%%%%%%%%%%%%
%%% Internal functions
%%%%%%%%%%%%%%%


ask_for_pid(Known, []) ->
    lists:flatten(Known);

ask_for_pid(Known, [{X, Y, undefined}|Tail]) ->
    {Entry, AddToTail} = case gen_server:call({global, main_indexer}, {get_pid, X, Y}) of
                             {ok, PID} -> 
                                 {{X, Y, PID},
                                  []};
                             {error, unregistered} -> 
                                 {[],
                                  {X, Y, undefined}};
                             {error, badkey} -> []
                         end,
    NewTail = lists:flatten([AddToTail|Tail]),
    ask_for_pid([Entry|Known], NewTail).

ask_for_value(_Generation, {Res1, Res2}, []) ->
    {lists:flatten(Res1), lists:flatten(Res2)};

ask_for_value(Generation, {Values, List}, [{X, Y, PID}|Tail]) ->

    {NewValue,
     NewEntry,
     AddToTail} =
    case cell:get_state(PID, Generation) of
        {ok, no_value} -> 
            timer:sleep(1),
            {[],
             [],
             {X, Y, PID}};

        {ok, Value} -> 
            {Value,
             {X, Y, PID},
             []};

        {noproc, _} ->
            timer:sleep(10),
            {[],
             [],
             ask_for_pid([], [{X, Y, undefined}] )};
        _ -> {[], [], []}
    end,
    NewTail = lists:flatten([AddToTail|Tail]),
    ask_for_value(Generation, { [NewValue|Values], [NewEntry|List]  }, NewTail).


get_neightbours(MyValue, {PID, Generation, Neightbours}) ->
    Pred1 = fun({_X, _Y, P}) -> P == undefined end,
    Pred2 = fun({_X, _Y, P}) -> P =/= undefined end,
    Unknown = lists:filter(Pred1, Neightbours),
    NewList = lists:flatten(
                [lists:filter(Pred2, Neightbours) | ask_for_pid([], Unknown)]
               ),

    {Values, NewNeightbours} = ask_for_value(Generation, {[], []}, NewList),

    Sum = lists:foldl(fun(A, B) -> A+B end, 0, Values),
    NewValue = case {MyValue, Sum} of
                   {0, Sum} when Sum == 3 -> 1;
                   {1, Sum} when (Sum > 3) or (Sum < 2)-> 0;
                   _ -> 0
               end,
    cell:update_state(PID, NewValue, NewNeightbours).
