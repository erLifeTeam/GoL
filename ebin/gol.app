{application,gol,
             [{description,"Erlang Game of Life"},
              {vsn,"0.0.1"},
              {registered,[main_indexer]},
              {mod,{gol,["input.txt"]}},
              {modules,[automaton_sup,cell,gol,main_indexer,register_row_sup,
                        register_sup,row_indexer,row_sup]}]}.
