-module(actorTester).
-export([main/0]).

main() ->
    C = spawn(actor, actor, [0]),
    S = spawn(actor, actor, [0]),

    C ! {set, 5},
    C ! {get, self()},
    receive
        Value -> 
            io:format("C's value: ~w~n", [Value]),
            S ! {set, Value}
    end,

    S ! {get, self()},
    receive
        V -> io:format("~w~n", [V])
    end.

