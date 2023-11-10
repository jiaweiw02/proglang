-module(tutorial2).
-export([main/0, add/2]).

add(X, Y) ->
    Sum = X + Y,
    Sum.



main() ->
    L = add(2,3),
    io:fwrite("~w~n", [L]).

