-module(fibonacci).
-export([main/0, fibonacci/1, helperfibo/4]).

fibonacci(N) when N > 1 ->
    First = 1,
    Second = 1,

    helperfibo(First, Second, 2, N).


helperfibo(Prev1, Prev2, Counter, N) ->
    
    if
        Counter < N ->
            Current = Prev1 + Prev2,
            helperfibo(Prev2, Current, Counter + 1, N);
        true ->
            ReturnV = Prev2,
            io:fwrite("~w~n", [ReturnV])
    end.


main() ->
    fibonacci(9).
