-module(fiboRecursion).
-export([main/0, fibonacci/1]).

fibonacci(N) when N == 0 -> 1;
fibonacci(N) when N == 1 -> 1;
fibonacci(N) when N == 2 -> 1;
fibonacci(N) ->
    fibonacci(N - 1) + fibonacci(N - 2).

main() ->
    fibonacci(9).