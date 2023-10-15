-module(hello).
-export([hello_world/0, mapping/0, lists/0, for/2, runLoop/0, ifstatement/0]).

hello_world () -> 
    P = {john, 24, {june, 25}} , 
    io:fwrite("~w~n", [tuple_size(P)]).

mapping () ->
    M1 = #{name=>john, age=>25} ,
    io:fwrite("~w~n", [map_size(M1)]).

lists () ->
    L1 = [1,2,3,4,5],
    io:fwrite("~w~n", [length(L1)]).

for (0, _) -> [];
    for (N, Term) when N > 0 ->
        io:fwrite("Hello~n"),
        for(N-1, Term).

runLoop() ->
    for(3, atom).

ifstatement() ->
    Adrian = boy,
    Jose = boy,
    Andrew = boylover,

    if
    Adrian == Jose ->
        io:fwrite("~w~n", [nonkidslover]);
    Andrew == Jose ->
        io:fwrite("~w~n", [kidslover]);
    true ->
        io:fwrite("~w~n", [kidslover])
    end.

