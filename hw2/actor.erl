-module(actor).
-export([start/0, actor/1]).


actor(Content) ->

    receive
        {print, From, Farmer} ->
            % io:fwrite("this is my content ~n~w~n", Content)
            io:fwrite("from: ~p, content: ~p~n", [From, Content]),
            Farmer ! Content,
            actor(Content)
    end.

create(Content) -> spawn(actor, actor, [Content]).

createActors(0) -> [];
createActors(N) ->
    [create(N) | createActors(N - 1)].

printActors(_, []) -> ok;
printActors(A, [Actor | Actors]) ->
    Actor ! {print, A, self()},
    printActors(A, Actors).

callPrint([], _) -> ok;
callPrint([A | As], Actors) ->
    printActors(A, Actors),
    callPrint(As, Actors).

start() ->
    H = createActors(4),
    callPrint(H, H).

        

% c(actor), actor:start().