-module(actor).
-import(lists, []).
-import(dict, []).
-export([start/0, actor/1]).


actor(Content) ->

    receive
        {set, NewContent} ->
            actor(NewContent);
        {get, Customer} ->
            Customer ! Content,
            actor(Content);
        {print} ->
            % io:fwrite("this is my content ~n~w~n", Content)
            io:fwrite("HELLO YES THE CONTENT IS STORED")
    end.

create(Content) -> spawn(actor, actor, [Content]).


start() ->
    {A1, A2} = {create(1), create(2)},

    A1 ! {get, self()},
    receive
        S -> io:fwrite("~p~n", [S])
    end,


    A2 ! {get, self()},
    receive
        A -> io:fwrite("~p~n", [A])
    end.
        

% c(actor), actor:start().