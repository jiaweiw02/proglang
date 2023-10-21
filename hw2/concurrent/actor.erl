-module(actor).
-import(lists, []).
-export([create/1, actor/1]).

actor(Content) ->
    receive
        {set, NewContent} ->
            actor(NewContent);
        {get, Customer} ->
            Customer ! Content,
            actor(Content);
        {length, Farmer} -> 
            Dict = lists:nth(2, Content),
            % You have a dictionary Color:Nodes, compute the statistics and store it in a dictionary
            Length = length(Nodes),
            io:fwrite("~w~n", [Content]),
            Farmer ! Length;
        {print} ->
            % io:fwrite("this is my content ~n~w~n", Content)
            io:fwrite("HELLO YES THE CONTENT IS STORED")
    end.

create(Content) -> spawn(actor, actor, [Content]).

