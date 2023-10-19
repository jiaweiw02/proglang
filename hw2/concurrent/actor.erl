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
        {length} -> 
            Length = lists:nth(1, Content),
            Length;
        {print} ->
            io:fwrite("this is my content ~n~w~n", Content)
    end.

create(Content) -> spawn(actor, actor, [Content]).