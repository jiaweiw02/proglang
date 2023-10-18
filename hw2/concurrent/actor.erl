-module(actor).
-export([create/1, actor/1]).

actor(Content) ->
    receive
        {colorCount} -> 
            io:fwrite("colorcount");
        {set, NewContent} ->
            actor(NewContent);
        {get, Customer} ->
            Customer ! Content,
            actor(Content)
    end.

create(Content) -> spawn(actor, actor, [Content]).