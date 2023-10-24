-module(actor).
-import(lists, []).
-import(dict, []).
-export([create/1, actor/1, computeColorLength/1, computeEdgeLength/2]).

computeColorLengthHelper([], DictCount, _) -> DictCount;
computeColorLengthHelper([Key | Keys], DictCount, Dict) ->
    Color = dict:fetch(Key, Dict),
    NewD = dict:update_counter(Color, 1, DictCount),
    % V = dict:fetch(Color, NewD),
    % io:fwrite("~p: ~p~n", [Color, V]),
    computeColorLengthHelper(Keys, NewD, Dict).
computeColorLength(Dict) -> 
    % key is an atom
    Keys = dict:fetch_keys(Dict),
    % Tmp = dict:new(),
    D = computeColorLengthHelper(Keys, dict:new(), Dict),
    D.


computeEdgeLengthHelper([], DictCount, _) -> DictCount;
computeEdgeLengthHelper([P | Ps], DictCount, Dict) ->
    {P1, P2} = P,
    % node : color
    Color1 = dict:fetch(P1, Dict),
    NewD = dict:update_counter(Color1, 1, DictCount),
    D2InDict = dict:is_key(P2, Dict),
    if 
        D2InDict ->
            Color2 = dict:fetch(P2, Dict),
            NewD1 = dict:update_counter(Color2, 1, NewD),
            computeEdgeLengthHelper(Ps, NewD1, Dict);
        true ->
            computeEdgeLengthHelper(Ps, NewD, Dict)
    end.

computeEdgeLength(Dict, AllPairs) ->
    computeEdgeLengthHelper(AllPairs, dict:new(), Dict).
    

actor(Content) ->

    receive
        {set, NewContent} ->
            actor(NewContent);
        {get, Customer} ->
            Customer ! Content,
            actor(Content);
        {partA, Farmer} -> 
            Dict = lists:nth(2, Content),
            AllPairs = lists:nth(3, Content),
            % you have a dictionary node:color
            ColorLength = computeColorLength(Dict),
            EdgeLength = computeEdgeLength(Dict, AllPairs),
            Data = [ColorLength, EdgeLength],
            Farmer ! Data;
        {print} ->
            % io:fwrite("this is my content ~n~w~n", Content)
            io:fwrite("HELLO YES THE CONTENT IS STORED")
    end.

create(Content) -> spawn(actor, actor, [Content]).

