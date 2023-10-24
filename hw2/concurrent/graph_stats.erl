-module(graph_stats).
-import(lists, []).
-import(dict, []).
% for actor
-export([create/1, actor/1, computeColorLength/1, computeEdgeLength/2]).
% rest
-export([start/1, readFile/1, splitComma/1, splitSpace/1, partitions/1]).
-export([pairEdges/1, continueReading/2, stripEndLine/1, makeAtom/1]).

% actor stuff
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
    % Color1 = dict:fetch(P1, Dict),
    % NewD = dict:update_counter(Color1, 1, DictCount),
    D1InDict = dict:is_key(P1, Dict),
    D2InDict = dict:is_key(P2, Dict),
    if 
        D1InDict ->
            Color1 = dict:fetch(P1, Dict),
            NewD = dict:update_counter(Color1, 1, DictCount),
            if
                D2InDict ->
                    Color2 = dict:fetch(P2, Dict),
                    NewD1 = dict:update_counter(Color2, 1, NewD),
                    computeEdgeLengthHelper(Ps, NewD1, Dict);
                true ->
                    computeEdgeLengthHelper(Ps, NewD, Dict)
            end;
        true ->
            Color2 = dict:fetch(P2, Dict),
            NewD1 = dict:update_counter(Color2, 1, DictCount),
            computeEdgeLengthHelper(Ps, NewD1, Dict)
    end.

computeEdgeLength(Dict, AllPairs) ->
    computeEdgeLengthHelper(AllPairs, dict:new(), Dict).


computeDegreeAndExternalHelper([], DegreeCount, External, _) -> [DegreeCount, External];
computeDegreeAndExternalHelper([P | Ps], DegreeCount, External, Dict) ->
    {P1, P2} = P,
    % node : color
    % pairs : P1, P2 -> first and second nodes respectively
    P1InDict = dict:is_key(P1, Dict),
    P2InDict = dict:is_key(P2, Dict),
    if 
        % P1 in dict
        P1InDict ->
            NewDegreeCount = dict:update_counter(P1, 1, DegreeCount),
            if
                P2InDict ->
                    % Color2 = dict:fetch(P2, Dict),
                    NewDegreeCount1 = dict:update_counter(P2, 1, NewDegreeCount),
                    computeDegreeAndExternalHelper(Ps, NewDegreeCount1, External, Dict);
                true ->
                    NewExternal = lists:append([P2], External),
                    computeDegreeAndExternalHelper(Ps, NewDegreeCount, NewExternal, Dict)
            end;
        % P1 not in dict
        true ->
            % Color2 = dict:fetch(P2, Dict),
            NewDegreeCount1 = dict:update_counter(P2, 1, DegreeCount),
            NewExternal = lists:append([P1], External),
            computeDegreeAndExternalHelper(Ps, NewDegreeCount1, NewExternal, Dict)
    end.

computeDegreeAndExternal(Dict, AllPairs) ->
    computeDegreeAndExternalHelper(AllPairs, dict:new(), [], Dict).



% we can assume that the node exists somewhere, so we don't need an ending
% condition
% takes in a node and all actors, returns the edge length of that node
sendNodeEdges(_, [], _) -> ok;
sendNodeEdges(Node, [A | As], CurrActor) ->
    if
        A == CurrActor ->
            sendNodeEdges(Node, As, CurrActor);
        true ->
            io:fwrite("SEND~n"),
            A ! {getNode, Node, self()},
            sendNodeEdges(Node, As, CurrActor)
    end.
            

receiveNodeEdges(0, Res) -> Res;
receiveNodeEdges(N, Res) ->
    io:fwrite("RECEIVE~n"),
    receive
        Num ->
            if Num == 0 ->
                receiveNodeEdges(N - 1, Res);
            true ->
                receiveNodeEdges(N - 1, Num)
        end
    end.


% takes in external nodes and all actors, requests them for the edge count
% if it exists in their partition, [atom], [Actor]
computeExternalNode_EdgeCountHelper([], _, Res, _) -> Res;
computeExternalNode_EdgeCountHelper([N | Ns], Actors, Res, CurrActor) ->
    % get edge length of node N
    sendNodeEdges(N, Actors, CurrActor),
    Length = length(Actors),
    N_EdgeLength = receiveNodeEdges(Length - 1, 0),
    NewDict = dict:store(N, N_EdgeLength, Res),
    computeExternalNode_EdgeCountHelper(Ns, Actors, NewDict, CurrActor).
computeExternalNode_EdgeCount(ExternalNodes, Actors, CurrActor) ->
    computeExternalNode_EdgeCountHelper(ExternalNodes, Actors, dict:new(), CurrActor).
    

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
        {partB, Farmer, Actors} ->
            Partition = lists:nth(1, Content),
            io:fwrite("BEGIN: ~p~n", [Partition]),
            Dict = lists:nth(2, Content),
            AllPairs = lists:nth(3, Content),
            [Node_DegreeCount, ExternalNodes] = computeDegreeAndExternal(Dict, AllPairs),
            ExternalNode_EdgeCount = computeExternalNode_EdgeCount(ExternalNodes, Actors, self()),
            io:fwrite("END: ~p~n", [Partition]),
            Farmer ! [Partition, Node_DegreeCount, ExternalNodes];
        {getNode, Node, Farmer} ->
            Partition = lists:nth(1, Content),
            io:fwrite("CHECKING PARTITION: ~p~n", [Partition]),
            Dict = lists:nth(2, Content),
            AllPairs = lists:nth(3, Content),
            InDict = dict:is_key(Node, Dict),
            
            if
                InDict ->
                    [Node_DegreeCount, _] = computeDegreeAndExternal(Dict, AllPairs),
                    Value = dict:fetch(Node, Node_DegreeCount),
                    Farmer ! Value;
                true ->
                    Farmer ! 0
            end
    end.

create(Content) -> spawn(graph_stats, actor, [Content]).

% actor end, parser begin

readFile(File) ->
    {ok, Txt} = file:open(File, [read]),
    P = io:get_line(Txt, ""),
    CompletePartition = continueReading(P, Txt),
    file:close(File),
    CompletePartition.


% helper function for readFile, continues reading
% all the partitions
continueReading(P, _) when P == eof -> [];
continueReading(P, Txt) ->
    Data = partitions(Txt),
    StripP = stripEndLine(P),
    NextPartition = io:get_line(Txt, ""),
    [[StripP|Data] | continueReading(NextPartition, Txt)].


% takes a string, removes endline character
stripEndLine(Str) ->
    NewStr = string:tokens(Str, "\n"),
    % [H|T] = NewStr,
    H = lists:nth(1, NewStr),
    H.


% takes in a line with elements split by a comma
splitComma(Str) ->
    H = stripEndLine(Str),
    Split = string:tokens(H, ","),
    Split.


% helper for splitSpace
% take in a list of "x,y" elements
% returns "{x,y}" elements
pairEdges(Lst) when Lst == [] -> [];
pairEdges(Lst) ->
    [H|T0] = Lst,
    [N1, N2] = string:tokens(H, ","),
    AtomN1 = list_to_atom(N1),
    AtomN2 = list_to_atom(N2),
    Pair = {AtomN1, AtomN2},
    [Pair | pairEdges (T0)].


% takes in a line of edges separated by space
splitSpace(Str) ->
    H = stripEndLine(Str),
    Split = string:tokens(H, " "),
    FinalList = pairEdges(Split),
    FinalList.


makeAtom(Lst) when Lst == [] -> [];
makeAtom(Lst) ->
    [H|T] = Lst,
    Atom = list_to_atom(H),
    [Atom | makeAtom(T)].
    

% takes in P (partition), and the String of the file
% reads 4 lines and partitions it accordingly
partitions(Txt) ->
    Nodes = io:get_line(Txt, ""),
    Colors = io:get_line(Txt, ""),
    Edges = io:get_line(Txt, ""),
    SplitNodes = splitComma(Nodes),
    AtomNodes = makeAtom(SplitNodes),
    SplitColors = splitComma(Colors),
    SplitEdges = splitSpace(Edges),
    [dictionaryCreator(SplitColors, AtomNodes), SplitEdges].

% length of keys has to equal length of values
dictionaryCreator(Keys, Values) ->
    NewDict = dict:new(),
    FinishedDict = dictionaryCreatorHelper(Keys, Values, NewDict),
    FinishedDict.

dictionaryCreatorHelper([], _, D) ->
    D;
dictionaryCreatorHelper([Color | Colors], [Node | Nodes], D) ->
    NewD = dict:store(Node, Color, D),
    dictionaryCreatorHelper(Colors, Nodes, NewD).


% BEGIN HERE (everything above is parser)

createActors([]) -> [];
createActors([H|T]) ->
    [create(H) | createActors(T)].


sendFA([], _, _) -> true;
sendFA([Actor | Actors], Farmer, Message) -> 
    Actor ! {Message, Farmer},
    sendFA(Actors, Farmer, Message).

receiveFA(N) ->
    F = fun(_, V1, V2) ->
        V1 + V2
    end,
    receiveFAuxA(N, dict:new(), dict:new(), F).
receiveFAuxA(0, Node, Edge, _) -> [Node, Edge];
receiveFAuxA(N, Node, Edge, F) ->
    receive
        [CurrNode, CurrEdge] -> 
            receiveFAuxA(N-1, dict:merge(F, CurrNode, Node), dict:merge(F, CurrEdge, Edge), F)
    end.

sendFB([], _, _) -> true;
sendFB([Actor | Actors], Farmer, Message) -> 
    if 
        Message == partB ->
            Actor ! {Message, Farmer, [Actor | Actors]};
        Message == partA ->
            Actor ! {partA, Farmer}
    end,
    sendFB(Actors, Farmer, Message).

% part b receives partition number, degree of each internal node, list of external nodes
receiveFAuxB(0, ResDict) -> ResDict;
receiveFAuxB(N, ResDict) ->
    receive
        [PNum, DegreesDict, ExternalList] -> 
            receiveFAuxB(N-1, dict:store(PNum, [DegreesDict, ExternalList], ResDict));
        O -> io:fwrite("~p~n", [O])
    end.
receiveFB(N) ->
    receiveFAuxB(N, dict:new()).



% % edge count
% sendEdgeCount([], _) -> true;
% sendEdgeCount([Actor | Actors], Farmer) ->
%     Actor ! {edge, Farmer},
%     sendEdgeCount(Actors, Farmer).


printDictHelper([], _) -> true;
printDictHelper([Key | Keys], Dict) ->
    Value = dict:fetch(Key, Dict),
    io:fwrite("~p: ~p~n", [Key, Value]),
    printDictHelper(Keys, Dict).
printDict(Dict) ->
    Keys = dict:fetch_keys(Dict),
    Size = dict:size(Dict),
    io:fwrite("Size of dictionary: ~p~n", [Size]),
    printDictHelper(Keys, Dict).

partA([], _, _) -> ok;
partA([Key | Keys], NodeCountRes, EdgeCountRes) ->
    NodeCount = dict:fetch(Key, NodeCountRes),
    EdgeCount = dict:fetch(Key, EdgeCountRes),
    KeyAtom = list_to_atom(Key),
    io:fwrite("~p, ~p, ~p~n", [KeyAtom, NodeCount, EdgeCount]),
    partA(Keys, NodeCountRes, EdgeCountRes).

start(InputFile) ->
    PartitionsList = readFile(InputFile),

    A = createActors(PartitionsList),
    Length = length(A),
    sendFA(A, self(), partA),
    [NodeCountRes, EdgeCountRes] = receiveFA(Length),
    Keys = dict:fetch_keys(NodeCountRes),
    partA(Keys, NodeCountRes, EdgeCountRes),

    B = createActors(PartitionsList),
    Length = length(B),
    sendFB(B, self(), partB),
    ResDict = receiveFB(Length),
    printDict(ResDict).
    % printDict(NodeCountRes),
    % printDict(EdgeCountRes),
    
    % edges
    % sendF(A, self(), edges),
    % EdgeCountRes = receiveF(Length),
    % io:fwrite("~p~n", [PartitionsList]),
    % io:fwrite("done").

% c(graph_stats), graph_stats:start("../input.txt").