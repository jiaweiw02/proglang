-module(graph_stats).
-import(lists, []).
-import(dict, []).
% for actor
-export([create/1, actor/1, computeColorLength/1, computeDegreeAndExternal/2]).
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


% getExternalNodes([], _, Res) -> Res
% getExternalNodes([P | Ps], Dict, Res) ->
%     {P1, P2} = P,

    

actor(Content) ->

    receive
        {set, NewContent} ->
            actor(NewContent);
        {partB, Farmer} ->
            Partition = lists:nth(1, Content),
            Dict = lists:nth(2, Content),
            AllPairs = lists:nth(3, Content),
            [Node_DegreeCount, ExternalNodes] = computeDegreeAndExternal(Dict, AllPairs),
            Farmer ! [Partition, Node_DegreeCount, ExternalNodes]
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


sendF([], _, _) -> true;
sendF([Actor | Actors], Farmer, Message) -> 
    Actor ! {Message, Farmer},
    sendF(Actors, Farmer, Message).

% part b receives partition number, degree of each internal node, list of external nodes
receiveFAux(0, ResDict) -> ResDict;
receiveFAux(N, ResDict) ->
    receive
        [PNum, DegreesDict, ExternalList] -> 
            receiveFAux(N-1, dict:store(PNum, [DegreesDict, ExternalList], ResDict))
    end.
receiveF(N) ->
    % F = fun(_, V1, V2) ->
    %     V1 + V2
    % end,
    receiveFAux(N, dict:new()).



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

start(InputFile) ->
    PartitionsList = readFile(InputFile),
    A = createActors(PartitionsList),
    Length = length(A),
    sendF(A, self(), partB),
    ResDict = receiveF(Length),
    printDict(ResDict).
    % [NodeCountRes, EdgeCountRes] = receiveF(Length),
    % Keys = dict:fetch_keys(NodeCountRes),
    % partA(Keys, NodeCountRes, EdgeCountRes),
    % % printDict(NodeCountRes),
    % printDict(EdgeCountRes).

% c(graph_stats), graph_stats:start("../input.txt").