-module(graph_stats).
-import(lists, [append/2]).
-import(dict, [append/3, new/0]).
-export([start/1, readFile/1, splitComma/1, splitSpace/1, partitions/1]).
-export([pairEdges/1, continueReading/2, stripEndLine/1, makeAtom/1]).


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
    [spawn(actor, actor, [H]) | createActors(T)].


sendF([], _, _) -> true;
sendF([Actor | Actors], Farmer, Message) -> 
    Actor ! {Message, Farmer},
    sendF(Actors, Farmer, Message).

receiveF(N) ->
    F = fun(_, V1, V2) ->
        V1 + V2
    end,
    receiveFAux(N, dict:new(), dict:new(), F).
receiveFAux(0, Node, Edge, _) -> [Node, Edge];
receiveFAux(N, Node, Edge, F) ->
    receive
        [CurrNode, CurrEdge] -> 
            receiveFAux(N-1, dict:merge(F, CurrNode, Node), dict:merge(F, CurrEdge, Edge), F)
    end.


% % edge count
% sendEdgeCount([], _) -> true;
% sendEdgeCount([Actor | Actors], Farmer) ->
%     Actor ! {edge, Farmer},
%     sendEdgeCount(Actors, Farmer).


% printDictHelper([], _) -> true;
% printDictHelper([Key | Keys], Dict) ->
%     Value = dict:fetch(Key, Dict),
%     io:fwrite("~p: ~p~n", [Key, Value]),
%     printDictHelper(Keys, Dict).
% printDict(Dict) ->
%     Keys = dict:fetch_keys(Dict),
%     Size = dict:size(Dict),
%     io:fwrite("Size of dictionary: ~p~n", [Size]),
%     printDictHelper(Keys, Dict).

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
    sendF(A, self(), partA),
    [NodeCountRes, EdgeCountRes] = receiveF(Length),
    Keys = dict:fetch_keys(NodeCountRes),
    partA(Keys, NodeCountRes, EdgeCountRes).
    % printDict(NodeCountRes),
    % printDict(EdgeCountRes),
    
    % edges
    % sendF(A, self(), edges),
    % EdgeCountRes = receiveF(Length),
    % io:fwrite("~p~n", [PartitionsList]),
    % io:fwrite("done").

% c(actor), c(graph_stats), graph_stats:start("../input.txt").