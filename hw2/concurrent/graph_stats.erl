-module(graph_stats).
-import(lists, [append/2]).
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
    [N1, N2|T1] = string:tokens(H, ","),
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
    io:fwrite("~p~n", [Atom]),
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
    [AtomNodes, SplitColors, SplitEdges].


start(InputFile) ->
    PartitionsList = readFile(InputFile),
    io:fwrite("~p~n", [PartitionsList]).