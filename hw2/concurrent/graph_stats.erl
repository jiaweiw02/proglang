-module(graph_stats).
-import(lists, [append/2]).
-export([start/1, readFile/1, splitComma/1, splitSpace/1, partions/1]).
-export([pairEdges/1, continueReading/2, stripEndLine/1]).


readFile(File) ->
    {ok, Txt} = file:open(File, [read]),
    P = io:get_line(Txt, ""),
    CompleteP = continueReading(P, Txt),
    io:fwrite("~p~n", [CompleteP]),
    file:close(File),
    io:fwrite("closed file").


% helper function for readFile, continues reading
% all the partions
continueReading(P, _) when P == eof -> [];
continueReading(P, Txt) ->
    Data = partions(Txt),
    StripP = stripEndLine(P),
    NextPartion = io:get_line(Txt, ""),
    [[StripP|Data] | continueReading(NextPartion, Txt)].


% takes a string, removes endline character
stripEndLine(Str) ->
    NewStr = string:tokens(Str, "\n"),
    [H|T] = NewStr,
    io:fwrite("before ~p~n after~p~n", [Str, H]),
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
    Pair = {N1, N2},
    [Pair | pairEdges (T0)].


% takes in a line of edges separated by space
splitSpace(Str) ->
    H = stripEndLine(Str),
    Split = string:tokens(H, " "),
    FinalList = pairEdges(Split),
    FinalList.
    

% takes in P (partion), and the String of the file
% reads 4 lines and partions it accordingly
partions(Txt) ->
    Nodes = io:get_line(Txt, ""),
    Colors = io:get_line(Txt, ""),
    Edges = io:get_line(Txt, ""),
    SplitNodes = splitComma(Nodes),
    SplitColors = splitComma(Colors),
    SplitEdges = splitSpace(Edges),
    [SplitNodes, SplitColors, SplitEdges].


start(InputFile) ->
    readFile(InputFile).