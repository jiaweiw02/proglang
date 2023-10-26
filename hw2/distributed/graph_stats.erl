-module(graph_stats).
-import(lists, []).
-import(dict, []).
% -compile(export_all).
-export([start/3, actor/1]).

% actor stuff
computeColorLengthHelper([], DictCount, _) -> DictCount;
computeColorLengthHelper([Key | Keys], DictCount, Dict) ->
    Color = dict:fetch(Key, Dict),
    NewD = dict:update_counter(Color, 1, DictCount),
    computeColorLengthHelper(Keys, NewD, Dict).
computeColorLength(Dict) -> 
    % key is an atom
    Keys = dict:fetch_keys(Dict),
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


% ask the actors if the node is their internal node
contactActors(_, [], _) -> ok;
contactActors(Node, [Actor | Actors], Me) ->
    if
        Actor == Me ->
            contactActors(Node, Actors, Me);
        true ->
            Actor ! {getNode, Node, self()},
            contactActors(Node, Actors, Me)
    end.

receiveActor(0, Res) -> Res;
receiveActor(N, Res) ->
    receive
        {nodeCount, V} -> 
            if V == 0 ->
                receiveActor(N-1, Res);
            true ->
                receiveActor(N-1, V)
            end;
        A -> 
            io:fwrite("~p~n", [A]),
            receiveActor(N-1, Res)
    end.


% 1 : all external nodes
% 2 : all actors
% 3 : the actor calling other actors
computeExternal([], _, _, Dict_External_Edge) -> Dict_External_Edge;
computeExternal([Node | Nodes], Actors, Me, Dict_External_Edge) ->
    contactActors(Node, Actors, Me),
    ReceiveLength = length(Actors),
    Res = receiveActor(ReceiveLength, 0),
    NewDict = dict:store(Node, Res, Dict_External_Edge),
    computeExternal(Nodes, Actors, Me, NewDict).


% takes in a dictionary [Atom : EdgeCount], outputs a list of Atoms of highest EdgeCount
highCountHelper([], _, _, ResList) -> ResList;
highCountHelper([Key | Keys], Dict, HighestCount, ResList) ->
    EdgeCount = dict:fetch(Key, Dict),
    if 
        EdgeCount > HighestCount ->
            highCountHelper(Keys, Dict, EdgeCount, [Key]);
        EdgeCount == HighestCount ->
            NewList = lists:append(ResList, [Key]),
            highCountHelper(Keys, Dict, HighestCount, NewList);
        true ->
            highCountHelper(Keys, Dict, HighestCount, ResList)
    end.

highCount(Dict) ->
    Keys = dict:fetch_keys(Dict),
    highCountHelper(Keys, Dict, 0, []).
    

actor(Content) ->

    receive
        {set, NewContent} ->
            actor(NewContent);
        {get, Pid} ->
            Pid ! Content,
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
            Dict = lists:nth(2, Content),
            AllPairs = lists:nth(3, Content),
            [Node_DegreeCount, ExternalNodes] = computeDegreeAndExternal(Dict, AllPairs),
            ExternalDict = computeExternal(ExternalNodes, Actors, self(), dict:new()),

            F = fun(_, V1, _) ->
                V1
            end,

            CombinedDict = dict:merge(F, ExternalDict, Node_DegreeCount),
            NodesHighestEdges = highCount(CombinedDict),
            Farmer ! [Partition, Node_DegreeCount, NodesHighestEdges],
            actor(Content);
        {getNode, Node, Farmer} ->
            Dict = lists:nth(2, Content),
            AllPairs = lists:nth(3, Content),
            InDict = dict:is_key(Node, Dict),
            
            if
                InDict ->
                    [Node_DegreeCount, _] = computeDegreeAndExternal(Dict, AllPairs),
                    Value = dict:fetch(Node, Node_DegreeCount),
                    Farmer ! {nodeCount, Value};
                true ->
                    Farmer ! {nodeCount, 0}
            end,
            actor(Content)
    end.

create(Content, N) -> 
    AtomStr = "child" ++ integer_to_list(N) ++ "@127.0.0.1",
    Atom = list_to_atom(AtomStr),
    spawn(Atom, graph_stats, actor, [Content]).
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

createActors(_, [], Res) -> Res;
createActors(Count, [H|T], Res) ->
    % AtomStr = "child" ++ integer_to_list(Count) ++ "@127.0.0.1",
    Atom = create(H, Count),
    NewRes = lists:append(Res, [Atom]),
    createActors(Count + 1, T, NewRes).


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

sendFB([], _, _, _) -> true;
sendFB([Actor | Actors], Farmer, Message, AllActors) -> 
    if 
        Message == partB ->
            Actor ! {partB, Farmer, AllActors};
        Message == partA ->
            Actor ! {partA, Farmer}
    end,
    sendFB(Actors, Farmer, Message, AllActors).

% part b receives partition number, degree of each internal node, list of external nodes
receiveFAuxB(0, ResDict) -> ResDict;
receiveFAuxB(N, ResDict) ->
    receive
        [PNum, _, NodesHighestEdges] -> 
            receiveFAuxB(N-1, dict:store(PNum, NodesHighestEdges, ResDict))
    end.
receiveFB(N) ->
    receiveFAuxB(N, dict:new()).

partA([], _, _, _) -> ok;
partA([Key | Keys], NodeCountRes, EdgeCountRes, OutFile) ->
    NodeCount = dict:fetch(Key, NodeCountRes),
    EdgeCount = dict:fetch(Key, EdgeCountRes),
    KeyAtom = list_to_atom(Key),
    FormattedStr = io_lib:format("~p, ~p, ~p~n", [KeyAtom, NodeCount, EdgeCount]),
    file:write(OutFile, FormattedStr),
    partA(Keys, NodeCountRes, EdgeCountRes, OutFile).


printListAtoms([], _) -> ok;
printListAtoms([N], OutFile) ->
    AtomStr = atom_to_list(N),
    FormattedStr = io_lib:format("~s~n", [AtomStr]),
    file:write(OutFile, FormattedStr),
    ok;
printListAtoms([N |Ns], OutFile) ->
    AtomStr = atom_to_list(N),
    FormattedStr = io_lib:format("~s,", [AtomStr]),
    file:write(OutFile, FormattedStr),
    printListAtoms(Ns, OutFile).

partBHelper([], _, Res, _) -> Res;
partBHelper([P | Ps], Dict, Res, OutFile) ->
    Nodes = dict:fetch(P, Dict),
    FormattedStr = io_lib:format("~s: ", [P]),
    file:write(OutFile, FormattedStr),
    printListAtoms(Nodes, OutFile),
    NewRes = lists:append(Res, Nodes),
    partBHelper(Ps, Dict, NewRes, OutFile).


partB(ResDict, OutFile) ->
    P = dict:fetch_keys(ResDict),
    Res = partBHelper(P, ResDict, [], OutFile),
    NewRes = lists:usort(Res),
    file:write(OutFile, "G: "),
    printListAtoms(NewRes, OutFile).



start(Input_file_path, Part_a_output_file_path, Part_b_output_file_path) ->
    PartitionsList = readFile(Input_file_path),
    {ok, OutA} = file:open(Part_a_output_file_path, [write]),
    {ok, OutB} = file:open(Part_b_output_file_path, [write]),

    A = createActors(0, PartitionsList, []),
    Length = length(A),
    sendFA(A, self(), partA),
    [NodeCountRes, EdgeCountRes] = receiveFA(Length),
    Keys = dict:fetch_keys(NodeCountRes),
    partA(Keys, NodeCountRes, EdgeCountRes, OutA),

    B = createActors(0, PartitionsList, []),
    CopyB = createActors(PartitionsList),
    Length = length(B),
    sendFB(B, self(), partB, CopyB),
    ResDict = receiveFB(Length),
    partB(ResDict, OutB),
    ok.


% erl -name child0@127.0.0.1
% erl -name child1@127.0.0.1
% erl -name child2@127.0.0.1
% erl -name parent@127.0.0.1
% c(graph_stats), graph_stats:start("../input.txt", "outA.txt", "outB.txt").