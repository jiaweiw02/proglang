:- [hogwarts].
:- [part2Helper].
:- use_module(library(clpfd)).

% Hints:
%   for NLP parser make sure you match the type of the atom: 
%      it may come in handy that print and write are two different method of printing within prolog 

% item
nlpParser([], Item, Item, Constraint, Constraint).
nlpParser([[Person, has, item, Item, that, costs, Cost, _, _, _, _, Size, _ | _] | T], ItemC, ItemR, ConstraintC, ConstraintR) :-
    atom_number(Cost, CostNumber),
    atom_number(Size, SizeNumber),
    nlpParser(T, [ [Person, Item, CostNumber, SizeNumber] | ItemC], ItemR, ConstraintC, ConstraintR).

% constraint
nlpParser([[House, house, wants, total, price, CompPrice, than, Price, dollars, and, total, volume, CompSize, than, Size | _] | T],
            ItemC, ItemR, ConstraintC, ConstraintR) :-
    atom_number(Price, PriceNumber),
    atom_number(Size, SizeNumber),
    nlpParser(T, ItemC, ItemR, [ [House, CompPrice, PriceNumber, CompSize, SizeNumber] | ConstraintC], ConstraintR).

% constraint
nlpParser([[House, house, wants, total, volume, CompSize, than, Size, cubic, feet, and, total, price, CompPrice, than, Price | _] | T],
            ItemC, ItemR, ConstraintC, ConstraintR) :-
    atom_number(Price, PriceNumber),
    atom_number(Size, SizeNumber),
    nlpParser(T, ItemC, ItemR, [ [House, CompPrice, PriceNumber, CompSize, SizeNumber] | ConstraintC], ConstraintR).


priceIsGreater(Price, Items) :-
    maplist(nth0(2), Items, Prices),
    sum_list(Prices, TotalPrice),
    TotalPrice > Price.

priceIsSmaller(Price, Items) :-
    maplist(nth0(2), Items, Prices),
    sum_list(Prices, TotalPrice),
    TotalPrice < Price.

sizeIsGreater(Size, Items) :-
    maplist(nth0(3), Items, Sizes),
    sum_list(Sizes, TotalSize),
    TotalSize > Size.

sizeIsSmaller(Size, Items) :-
    maplist(nth0(3), Items, Sizes),
    sum_list(Sizes, TotalSize),
    TotalSize < Size.


isHouse(House, [Name, _, _, _]) :-
    houseOf(House, Name).

combs([],[]).

combs([_ | T], T2) :-
    combs(T, T2).
combs([H | T], [H | T2]) :-
    combs(T, T2).


generateCombinations(Items, ValidSubsets) :-
    findall(Subset, combs(Items, Subset), ValidSubsets).

priceFilter(greater, Subsets, Price, ValidSubsets) :-
    include(priceIsGreater(Price), Subsets, ValidSubsets).

priceFilter(less, Subsets, Price, ValidSubsets) :-
    include(priceIsSmaller(Price), Subsets, ValidSubsets).

sizeFilter(greater, Subsets, Size, ValidSubsets) :-
    include(sizeIsGreater(Size), Subsets, ValidSubsets).

sizeFilter(less, Subsets, Size, ValidSubsets) :-
    include(sizeIsSmaller(Size), Subsets, ValidSubsets).


filter([House, PriceConstraint, Price, SizeConstraint, Size], Items, Result) :-
    include(isHouse(House), Items, HouseItems),
    generateCombinations(HouseItems, ValidSubsets),
    priceFilter(PriceConstraint, ValidSubsets, Price, ValidSubsets1),
    sizeFilter(SizeConstraint, ValidSubsets1, Size, Result),
    writeln(House).


printResults([], _).
printResults([C | T], Items) :-
    filter(C, Items, ResultsReversed),
    reverse(ResultsReversed, Result),
    printResultHelper(Result),
    printResults(T, Items).

printResultHelper([]).
printResultHelper([H | T]) :-
    maplist(nth0(1), H, ItemNames),
    maplist(nth0(2), H, ItemPrices),
    maplist(nth0(3), H, ItemSizes),
    sum_list(ItemPrices, TotalPrice),
    sum_list(ItemSizes, TotalSize),
    format("\t[~w,~w]:~p~n", [TotalPrice, TotalSize, ItemNames]),
    % write(TotalPrice), write(' '), write(TotalSize), write(' '), writeln(ItemNames),
    printResultHelper(T).



% Main 
main :-
    current_prolog_flag(argv, [DataFile|_]),
    open(DataFile, read, Stream),
    read_file(Stream,Lines), %Lines contain all the information within line split by spaces, comma and period.
    nlpParser(Lines, [], ItemsReversed, [], ConstraintsReversed),
    reverse(ItemsReversed, Items),
    reverse(ConstraintsReversed, Constraints),
    printResults(Constraints, Items),
    close(Stream).


% swipl -s part2.pl -t main --quiet -- example1.txt > part2Result1.txt
% swipl -s part2.pl -t main --quiet -- example1.txt