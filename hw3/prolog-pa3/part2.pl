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

% totalPrice([], Res, Res).
% totalPrice([[_, _, Price, _] | T], Sum, Res) :-
%     totalPrice(T, Sum + Price, Res).

% totalSize([], Res, Res).
% totalSize([[_, _, _, Size] | T], Sum, Res) :-
%     totalSize(T, Sum + Size, Res).

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

isGryffindor([Name, _, _, _]) :-
    houseOf(gryffindor, Name).

isSlytherin([Name, _, _, _]) :-
    houseOf(slytherin, Name).

isRavenclaw([Name, _, _, _]) :-
    houseOf(ravenclaw, Name).

isHufflepuff([Name, _, _, _]) :-
    houseOf(hufflepuff, Name).

combs([],[]).

combs([_ | T], T2) :-
    combs(T, T2).
combs([H | T], [H | T2]) :-
    combs(T, T2).

separateHouses(Items, Gryffindor, Slytherin, Ravenclaw, Hufflepuff) :-
    include(isGryffindor, Items, Gryffindor),
    include(isHufflepuff, Items, Hufflepuff),
    include(isRavenclaw, Items, Ravenclaw),
    include(isSlytherin, Items, Slytherin).

generateCombinations(Items, ValidSubsets) :-
    findall(Subset, combs(Items, Subset), ValidSubsets).

priceGreater(Subsets, Price, ValidSubsets) :-
    include(priceIsGreater(Price), Subsets, ValidSubsets).

priceSmaller(Subsets, Price, ValidSubsets) :-
    include(priceIsSmaller(Price), Subsets, ValidSubsets).

sizeGreater(Subsets, Price, ValidSubsets) :-
    include(sizeIsGreater(Price), Subsets, ValidSubsets).

sizeSmaller(Subsets, Price, ValidSubsets) :-
    include(sizeIsSmaller(Price), Subsets, ValidSubsets).


% Main 
main :-
    current_prolog_flag(argv, [DataFile|_]),
    open(DataFile, read, Stream),
    read_file(Stream,Lines), %Lines contain all the information within line split by spaces, comma and period.
    nlpParser(Lines, [], ItemsReversed, [], ConstraintsReversed),
    reverse(ItemsReversed, Items),
    reverse(ConstraintsReversed, Constraints),

    separateHouses(Items, Gryffindor, Slytherin, Ravenclaw, Hufflepuff),
    % test for ravenclaw
    generateCombinations(Ravenclaw, ValidSubsets),
    priceGreater(ValidSubsets, 700, ValidSubsets1),
    sizeGreater(ValidSubsets1, 80, ValidSubsets2),
    writeln(ValidSubsets2),


    close(Stream).


% swipl -s part2.pl -t main --quiet -- example1.txt > part2Result1.txt
% swipl -s part2.pl -t main --quiet -- example1.txt