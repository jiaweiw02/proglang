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

% choose combinations of items such that the total of all items is greater than price
priceGreater(Items, Price) :-
    findall(Subset, subset(Items, Subset), Subsets),
    maplist(nth0(30), Subsets, One),
    maplist(nth0(5), Subsets, Two),
    writeln(One),
    writeln(Two),
    writeln(Subsets).
    % include(priceIsGreater(Price), Subsets, ValidSubsets),
    % writeln(ValidSubsets).

priceSmaller(Items, Price) :-
    findall(Subset, subset(Items, Subset), Subsets),
    include(priceIsSmaller(Price), Subsets, ValidSubsets),
    writeln(ValidSubsets).


% Main 
main :-
    current_prolog_flag(argv, [DataFile|_]),
    open(DataFile, read, Stream),
    read_file(Stream,Lines), %Lines contain all the information within line split by spaces, comma and period.
    nlpParser(Lines, [], ItemsReversed, [], ConstraintsReversed),
    reverse(ItemsReversed, Items),
    reverse(ConstraintsReversed, Constraints),
    % writeln(Items),
    % writeln(Constraints),
    % priceGreater(Items, 1000),
    Vs = [_,_,_], 
    global_cardinality(Vs, [1-2,3-_]), 
    label(Vs),
    close(Stream).


% swipl -s part2.pl -t main --quiet -- example1.txt > part2Result1.txt
% swipl -s part2.pl -t main --quiet -- example1.txt