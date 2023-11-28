:- [hogwarts].
:- [part2Helper].

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

% Main 
main :-
    current_prolog_flag(argv, [DataFile|_]),
    open(DataFile, read, Stream),
    read_file(Stream,Lines), %Lines contain all the information within line split by spaces, comma and period.
    nlpParser(Lines, [], ItemsReversed, [], ConstraintsReversed),
    reverse(ItemsReversed, Items),
    reverse(ConstraintsReversed, Constraints),
    writeln(Items),
    writeln(Constraints),
    close(Stream).


% swipl -s part2.pl -t main --quiet -- example1.txt > part2Result1.txt
% swipl -s part2.pl -t main --quiet -- example1.txt