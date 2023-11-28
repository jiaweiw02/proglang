:- [hogwarts].
:- [part2Helper].

% Hints:
%   for NLP parser make sure you match the type of the atom: 
%      it may come in handy that print and write are two different method of printing within prolog 

nlpParser([]).
nlpParser([ [Person, has, item, Item, that, costs, Cost, _, _, _, _, Size, _ | _] | T]) :-
    % atom(Person),
    % atom(Item),
    % number(Cost),
    % number(Size),
    writeln([Person, Item, Cost, Size]),
    nlpParser(T).

nlpParser([ [House, house, wants, total, price, CompPrice, than, Price, dollars, and, total, volume, CompSize, than, Size | _] | T]) :-
    writeln([House, CompPrice, Price, CompSize, Size]),
    nlpParser(T).

nlpParser([ [House, house, wants, total, volume, CompSize, than, Size, cubic, feet, and, total, price, CompPrice, than, Price | _] | T]) :-
    writeln([House, CompPrice, Price, CompSize, Size]),
    nlpParser(T).

% Main 
main :-
    current_prolog_flag(argv, [DataFile|_]),
    open(DataFile, read, Stream),
    read_file(Stream,Lines), %Lines contain all the information within line split by spaces, comma and period.
    nlpParser(Lines),
    % write(Lines),
    close(Stream).


% swipl -s part2.pl -t main --quiet -- example1.txt > part2Result1.txt
% swipl -s part2.pl -t main --quiet -- example1.txt