:- use_module(library(clpfd)).


constraint(N) :-
    % find all three-digit numbers such that the sum of the first two
    % is equal to the third, and the product of the first two is equal
    % to twice the third
    D1 in 1..9, D2 in 0..9, D3 in 0..9,
    D2 + D1 #= D3,
    D1 * D2 #= 2 * D3,
    N #= D1 * 100 + D2 * 10 + D3,
    label([D1, D2, D3]).
