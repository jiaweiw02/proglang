sentence --> a, b_sequence, a.
a --> [a].
b_sequence --> [b].
b_sequence --> [b], b_sequence.

parse(Input) :-
    phrase(sentence, Input).

% Example usage:
% ?- parse([a,b,a]).
% ?- parse([a,b,b,a]).
% ?- parse([a,b,b,a,a,a]).