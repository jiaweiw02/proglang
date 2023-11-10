% Facts
parent(alice, bob). % Alice is a parent of Bob
parent(alice, carol). % Alice is a parent of Carol
parent(bob, david). % Bob is a parent of David
parent(carol, emma). % Carol is a parent of Emma

% Rule
grandparent(X, Y) :- parent(X, Z), parent(Z, Y).
