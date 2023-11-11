% facts
male(adrian).
male(jericho).
male(jiawei).
likes(adrian, jericho).
likes(jericho, adrian).
likes(adrian, jiawei).
likes(jericho, jiawei).

% rules
gay(X) :- likes(X, Y).