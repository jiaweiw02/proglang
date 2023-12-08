p(a). p(b).
  q(b). q(c). q(d).
  r(X) :- p(X), q(X).
  s(X,Y) :- p(X), !, q(Y).
  t(X,Y) :- q(X), !, p(Y).