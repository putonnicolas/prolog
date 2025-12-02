% Un petit fait
parent(john, mary).
parent(mary, alice).

% Une règle
grandparent(X, Y) :-
    parent(X, Z),
    parent(Z, Y).
