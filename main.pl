plateau_initial([
    [], [], [], [], [], [], []
]).

piece_a(Board, Ligne, Colonne, Piece) :-
    nth1(Colonne, Board, Col),
    nth1(Ligne, Col, Piece), !.
piece_a(_, _, _, vide) :- !.

symbole(vide, '_').
symbole(x, 'X').
symbole(o, 'O').

affiche_ligne(Board, Ligne) :-
    write('| '),
    affiche_colonnes(Board, Ligne, 1),
    write('|'), nl.

affiche_colonnes(_, _, 8) :- !. % stop après la 7e colonne
affiche_colonnes(Board, Ligne, Col) :-
    piece_a(Board, Ligne, Col, P),
    symbole(P, S),
    write(S), write(' '),
    Col1 is Col + 1,
    affiche_colonnes(Board, Ligne, Col1).

affiche_plateau(Board) :-
    affiche_lignes(Board, 6),
    write('  1 2 3 4 5 6 7'), nl.

affiche_lignes(_, 0) :- !.
affiche_lignes(Board, Ligne) :-
    affiche_ligne(Board, Ligne),
    Ligne1 is Ligne - 1,
    affiche_lignes(Board, Ligne1).
