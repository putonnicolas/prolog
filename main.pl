%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Fonctions utiles au plateau de jeu
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

plateau_initial([
    [], [], [], [], [], [], []
]).

piece_a(Board, NumLigne, NumColonne, Piece) :-
    nth1(NumColonne, Board, Colonne),
    nth1(NumLigne, Colonne, Piece),
    !.
piece_a(_, _, _, vide) :- !. % Si on n'a pas trouvé de pièce, c'est que la case est vide

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Affichage du plateau
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
symbole(vide, '_').
symbole(x, 'X').
symbole(o, 'O').

affiche_ligne(Board, Ligne) :-
    write('| '),
    affiche_colonnes(Board, Ligne, 1),
    write('|'), nl.

affiche_colonnes(_, _, 8) :- !.
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

win(Board) :- 
    win_ligne(Board); 
    win_colonne(Board); 
    win_diago(Board). 

alignees(P, P, P, P) :-
    P \= vide.

win_ligne(Board) :-
    between(1, 6, Ligne),
    between(1, 4, Col),
    Col1 is Col + 1,
    Col2 is Col + 2,
    Col3 is Col + 3,
    piece_a(Board, Ligne, Col,  P1),
    piece_a(Board, Ligne, Col1, P2),
    piece_a(Board, Ligne, Col2, P3),
    piece_a(Board, Ligne, Col3, P4),
    alignees(P1, P2, P3, P4).

win_colonne(Board) :-
    between(1, 3, Ligne),
    between(1, 7, Col),
    Ligne1 is Ligne + 1,
    Ligne2 is Col + 2,
    Ligne3 is Col + 3,
    piece_a(Board, Ligne, Col,  P1),
    piece_a(Board, Ligne2, Col, P2),
    piece_a(Board, Ligne3, Col, P3),
    piece_a(Board, Ligne4, Col, P4),
    alignees(P1, P2, P3, P4).
