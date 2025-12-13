:- dynamic ia_niveau1/2.  % Prédicat utilisé depuis ai_naive.pl


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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Plateau initial
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
plateau_initial([ [],[],[],[],[],[],[] ]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Check les pièces
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Permet de récupérer la pièce à une position donnée (ligne, colonne)
piece_a(Board, NumLigne, NumColonne, Piece) :-
    nth1(NumColonne, Board, Colonne),
    nth1(NumLigne, Colonne, Piece),
    !.
piece_a(_, _, _, vide) :- !. % Si on n'a pas trouvé de pièce, c'est que la case est vide

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Choisir un coup
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
choisir_coup(Board, x, Colonne) :-
    repeat,
        write('Joueur x, choisis un colonne (1-7): '),
        read(C),
        (   integer(C), between(1,7,C), colonne_disponible(Board, C)
        ->  Colonne = C,
            retractall(last_human_move(_)),
            assert(last_human_move(Colonne)),
            !
        ;   writeln('Mouvement invalide ! Recommence.'),
            fail
        ).

choisir_coup(Board, o, Colonne) :-
    % IA (O)
    ia_niveau1(Board, Colonne).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Jouer un coup
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

jouer_coup(Board, NumColonne, Player, NewBoard) :-
    nth1(NumColonne, Board, Colonne), % Récupère la colonne choisie
    length(Colonne, Hauteur), % Calcule la hauteur actuelle de la colonne
    Hauteur < 6, % Vérifier que le coup peut être joué
    append(Colonne, [Player], NewColonne), % Ajoute la pièce du dans la liste de la colonne
    replace_colonne(Board, NewBoard, NumColonne, NewColonne), !. % Remplace la colonne dans le plateau

% Met à jour le plateau avec la nouvelle colonne
replace_colonne([_ | T], [NewColonne | T], 1, NewColonne). % Cas de base la colonne à remplacer est en position 1
replace_colonne([HToKeep | Tail], [HToKeep | NewTail], NumColonne, NewColonne) :- % On décrémente le numéro de colonne jusqu'à atteindre 1
    NewNum is NumColonne - 1,
    replace_colonne(Tail, NewTail, NewNum, NewColonne).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Victoire
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

win(Board) :- 
    between(1, 6, Ligne),
    between(1, 7, Colonne),
    win(Board, Ligne, Colonne).

win(Board, Ligne, Colonne) :- 
    ( win_ligne(Board, Ligne);
    win_colonne(Board, Colonne);
    win_diago_sens1(Board, Ligne, Colonne);
    win_diago_sens2(Board, Ligne, Colonne)), !.

alignees(P, P, P, P) :-
    P \= vide.

win_ligne(Board, Ligne) :-
    between(1, 4, Col),
    Col2 is Col + 1,
    Col3 is Col + 2,
    Col4 is Col + 3,
    piece_a(Board, Ligne, Col,  P1),
    piece_a(Board, Ligne, Col2, P2),
    piece_a(Board, Ligne, Col3, P3),
    piece_a(Board, Ligne, Col4, P4),
    alignees(P1, P2, P3, P4), !.

win_colonne(Board, Colonne) :-
    between(1, 3, Ligne),
    Ligne2 is Ligne + 1,
    Ligne3 is Ligne + 2,
    Ligne4 is Ligne + 3,
    piece_a(Board, Ligne, Colonne,  P1),
    piece_a(Board, Ligne2, Colonne, P2),
    piece_a(Board, Ligne3, Colonne, P3),
    piece_a(Board, Ligne4, Colonne, P4),
    alignees(P1, P2, P3, P4), !.

win_diago_sens1(Board, Ligne, Colonne) :-
    piece_a(Board, Ligne, Colonne, P),
    P \= vide,  % La pièce ne doit pas être vide
    recule_diag1(Board, Ligne, Colonne, P, L0, C0),
    compte_diag1(Board, L0, C0, P, Compte),
    Compte >= 4, !.

recule_diag1(Board, L, C, P, L0, C0) :-
    L1 is L - 1,
    C1 is C - 1,
    L1 >= 1, C1 >= 1,
    piece_a(Board, L1, C1, P), !,
    recule_diag1(Board, L1, C1, P, L0, C0).
recule_diag1(_, L, C, _, L, C).

compte_diag1(Board, L, C, P, Count) :-
    avance_diag1(Board, L, C, P, 1, Count).

avance_diag1(Board, L, C, P, Acc, Count) :-
    L1 is L + 1,
    C1 is C + 1,
    L1 =< 6, C1 =< 7,
    piece_a(Board, L1, C1, P), !,
    Acc1 is Acc + 1,
    avance_diag1(Board, L1, C1, P, Acc1, Count).
avance_diag1(_, _, _, _, Count, Count).

win_diago_sens2(Board, Ligne, Colonne) :-
    piece_a(Board, Ligne, Colonne, P),
    P \= vide,  % La pièce ne doit pas être vide
    recule_diag2(Board, Ligne, Colonne, P, L0, C0),
    compte_diag2(Board, L0, C0, P, Compte),
    Compte >= 4, !.

recule_diag2(Board, L, C, P, L0, C0) :-
    L1 is L + 1,
    C1 is C - 1,
    L1 =< 6, C1 >= 1,
    piece_a(Board, L1, C1, P), !,
    recule_diag2(Board, L1, C1, P, L0, C0).
recule_diag2(_, L, C, _, L, C).

compte_diag2(Board, L, C, P, Count) :-
    avance_diag2(Board, L, C, P, 1, Count).

avance_diag2(Board, L, C, P, Acc, Count) :-
    L1 is L - 1,
    C1 is C + 1,
    L1 >= 1, C1 =< 7,
    piece_a(Board, L1, C1, P), !,
    Acc1 is Acc + 1,
    avance_diag2(Board, L1, C1, P, Acc1, Count).
avance_diag2(_, _, _, _, Count, Count).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Plateau plein
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
plateau_plein(Board) :-
    \+ colonne_disponible(Board, _). % Négation de l'existence d'une colonne disponible

colonne_disponible(Board, NumCol) :- 
    between(1, 7, NumCol), % Teste les colonnes de 1 à 7
    nth1(NumCol, Board, Colonne), % Récupère la colonne correspondante
    length(Colonne, Hauteur), % Calcule la hauteur de la colonne
    Hauteur < 6. % Vérifie si la colonne n'est pas pleine
