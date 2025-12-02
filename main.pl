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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Victoire d'un joueur
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
win(Board) :- 
    win_ligne(Board); 
    win_colonne(Board); 
    win_diago_sens1(Board);
    win_diago_sens2(Board). 

alignees(P, P, P, P) :-
    P \= vide.

win_ligne(Board) :-
    between(1, 6, Ligne),
    between(1, 4, Col),
    Col2 is Col + 1,
    Col3 is Col + 2,
    Col4 is Col + 3,
    piece_a(Board, Ligne, Col,  P1),
    piece_a(Board, Ligne, Col2, P2),
    piece_a(Board, Ligne, Col3, P3),
    piece_a(Board, Ligne, Col4, P4),
    alignees(P1, P2, P3, P4).

win_colonne(Board) :-
    between(1, 3, Ligne),
    between(1, 7, Col),
    Ligne2 is Ligne + 1,
    Ligne3 is Ligne + 2,
    Ligne4 is Ligne + 3,
    piece_a(Board, Ligne, Col,  P1),
    piece_a(Board, Ligne2, Col, P2),
    piece_a(Board, Ligne3, Col, P3),
    piece_a(Board, Ligne4, Col, P4),
    alignees(P1, P2, P3, P4).

win_diago_sens1(Board) :-
    between(1, 3, Ligne),
    between(1, 4, Col),
    Col2 is Col + 1,
    Col3 is Col + 2,
    Col4 is Col + 3,
    Ligne2 is Ligne + 1,
    Ligne3 is Ligne + 2,
    Ligne4 is Ligne + 3,
    piece_a(Board, Ligne, Col,  P1),
    piece_a(Board, Ligne2, Col2, P2),
    piece_a(Board, Ligne3, Col3, P3),
    piece_a(Board, Ligne4, Col4, P4),
    alignees(P1, P2, P3, P4).

win_diago_sens2(Board) :-
    between(1, 3, Ligne),
    between(4, 7, Col),
    Col2 is Col - 1,
    Col3 is Col - 2,
    Col4 is Col - 3,
    Ligne2 is Ligne + 1,
    Ligne3 is Ligne + 2,
    Ligne4 is Ligne + 3,
    piece_a(Board, Ligne, Col,  P1),
    piece_a(Board, Ligne2, Col2, P2),
    piece_a(Board, Ligne3, Col3, P3),
    piece_a(Board, Ligne4, Col4, P4),
    alignees(P1, P2, P3, P4).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Plateau plein
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
plateau_plein(Board) :-
    \+ colonne_disponible(Board, _). %% Négation de l'existence d'une colonne disponible

colonne_disponible(Board, NumCol) :- 
    between(1, 7, NumCol), % Teste les colonnes de 1 à 7
    nth1(NumCol, Board, Colonne), % Récupère la colonne correspondante
    length(Colonne, Hauteur), % Calcule la hauteur de la colonne
    Hauteur < 6. % Vérifie si la colonne n'est pas pleine

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Changer de joueur
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
changePlayer(x, o).
changePlayer(o, x).

play(Player):- 
    board(Board), % récupère le plateau depuis la base de connaissances
    affiche_plateau(Board), % affiche le plateau
    
    % Vérifie d'abord si le plateau est plein (match nul)
    (plateau_plein(Board) -> 
        writeln('Draw! Board is full.'), !
    ; 
        % Le jeu continue - le joueur actuel joue
        write('New turn for: '), writeln(Player),
        choisir_coup(Board, Player, Colonne), % demande le coup (IA ou humain)
        jouer_coup(Board, Colonne, Player, NewBoard), % joue le coup
        retract(board(Board)), % retire l'ancien plateau
        assert(board(NewBoard)), % stocke le nouveau plateau
        
        % Vérifie si ce joueur vient de gagner
        (win(NewBoard) -> 
            affiche_plateau(NewBoard),
            write('Player '), write(Player), writeln(' wins!'), !
        ;
            % Continue avec le joueur suivant
            changePlayer(Player, NextPlayer),
            play(NextPlayer)
        )
    ).

%%%%% Start the game! 
init :- 
    plateau_initial(Board), 
    assert(board(Board)), 
    writeln('=== Puissance 4 ==='),
    play(x).