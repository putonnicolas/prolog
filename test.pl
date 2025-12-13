:- begin_tests(puissance4_tests).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%       TESTS UNITAIRES    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% === Tests de plateau ===

test(plateau_vide) :-
    % Vérifier que le plateau initial est bien vide
    user:plateau_initial(Board),
    Board == [[], [], [], [], [], [], []].

test(board_full) :-
    % Vérifier que plateau_plein/1 détecte un plateau complètement rempli, sans victoire
    Board = [
        [o, o, x, o, x, o],
        [x, x, x, o, x, x],
        [x, o, o, x, x, o],
        [o, x, x, x, o, x],
        [x, o, o, o, x, o],
        [o, x, x, o, x, o],
        [x, x, x, o, x, o]
    ],
    user:plateau_plein(Board).

test(board_not_full) :-
    % Vérifier que plateau_plein/1 rejette un plateau partiellement rempli
    Board = [
        [x, o],
        [x],
        [],
        [],
        [],
        [],
        []
    ],
    \+ user:plateau_plein(Board).


% === Tests de colonnes disponibles ===

test(colonne_disponible_empty) :-
    % Vérifier qu'une colonne vide est disponible
    user:plateau_initial(Board),
    user:colonne_disponible(Board, 1).

test(colonne_disponible_partial) :-
    % Vérifier qu'une colonne partiellement remplie (< 6) est disponible
    Board = [
        [x, o, x], 
        [],
        [],
        [],
        [],
        [],
        []
    ],
    user:colonne_disponible(Board, 1).

test(colonne_not_available_full) :-
    % Vérifier qu'une colonne remplie (6 pièces) n'est pas disponible
    Board = [
        [x, o, x, o, x, o],
        [],
        [],
        [],
        [],
        [],
        []
    ],
    \+ user:colonne_disponible(Board, 1).


% === Tests de coups ===

test(jouer_coup_empty_col) :-
    % Vérifier que jouer dans une colonne vide place la pièce à la bonne position
    user:plateau_initial(Board),
    user:jouer_coup(Board, 1, x, NewBoard),
    user:piece_a(NewBoard, 1, 1, x).

test(jouer_coup_stack) :-
    % Vérifier que les coups s'empilent correctement dans une colonne
    Board = [[x, o], [], [], [], [], [], []],
    user:jouer_coup(Board, 1, x, NewBoard),
    % Après le coup, la colonne 1 doit avoir [x, o, x]
    nth1(1, NewBoard, Col),
    Col == [x, o, x].

test(jouer_coup_refuses_full_col) :-
    % Vérifier que jouer dans une colonne pleine échoue
    Board = [[x, o, x, o, x, o], [], [], [], [], [], []],
    \+ user:jouer_coup(Board, 1, x, _).


% === Tests de victoire en colonne ===

test(win_colonne) :-
    % Vérifier qu'on détecte une victoire verticale
    Board = [
        [x, x, x, x],
        [o, o, o],
        [],
        [],
        [],
        [],
        []
    ],
    user:win(Board, 4, 1).

test(win_colonne_not_four) :-
    % Vérifier qu'on ne compte pas une victoire avec seulement 3 pièces alignées
    Board = [
        [x, x, x],
        [o, o],
        [],
        [],
        [],
        [],
        []
    ],
    \+ user:win(Board, _, _).


% === Tests de victoire en ligne ===

test(win_ligne) :-
    % Vérifier qu'on détecte une victoire horizontale 
    Board = [  
        [o, x],
        [o, x],
        [o],
        [o],
        [],
        [],
        [x]
    ],
    user:win(Board, 1, 3).

test(win_ligne_partial) :-
    % Vérifier qu'une ligne avec seulement 3 pièces alignées n'est pas une victoire
    Board = [
        [x],
        [x],
        [x],
        [o],
        [],
        [],
        []
    ],
    \+ user:win(Board, _, _).

% === Tests de victoire en diagonale ===

test(win_diagonale_sens1) :-
    % Vérifier une victoire en diagonale (bas-gauche vers haut-droite)
    Board = [
        [x],               
        [o, x],            
        [x, o, x],         
        [o, o, o, x],      
        [],
        [],
        []
    ],
    user:win(Board, 4, 4).

test(win_diagonale_sens2) :-
    % Vérifier une victoire en diagonale (haut-gauche vers bas-droite) et en annoncant un autre coup que le bout de la diagonale
    Board = [
        [x, x, x, o],
        [x, o, o],
        [x, o],
        [o],
        [],
        [],
        []
    ],
    user:win(Board, 2, 3).

test(win_diagonale_not_four) :-
    % Vérifier qu'on ne compte pas une victoire diagonale avec moins de 4 pièces
    Board = [
        [x],
        [o, x],
        [o, o, x],
        [o],
        [],
        [],
        []
    ],
    \+ user:win(Board, _, _).


% === Tests de piece_a ===

test(piece_a_existing) :-
    % Vérifier la récupération d'une pièce existante
    Board = [[x, o], [o], [], [], [], [], []],
    user:piece_a(Board, 1, 1, x),
    user:piece_a(Board, 2, 1, o),
    user:piece_a(Board, 1, 2, o).

test(piece_a_empty) :-
    % Vérifier qu'une case vide retourne 'vide'
    user:plateau_initial(Board),
    user:piece_a(Board, 1, 1, vide).

test(piece_a_out_of_bounds) :-
    % Vérifier que demander une pièce hors limites retourne 'vide'
    Board = [[x], [], [], [], [], [], []],
    user:piece_a(Board, 10, 10, vide).

:- end_tests(puissance4_tests).
