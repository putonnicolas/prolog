:- begin_tests(puissance4_tests).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%       TESTS UNITAIRES    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%     Etat du plateau       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

test(piece_a_beyond_column_height) :-
    % Vérifier que piece_a retourne 'vide' pour une ligne au-delà de la hauteur d'une colonne
    Board = [[x], [o, x], [], [], [], [], []],
    % Colonne 1 a 1 pièce, donc ligne 3 n'existe pas -> vide
    user:piece_a(Board, 3, 1, vide).

test(piece_a_mixed_columns) :-
    % Vérifier piece_a avec plusieurs colonnes de hauteurs différentes
    Board = [[x, o], [x, x, o], [o], [x], [], [], []],
    user:piece_a(Board, 2, 2, x),    % Colonne 2, ligne 2
    user:piece_a(Board, 3, 2, o),    % Colonne 2, ligne 3
    user:piece_a(Board, 2, 3, vide), % Colonne 3, ligne 2 (n'existe pas)
    user:piece_a(Board, 5, 4, vide). % Colonne 4, ligne 5 (n'existe pas)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%     Jouer un coup         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%     Victoire             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

test(no_win_broken_diag) :-
    % Vérifier qu'une diagonale cassée (3 x + 1 o au milieu) n'est pas une victoire
    Board = [
        [x],
        [o, x],
        [o, o, o],
        [o, o, o, x],
        [],
        [],
        []
    ],
    \+ user:win(Board, _, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests d'intégration : Séquence de coups %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(integration_sequence_no_win) :-
    % Simulation : joueur 1 (x) et joueur 2 (o) jouent plusieurs coups sans victoire
    user:plateau_initial(B0),
    user:jouer_coup(B0, 1, x, B1),
    user:piece_a(B1, 1, 1, x),
    user:jouer_coup(B1, 2, o, B2),
    user:piece_a(B2, 1, 2, o),
    user:jouer_coup(B2, 1, x, B3),
    user:piece_a(B3, 2, 1, x),
    user:jouer_coup(B3, 1, o, B4),
    user:piece_a(B4, 3, 1, o),
    \+ user:win(B4, _, _).

test(integration_sequence_with_win) :-
    % Simulation : joueur x aligne 4 pièces en colonne et gagne
    user:plateau_initial(B0),
    user:jouer_coup(B0, 1, x, B1),
    user:jouer_coup(B1, 2, o, B2), 
    user:jouer_coup(B2, 1, x, B3),
    user:jouer_coup(B3, 2, o, B4),
    user:jouer_coup(B4, 1, x, B5),
    user:jouer_coup(B5, 2, o, B6),
    user:jouer_coup(B6, 1, x, B7),
    %Vérifier la victioire
    user:win(B7, 4, 1),
    % Vérifier que la colonne 1 a bien [x, x, x, x]
    nth1(1, B7, Col1),
    Col1 == [x, x, x, x].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%            Tests des IA                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% === Tests IA Random ===

test(ia_random_validity) :-
    % Vérifier que l'IA random renvoie un coup valide (entre 1 et 7 et colonne disponible)
    user:plateau_initial(Board),
    user:ia_random(Board, Move),
    between(1, 7, Move),
    user:colonne_disponible(Board, Move).

% === Tests IA Naive (Construire des tours) ===

test(ia_naive_validity) :-
    % Vérifier que l'IA naïve renvoie un coup valide (entre 1 et 7 et colonne disponible)
    user:plateau_initial(Board),
    user:ia_naive(Board, Move),
    between(1, 7, Move),
    user:colonne_disponible(Board, Move).

test(ia_naive_sticks_to_target) :-
    % Vérifier que l'IA joue dans sa colonne cible si elle est disponible
    user:plateau_initial(Board),
    retractall(user:ia_target(_)), % Réinitialiser la cible
    assert(user:ia_target(3)), % Définir la cible à la colonne 3
    user:ia_naive(Board, Move),
    Move == 3. % L'IA naïve doit jouer dans sa cible

test(ia_naive_changes_target_when_blocked) :-
    % Vérifier que l'IA change de cible si l'humain vient de jouer dans sa colonne cible
    Board = [
        [x],
        [],
        [],
        [],
        [],
        [],
        []
    ],
    retractall(user:ia_target(_)), % Réinitialiser la cible
    assert(user:ia_target(1)), % Définir la cible à la colonne 1
    retractall(user:last_human_move(_)),
    assert(user:last_human_move(1)), % Simuler que l'humain a joué dans la colonne 1
    user:ia_naive(Board, Move),
    Move \== 1. % L'IA naïve doit changer de cible

% === Tests IA Niveau 1 (Opportuniste) ===

test(ia_niveau_1_validity) :-
    % Vérifier que l'IA joue un coup valide (entre 1 et 7 et colonne disponible)
    user:plateau_initial(Board),
    user:ia_niveau1(Board, Move),
    between(1, 7, Move),
    user:colonne_disponible(Board, Move).

test(ia_niveau1_win) :-
    % Vérifier que l'IA joue un coup gagnant si possible
    Board = [
        [o, o, o],
        [x, x],
        [],
        [],
        [],
        [],
        []
    ],
    user:ia_niveau1(Board, Move),
    Move == 1.  % L'IA doit jouer en colonne 1 pour gagner

test(ia_niveau1_block) :-
    % Vérifier que l'IA bloque le joueur si celui-ci peut gagner au prochain coup
    Board = [
        [x, x, x],
        [o, o],
        [],
        [],
        [],
        [],
        []
    ],
    user:ia_niveau1(Board, Move),
    Move == 1.  % L'IA doit jouer en colonne 1 pour bloquer

test(ia_niveau1_block_autre) :-
    % Vérifier que l'IA bloque le coup du joueur dans une configuration plus compliquée
    Board = [
        [x],
        [o, x],
        [o, o, x],
        [x, o, o],
        [x],
        [],
        []
    ],
    user:ia_niveau1(Board, Move),
    Move == 4.  % L'IA doit jouer en colonne 4 pour bloquer

:- end_tests(puissance4_tests).
