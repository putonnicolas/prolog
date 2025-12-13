:- begin_tests(puissance4_tests).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%       TESTS UNITAIRES    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(plateau_vide) :-
    user:plateau_initial(Board),
    Board == [[], [], [], [], [], [], []].

test(win_colonne) :-
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

test(win_ligne) :-
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

test(win_diagonale) :-
    % Diagonale sens 1 (haut-gauche vers bas-droite):
    % piece_a(Board, 1, 1, x)
    % piece_a(Board, 2, 2, x)
    % piece_a(Board, 3, 3, x)
    % piece_a(Board, 4, 4, x)
    Board = [
        [x],               % Colonne 1: [x] -> position (1,1)
        [o, x],            % Colonne 2: [o, x] -> positions (1,2), (2,2)
        [o, o, x],         % Colonne 3: [o, o, x] -> positions (1,3), (2,3), (3,3)
        [o, o, o, x],      % Colonne 4: [o, o, o, x] -> positions (1,4), (2,4), (3,4), (4,4)
        [],
        [],
        []
    ],
    user:win(Board, 4, 4).

test(board_full) :-
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


:- end_tests(puissance4_tests).
