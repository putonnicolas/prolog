%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MINIMAX SIMPLE ET FONCTIONNEL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

max_player(o). 
opponent(x, o).
opponent(o, x).

% Matrice de pondération
matrice_ponderation([
    [3, 4, 5, 7, 5, 4, 3],
    [4, 6, 8, 10, 8, 6, 4],
    [5, 8, 11, 14, 11, 8, 5],
    [5, 8, 11, 14, 11, 8, 5],
    [4, 6, 8, 10, 8, 6, 4],
    [3, 4, 5, 7, 5, 4, 3]
]).

obtenir_poid(L, C, Valeur) :-
    matrice_ponderation(Matrice),
    nth1(L, Matrice, Liste_Ligne),
    nth1(C, Liste_Ligne, Valeur).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% POINT D'ENTRÉE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ia_minimax(Board, Colonne, Depth) :-
    max_player(Player),
    opponent(Player, Opp),
    findall(Col, colonne_disponible(Board, Col), Columns),
    
    % 1. Chercher coup gagnant
    (   member(Col, Columns),
        jouer_coup(Board, Col, Player, TestBoard),
        win(TestBoard) ->
        Colonne = Col, !
    
    % 2. Bloquer l'adversaire
    ;   member(Col, Columns),
        jouer_coup(Board, Col, Opp, TestBoard2),
        win(TestBoard2) ->
        Colonne = Col, !
    
    % 3. Minimax
    ;   evaluer_coups(Columns, Depth, Player, Board, ScoredMoves),
        meilleur_coup(Player, ScoredMoves, Colonne)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ÉVALUATION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

evaluer_coups([], _, _, _, []).
evaluer_coups([Col|Cols], Depth, Player, Board, [Score-Col|Rest]) :-
    minimax(Board, Col, Player, Depth, Score),
    evaluer_coups(Cols, Depth, Player, Board, Rest).

minimax(Board, Col, Player, Depth, Score) :-
    jouer_coup(Board, Col, Player, NewBoard),
    
    % Cas terminaux
    (   win(NewBoard) ->
        (   max_player(Player) ->
            Score is 1000 + Depth
        ;
            Score is -1000 - Depth
        )
    
    ;   plateau_plein(NewBoard) ->
        Score is 0
    
    ;   Depth =< 0 ->
        heuristique(NewBoard, Player, Score)
    
    % Récursion
    ;   opponent(Player, Opp),
        D1 is Depth - 1,
        findall(C, colonne_disponible(NewBoard, C), NextCols),
        (   NextCols = [] ->
            Score is 0
        ;
            evaluer_coups(NextCols, D1, Opp, NewBoard, Scored),
            meilleur_score(Opp, Scored, Score)
        )
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SÉLECTION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

meilleur_score(_, [], 0) :- !.
meilleur_score(Player, Scored, Best) :-
    extraire_scores(Scored, Scores),
    (   max_player(Player) ->
        max_list(Scores, Best)
    ;
        min_list(Scores, Best)
    ).

extraire_scores([], []).
extraire_scores([S-_|R], [S|Ss]) :-
    extraire_scores(R, Ss).

meilleur_coup(_, [], 4) :- !.
meilleur_coup(Player, Scored, Col) :-
    meilleur_score(Player, Scored, Best),
    findall(C, member(Best-C, Scored), BestCols),
    (   BestCols = [] ->
        Col = 4
    ;
        length(BestCols, N),
        random(0, N, I),
        nth0(I, BestCols, Col)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HEURISTIQUE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

heuristique(Board, Player, Score) :-
    opponent(Player, Opp),
    calc_poids(Board, Player, PIA),
    calc_poids(Board, Opp, PAdv),
    RawScore is PIA - PAdv,
    (   RawScore > 100 -> Score is 100
    ;   RawScore < -100 -> Score is -100
    ;   Score is RawScore
    ).

calc_poids(Board, Player, Total) :-
    findall(Poids, (
        between(1, 6, L),
        between(1, 7, C),
        piece_a(Board, L, C, Player),
        obtenir_poid(L, C, Poids)
    ), Poids_List),
    sumlist(Poids_List, Total).
