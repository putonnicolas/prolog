% Déclarations pour éviter les warnings d'éditeur (prédicats définis dans utils.pl)
:- dynamic colonne_disponible/2.
:- dynamic jouer_coup/4.
:- dynamic win/1.
:- dynamic win/3.
:- dynamic plateau_plein/1.
:- dynamic piece_a/4.
:- dynamic changePlayer/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MINIMAX SIMPLE ET FONCTIONNEL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

max_player(o). % le joueur à maximiser est l'IA donc "o"

% Matrice de pondération
matrice_ponderation([
    [3, 4, 5, 7, 5, 4, 3],
    [4, 6, 8, 10, 8, 6, 4],
    [5, 8, 11, 14, 11, 8, 5],
    [5, 8, 11, 14, 11, 8, 5],
    [4, 6, 8, 10, 8, 6, 4],
    [3, 4, 5, 7, 5, 4, 3]
]).

% Retourne la valeur à la case C de la ligne L
obtenir_poid(L, C, Valeur) :-
    matrice_ponderation(Matrice),
    nth1(L, Matrice, Liste_Ligne), % Récupère la ligne L de la matrice
    nth1(C, Liste_Ligne, Valeur). % Récupère la colonne C de la ligne L et le met dans valeur 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% POINT D'ENTRÉE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ia_minimax(Board, Colonne, Depth, AIPlayer) :-
    changePlayer(AIPlayer, Opp),
    findall(Col, colonne_disponible(Board, Col), Columns), % Chercher tous les coups jouables
    
    % 1. Chercher coup gagnant
    (   member(Col, Columns), % on test pour chaque coup possible 
        jouer_coup(Board, Col, AIPlayer, TestBoard), % s'il permettrait de gagner
        win(TestBoard) ->
        Colonne = Col, !
    
    % 2. Bloquer l'adversaire
    ;   member(Col, Columns),
        jouer_coup(Board, Col, Opp, TestBoardOpponent),
        win(TestBoardOpponent) ->
        Colonne = Col, !
    
    % 3. Minimax
    ;   evaluer_coups(Columns, Depth, AIPlayer, AIPlayer, Board, ScoredMoves),
        meilleur_coup(AIPlayer, AIPlayer, ScoredMoves, Colonne)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ÉVALUATION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Évaluer tous les coups possibles à une profondeur donnée
evaluer_coups([], _, _, _, _, []).
% Score-Col est une structure
evaluer_coups([Col|Cols], Depth, AIPlayer, Player, Board, [Score-Col|Rest]) :-
    minimax(Board, Col, AIPlayer, Player, Depth, Score),
    evaluer_coups(Cols, Depth, AIPlayer, Player, Board, Rest).

%% Algorithme Minimax principal
minimax(Board, Col, AIPlayer, Player, Depth, Score) :-
    jouer_coup(Board, Col, Player, NewBoard),
    
    % Cas terminaux
    (   win(NewBoard) ->
        (   Player == AIPlayer ->
            % On ajoute Depth pour maximiser le choix de gagner au plus tôt
            Score is 1000 + Depth
        ;
            % Si c'est pour l'adversaire
            Score is -1000 - Depth
        )
    
    ;   plateau_plein(NewBoard) ->
        Score is 0
    
    ;   Depth =< 0 ->
        heuristique(NewBoard, AIPlayer, Score)
    
    % Récursion
    ;   changePlayer(Player, Opp),
        D1 is Depth - 1,
        findall(C, colonne_disponible(NewBoard, C), NextCols),
        (   % Si y'a plus de coups
            NextCols = [] ->
            Score is 0
        ;
            % Explorer la suite de la branche
            evaluer_coups(NextCols, D1, AIPlayer, Opp, NewBoard, Scored),
            meilleur_score(AIPlayer, Opp, Scored, Score)
        )
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SÉLECTION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Trouver le meilleur score dans une liste de scores
meilleur_score(_, _, [], 0) :- !.
meilleur_score(AIPlayer, Player, Scored, Best) :-
    extraire_scores(Scored, Scores),
    (   Player == AIPlayer ->
        % Maximiser si c'est le tour de l'IA
        max_list(Scores, Best)
    ;
        % Minimiser si c'est le tour de l'adversaire
        min_list(Scores, Best)
    ).

extraire_scores([], []).
% S s'unifie avec le score et on ne regarde pas le coup
extraire_scores([S-_|R], [S|Ss]) :-
    extraire_scores(R, Ss).

meilleur_coup(_, _, [], 4) :- !.
meilleur_coup(AIPlayer, Player, Scored, Col) :-
    meilleur_score(AIPlayer, Player, Scored, Best),
    findall(C, member(Best-C, Scored), BestCols), % plusieurs colonnes peuvent avoir le même score
    (   % peu probable, mais si y'a pas de meilleur coup disponible on joue le 4
        BestCols = [] ->
        Col = 4
    ;
        % Sinon, on joue un coup aléatoire parmi les meilleurs
        length(BestCols, N),
        random(0, N, I),
        nth0(I, BestCols, Col)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HEURISTIQUE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% quand l'IA évalue un plateau, elle calcule la somme des poids de ses pièces - celles adverses
heuristique(Board, AIPlayer, Score) :-
    changePlayer(AIPlayer, Opp),
    calc_poids(Board, AIPlayer, PIA),
    calc_poids(Board, Opp, PAdv),
    RawScore is PIA - PAdv, % différence > 0 : avantageux pour l'IA, < 0 : avantageux pour le player. 
    (   RawScore > 100 -> Score is 100
    ;   RawScore < -100 -> Score is -100
    ;   Score is RawScore
    ).

%% Calculer le poids total des pièces d'un joueur sur le plateau
calc_poids(Board, Player, Total) :-
    findall(Poids, (
        between(1, 6, L),
        between(1, 7, C),
        piece_a(Board, L, C, Player),
        obtenir_poid(L, C, Poids)
    ), Poids_List),
    sumlist(Poids_List, Total).