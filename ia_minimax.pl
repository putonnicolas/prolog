:- module(ia_minimax, [coup_ia/3]).
:- use_module(utils, [piece_a/4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Définition de l'heuristique
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
matrice_pondération([
    [3, 4, 5, 7, 5, 4, 3],  % Ligne 1 (en haut)
    [4, 6, 8, 10, 8, 6, 4], % Ligne 2
    [5, 8, 11, 14, 11, 8, 5],% Ligne 3
    [5, 8, 11, 14, 11, 8, 5],% Ligne 4
    [4, 6, 8, 10, 8, 6, 4], % Ligne 5
    [3, 4, 5, 7, 5, 4, 3]   % Ligne 6 (en bas)
]).

% obtenir_poid(Ligne, Colonne, Valeur)
obtenir_poid(L, C, Valeur) :-
    matrice_pondération(Matrice),
    % Récupère la L-ième ligne (Liste_Ligne)
    nth1(L, Matrice, Liste_Ligne),
    % Récupère la C-ième valeur dans cette ligne
    nth1(C, Liste_Ligne, Valeur).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Minimax
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implémentation de l'IA Minimax
coup_ia(Board, Move, Depth) :-
    true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Calcul des poids
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% calculer_poids_colonne(Board, Col, Player, Adversaire, TotalPoidsJ, TotalPoidsA)
calculer_poids_colonne(Board, Col, Player, Adversaire, TotalPoidsJ, TotalPoidsA) :-
    calculer_poids_ligne_rec(Board, Col, Player, Adversaire, 1, 0, 0, TotalPoidsJ, TotalPoidsA).

% --- Récursion sur les Lignes (Lignes 1 à 6) ---
% AccJ = Accumulateur de l'IA
% AccA = Accumulateur de l'adversaire 
% --- Cas d'Arrêt Ligne (Ligne 7 atteinte) ---
calculer_poids_ligne_rec(_, _, _, _, 7, AccJ, AccA, AccJ, AccA) :- !.

% --- Cas Récursif (Lignes 1 à 6) ---
calculer_poids_ligne_rec(Board, Col, Player, Adversaire, L, AccJ, AccA, TotalPoidsJ, TotalPoidsA) :-
    
    % 1. Évaluer la case (L, Col) : SI OCCUPÉE ALORS ... ELSE ...
    (
        (piece_a(Board, L, Col, Piece), Piece \= vide) ->
            % SI la case est OCCUPÉE :
            
            obtenir_poid(L, Col, Poids),
            
            % 2. Mettre à jour les accumulateurs (IF imbriqué)
            (Piece = Player -> 
                NewAccJ is AccJ + Poids,
                NewAccA is AccA 
            ; % SINON (c'est l'Adversaire) :
                NewAccJ is AccJ,
                NewAccA is AccA + Poids
            )
        ; % SINON (la case est VIDE) :
            % Si la case est vide, les accumulateurs ne changent pas
            NewAccJ = AccJ,
            NewAccA = AccA
    ),
    
    % 3. Passer à la ligne suivante
    NextL is L + 1,
    
    % Appel récursif pour la ligne suivante
    calculer_poids_ligne_rec(Board, Col, Player, Adversaire, NextL, NewAccJ, NewAccA, TotalPoidsJ, TotalPoidsA).