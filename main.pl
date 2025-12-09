:- use_module(utils, [
    affiche_plateau/1,
    jouer_coup/4,
    colonne_disponible/2,
    win/1, 
    win/3,
    plateau_plein/1,
    piece_a/4,
    choisir_coup/3
]).
:- use_module(ai_naive, [ia_naive/2]).

% :- use_module(ia_minimax, [coup_ia/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Fonctions utiles au plateau de jeu
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic board/1. % Permet l'assertion et le retrait de faits board/1

plateau_initial([
    [], [], [], [], [], [], []
]).


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
        writeln('Egalité ! Plateau plein.'),
        retract(board(Board)), !  % Nettoie le plateau
    ; 
        % Le jeu continue - le joueur actuel joue
        write('Au tour de : '), writeln(Player),
        choisir_coup(Board, Player, Colonne), % demande le coup (IA ou humain)
        jouer_coup(Board, Colonne, Player, NewBoard), % joue le coup
        retract(board(Board)), % retire l'ancien plateau
        assert(board(NewBoard)), % stocke le nouveau plateau
        
        % Calcule la ligne où la pièce a atterri
        nth1(Colonne, NewBoard, ColonneJouee),
        length(ColonneJouee, Ligne),
        
        % Vérifie si ce joueur vient de gagner
        (win(NewBoard, Ligne, Colonne) -> 
            affiche_plateau(NewBoard),
            write('Joueur '), write(Player), writeln(' gagne !'),
            retract(board(NewBoard)), !  % Nettoie le plateau
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
