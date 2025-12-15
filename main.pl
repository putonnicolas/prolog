% Déclarations pour éviter les warnings d'éditeur (prédicats définis dans utils.pl)
:- dynamic affiche_plateau/1.
:- dynamic plateau_plein/1.
:- dynamic choisir_coup/3.
:- dynamic jouer_coup/4.
:- dynamic win/3.
:- dynamic plateau_initial/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Fonctions utiles au plateau de jeu
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic board/1. % Permet l'assertion et le retrait de faits board/1

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Changer de joueur
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
changePlayer(x, o).
changePlayer(o, x).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Boucle principale du jeu
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
play(Player):- 
    board(Board), % récupère le plateau depuis la base de connaissances
    affiche_plateau(Board),
    
    % Vérifie d'abord si le plateau est plein (match nul)
    (plateau_plein(Board) -> 
        writeln('Egalité ! Plateau plein.'),
        retract(board(Board)), !  % Nettoie le plateau
    ; 
        % Le jeu continue - le joueur actuel joue
        write('Au tour de : '), writeln(Player),
        choisir_coup(Board, Player, IdxColonne), % demande le coup (IA ou humain)
        jouer_coup(Board, IdxColonne, Player, NewBoard), % joue le coup
        retract(board(Board)), % retire l'ancien plateau
        assert(board(NewBoard)), % stocke le nouveau plateau
        
        % Calcule la ligne où la pièce a atterri
        nth1(IdxColonne, NewBoard, ColonneJouee),
        length(ColonneJouee, IdxLigne),
        
        % Vérifie si le joueur vient de gagner
        (win(NewBoard, IdxLigne, IdxColonne) -> 
            affiche_plateau(NewBoard),
            write('Joueur '), write(Player), writeln(' gagne !'),
            retract(board(NewBoard)), !  % Nettoie le plateau
        ;
            % Continue avec le joueur suivant
            changePlayer(Player, NextPlayer),
            play(NextPlayer)
        )
    ).




%%%%% Lancement du jeu ! 
init :- 
    plateau_initial(Board), 
    assert(board(Board)), 
    writeln('=== Puissance 4 ==='),
    play(x).
