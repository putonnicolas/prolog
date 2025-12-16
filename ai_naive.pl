%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% IA aléatoire
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ia_random(Board, Move):- 
    repeat,                          % recommencer jusqu'à ce qu'on trouve
    random(1, 8, Move),              % entre 1 et 7 (8 exclu)
    colonne_disponible(Board, Move),  % si la colonne est dispo
    !.   							 % break

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% IA naive : joue dans une colonne jusqu'à qu'elle soit bloquée 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic last_move/1.   % last_move(Column) stocke le dernier coup joué par l'IA, pour rajouer dedans
:- dynamic ia_target/1.  % ia_target(Column) stocke la colonne cible, pour rejouer dedans
:- dynamic last_human_move/1. % last_human_move(Column) stocke le dernier coup joué par l'humain pour changer de cible si joué dans la colonne cible

% Choisir une nouvelle colonne cible valide (en évitant une colonne spécifique par ex si pleine ou que l'humain vient de jouer dedans)
choose_new_target(Board, Target, AvoidTarget) :-
    repeat,
    random(1, 8, T),
    T \= AvoidTarget, % Eviter la colonne en question
    colonne_disponible(Board, T),
    Target = T, !.

ia_naive(Board, Move) :-
    % Récupérer la cible
    ( ia_target(Target) ->
        true
    ;
        choose_new_target(Board, Target, -1), % -1 car aucune colonne à éviter au début
        assert(ia_target(Target))
    ),

    % Si l’humain a joué sur la colonne cible = on change
    ( last_human_move(Target) ->
        choose_new_target(Board, NewTarget, Target), % On évite l'ancienne cible
        retractall(ia_target(_)),
        assert(ia_target(NewTarget)),
        Move = NewTarget,
        !
    ;

      % Sinon on essaie de jouer la colonne cible
      colonne_disponible(Board, Target) ->
        Move = Target, !
    ;

      % Si la colonne cible est pleine on change de cible
      choose_new_target(Board, NewTarget, Target), % On évite l'ancienne cible car pleine
      retractall(ia_target(_)),
      assert(ia_target(NewTarget)),
      Move = NewTarget,
      !
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% IA de niveau 1 : coup gagnant et coup défensif
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simulate_move(Board, Col, Player, SimBoard) :-
    colonne_disponible(Board, Col),
    jouer_coup(Board, Col, Player, SimBoard).

ia_niveau1(Board, Move) :-
    % Option 1. Coup gagnant pour l’IA 
    between(1, 7, Col),
    simulate_move(Board, Col, o, B2),
    win(B2),
    Move = Col,
    !.

ia_niveau1(Board, Move) :-
    % Option 2. Coup défensif en bloquant X si il peut gagner
    between(1, 7, Col),
    simulate_move(Board, Col, x, B2),
    win(B2),
    Move = Col,
    !.

ia_niveau1(Board, Move) :-
    % Option 3. coup random valide
    repeat,
    random(1, 8, Col),
    colonne_disponible(Board, Col),
    Move = Col,
    !.