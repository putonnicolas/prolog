%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% IA aléatoire
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ia_random(Board, Move):- 
    repeat,                          % recommencer jusqu'à ce qu'on trouve
    random(1, 8, Move),              % entre 1 et 7 (8 exclu)
    colonne_disponible(Board, Move),  % si la colonne est dispo
    write('IA joue la colonne : '), writeln(Move),
    !.   							 % break

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% IA naive : joue dans une colonne jusqu'à qu'elle soit bloquée 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic last_move/1.   % last_move(Column) utile pour utiliser l'ia naive
:- dynamic ia_target/1.
:- dynamic last_human_move/1.

% Choisir une nouvelle colonne cible valide
choose_new_target(Board, Target) :-
    repeat,
    random(1, 8, T),
    colonne_disponible(Board, T),
    Target = T, !.

ia_naive(Board, Move) :-
    % Récupérer la cible
    ( ia_target(Target) ->
        true
    ;
        choose_new_target(Board, Target),
        assert(ia_target(Target))
    ),

    % Si l’humain a joué sur la colonne cible = on change
    ( last_human_move(Target) ->
        write('Le humain vient de bloquer ma colonne je change de place'), nl,
        choose_new_target(Board, NewTarget),
        retractall(ia_target(_)),
        assert(ia_target(NewTarget)),
        Move = NewTarget,
        !
    ;

      % Sinon on essaie de jouer la colonne cible
      colonne_disponible(Board, Target) ->
        Move = Target,
        write('La IA joue dans sa colonne pref : '), writeln(Target), !
    ;

      % Si la colonne cible est pleine on change
      write('La colonne pref est pleine, je choisis une nouvelle'), nl,
      choose_new_target(Board, NewTarget),
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
    write('IA joue un coup gagnant en colonne '), writeln(Col), !.

ia_niveau1(Board, Move) :-
    % Option 2. Coup défensif en bloquant X si il peut gagner
    between(1, 7, Col),
    simulate_move(Board, Col, x, B2),
    win(B2),
    Move = Col,
    write('IA bloque le joueur en colonne '), writeln(Col), !.

ia_niveau1(Board, Move) :-
    % Option 3. coup random valide
    repeat,
    random(1, 8, Col),
    colonne_disponible(Board, Col),
    Move = Col,
    write('IA joue aléatoire en colonne '), writeln(Col),
    !.