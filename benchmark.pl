%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BENCHMARK DES IA – PUISSANCE 4
%% Permet de faire s’affronter deux IA sur plusieurs parties et de collecter des statistiques (victoires, nulles, nombre de coups).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Exemples d’utilisation :
% ?- [load].
% ?- [benchmark].
% ?- bench_games(random, minimax, 0, 2, 10, S), print_stats(S).


%% Déclarations pour éviter les warnings d’éditeur (prédicats définis dans d’autres fichiers : utils, IA…)
:- dynamic ia_random/2.
:- dynamic ia_naive/2.
:- dynamic ia_niveau1/2.
:- dynamic ia_minimax/3.
:- dynamic colonne_disponible/2.
:- dynamic jouer_coup/4.
:- dynamic plateau_initial/1.
:- dynamic win/3.
:- dynamic piece_a/4.
:- dynamic plateau_plein/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Sélection du coup en fonction du type d’IA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Appelle la bonne IA selon son nom
% L’affichage que font les IA est retiré pour garder un benchmark propre

ai_move(random, Board, _Player, _Depth, Move) :-
    with_output_to(string(_), ia_random(Board, Move)).

ai_move(naive, Board, _Player, _Depth, Move) :-
    with_output_to(string(_), ia_naive(Board, Move)).

ai_move(niveau1, Board, _Player, _Depth, Move) :-
    with_output_to(string(_), ia_niveau1(Board, Move)).

ai_move(minimax, Board, Depth, Move) :-
    with_output_to(string(_), ia_minimax(Board, Move, Depth)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Sécurisation des coups
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% valid_move_or_first(+Board, +MoveIn, -MoveOut)
% Vérifie si le coup proposé est valide
% Sinon, joue la première colonne disponible

valid_move_or_first(Board, MoveIn, MoveOut) :-
    ( integer(MoveIn),
      between(1, 7, MoveIn),
      colonne_disponible(Board, MoveIn)
    -> MoveOut = MoveIn
    ;  findall(C, colonne_disponible(Board, C), Cs),
       ( Cs = [H|_] -> MoveOut = H ; MoveOut = 1 )
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Simulation d’une partie complète
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% play_one_game(+AI_X, +AI_O, +DepthX, +DepthO, +Starter, -Winner, -Moves)
% Lance une partie complète entre deux IA
% Winner ∈ {x, o, draw}
% Moves = nombre total de coups joués

play_one_game(AI_X, AI_O, DepthX, DepthO, Starter, Winner, Moves) :-
    plateau_initial(Board),
    ( Starter == o -> Curr = o ; Curr = x ),
    ( play_loop(Board, Curr, AI_X, AI_O, DepthX, DepthO, 0, Winner, Moves)
    -> true
    ;  writeln('ERREUR : play_loop a échoué'),
       Winner = draw,
       Moves = 0
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Boucle récursive de jeu (sans affichage)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Cas 1 : un joueur a gagné
play_loop(Board, _Curr, _AIx, _AIo, _Dx, _Do, Count, Winner, Count) :-
    win(Board, L, C), !,
    piece_a(Board, L, C, Winner).

% Cas 2 : match nul (plateau plein)
play_loop(Board, _Curr, _AIx, _AIo, _Dx, _Do, Count, draw, Count) :-
    plateau_plein(Board), !.

% Cas général : un joueur joue un coup, puis on continue
play_loop(Board, Curr, AIx, AIo, Dx, Do, Count, Winner, Moves) :-
    % Sélection de l’IA et de la profondeur selon le joueur courant
    ( Curr == x -> AI = AIx, Depth = Dx
    ; Curr == o -> AI = AIo, Depth = Do
    ),

    % Récupération du coup proposé par l’IA
    ( catch(ai_move(AI, Board, Curr, Depth, RawMove), _, fail)
    -> true
    ;  RawMove = _
    ),

    % Sécurisation du coup
    valid_move_or_first(Board, RawMove, Move),

    % Application du coup
    ( jouer_coup(Board, Move, Curr, NewBoard)
    -> Count1 is Count + 1,
       ( Curr == x -> Next = o ; Next = x ),
       play_loop(NewBoard, Next, AIx, AIo, Dx, Do, Count1, Winner, Moves)
    ;  % Sécurité ultime si le coup échoue
       Winner = draw,
       Moves = Count
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Lancement de plusieurs parties et agrégation des résultats
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% bench_games(+AI_X, +AI_O, +DepthX, +DepthO, +N, -Stats)
% Lance N parties et retourne les statistiques globales

bench_games(AI_X, AI_O, DepthX, DepthO, N, Stats) :-
    bench_games_loop(N, AI_X, AI_O, DepthX, DepthO, 0-0-0-0, 0, Acc),
    Acc = WinsX-WinsO-Draws-TotalMoves,
    ( N > 0 -> AvgMoves is TotalMoves / N ; AvgMoves = 0 ),
    Stats = stats{
        ai_x:AI_X,
        ai_o:AI_O,
        depth_x:DepthX,
        depth_o:DepthO,
        games:N,
        wins_x:WinsX,
        wins_o:WinsO,
        draws:Draws,
        avg_moves:AvgMoves
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Boucle récursive de benchmark
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bench_games_loop(0, _AIx, _AIo, _Dx, _Do, Acc, _I, Acc) :- !.

bench_games_loop(N, AIx, AIo, Dx, Do, Acc, I, Final) :-
    N > 0,
    Starter = ( I mod 2 =:= 0 -> x ; o ),

    catch(
        play_one_game(AIx, AIo, Dx, Do, Starter, Winner, Moves),
        _,
        (Winner = draw, Moves = 0)
    ),

    Acc = WX-WO-DT-TM,
    ( Winner == x -> WX1 is WX + 1, WO1 = WO, DT1 = DT
    ; Winner == o -> WO1 is WO + 1, WX1 = WX, DT1 = DT
    ; DT1 is DT + 1, WX1 = WX, WO1 = WO
    ),

    TM1 is TM + Moves,
    N1 is N - 1,
    I1 is I + 1,
    bench_games_loop(N1, AIx, AIo, Dx, Do, WX1-WO1-DT1-TM1, I1, Final).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Affichage des statistiques
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_stats(S) :-
    format('~n====================================~n'),
    format('             RESULTATS~n'),
    format('====================================~n'),
    format('AI X : ~w (depth ~w)~n', [S.ai_x, S.depth_x]),
    format('AI O : ~w (depth ~w)~n~n', [S.ai_o, S.depth_o]),
    format('Games played : ~w~n', [S.games]),
    format('Wins X       : ~w~n', [S.wins_x]),
    format('Wins O       : ~w~n', [S.wins_o]),
    format('Draws        : ~w~n', [S.draws]),
    format('Avg. moves   : ~2f~n', [S.avg_moves]),
    format('====================================~n').
