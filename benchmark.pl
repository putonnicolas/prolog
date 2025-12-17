%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BENCHMARK DES IA – PUISSANCE 4
%% Permet de faire s’affronter deux IA sur plusieurs parties et de collecter des statistiques (victoires, nulles, nombre de coups).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Exemples d’utilisation :
% ?- [load].
% ?- [benchmark].
% ?- bench_games(random, naive, 0, 2, 10, S), print_stats(S).


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
%% Simulation d’une partie complète
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Lance une partie complète entre deux IA
% Winner ∈ {x, o, draw}
% Moves = nombre total de coups joués

play_one_game(AI_X, AI_O, DepthX, DepthO, Starter, Winner, Moves) :-
    plateau_initial(Board),
    ( Starter == o -> Curr = o ; Curr = x ),
    ( play_loop(Board, Curr, AI_X, AI_O, DepthX, DepthO, 0, Winner, Moves)
    -> true
    ;  writeln('ERREUR : play_loop a echoue'),
       Winner = draw,
       Moves = 0
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Boucle récursive de jeu (sans affichage)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

play_loop(Board, Curr, AIx, AIo, Dx, Do, Count, Winner, Moves) :-
    % Choisir le coup automatiquement
    (Curr == x -> AI = AIx, Depth = Dx ; AI = AIo, Depth = Do),
    ai_move(AI, Board, Curr, Depth, Move),
    
    % Appliquer le coup
    jouer_coup(Board, Move, Curr, NewBoard),

    % Calcul de la ligne où la pièce est tombée
    nth1(Move, NewBoard, ColonneJouee),
    length(ColonneJouee, LigneJouee),

    % Vérification victoire, plateau plein ou continuation
    ( win(NewBoard, LigneJouee, Move) ->
        Winner = Curr,
        Moves is Count + 1
    ; plateau_plein(NewBoard) ->
        Winner = draw,
        Moves is Count + 1
    ;
        Count1 is Count + 1,
        changePlayer(Curr, Next),
        play_loop(NewBoard, Next, AIx, AIo, Dx, Do, Count1, Winner, Moves)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Lancement de plusieurs parties et agrégation des résultats
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
