% AI benchmarking harness for Puissance4
% Usage examples:
% ?- [ai_bench].
% ?- bench_games(random, minimax, 0, 4, 100, Stats).
% ?- bench_games(niveau1, minimax, 0, 3, 200, Stats).

% Declarations to avoid editor warnings for predicates defined in other files
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


:- use_module(library(lists)).

% Dispatch AI move by name
ai_move(random, Board, _Player, _Depth, Move) :-
    % capture any IA prints to avoid polluting the benchmark output
    with_output_to(string(_), ia_random(Board, Move)).
ai_move(naive, Board, _Player, _Depth, Move) :-
    with_output_to(string(_), ia_naive(Board, Move)).
ai_move(niveau1, Board, _Player, _Depth, Move) :-
    with_output_to(string(_), ia_niveau1(Board, Move)).
ai_move(minimax, Board, Depth, Move) :-
    with_output_to(string(_), ia_minimax(Board, Move, Depth)).

% Fallback: if AI returned invalid move, pick first available
valid_move_or_first(Board, MoveIn, MoveOut) :-
    ( integer(MoveIn), between(1,7,MoveIn), colonne_disponible(Board, MoveIn) -> MoveOut = MoveIn
    ; findall(C, colonne_disponible(Board, C), Cs), (Cs = [H|_] -> MoveOut = H ; MoveOut = 1)
    ).


% Play one full game between AI_X (plays x) and AI_O (plays o)
play_one_game(AI_X, AI_O, DepthX, DepthO, Starter, Winner, Moves) :-
    plateau_initial(B0),
    ( Starter == o -> Curr = o ; Curr = x ),
    ( play_loop(B0, Curr, AI_X, AI_O, DepthX, DepthO, 0, Winner, Moves)
    -> true
    ;  writeln('DEBUG: play_loop failed for play_one_game/7 — returning draw,0'), Winner = draw, Moves = 0
    ).

play_loop(Board, _Curr, _AIx, _AIo, _Dx, _Do, Count, Winner, Count) :-
    win(Board, L, C), !, 
    piece_a(Board, L, C, Winner).
play_loop(Board, _Curr, _AIx, _AIo, _Dx, _Do, Count, draw, Count) :-
    plateau_plein(Board), !.
play_loop(Board, Curr, AIx, AIo, Dx, Do, Count, Winner, Moves) :-
    % Choose AI for current player
    ( Curr == x -> AI = AIx, Depth = Dx ; AI = AIo, Depth = Do ),
    ( catch(ai_move(AI, Board, Curr, Depth, RawMove), _, fail) -> true ; RawMove = _ ),
    valid_move_or_first(Board, RawMove, Move),
    (   jouer_coup(Board, Move, Curr, NewBoard) ->
        Count1 is Count + 1,
        ( Curr == x -> Next = o ; Next = x ),
        play_loop(NewBoard, Next, AIx, AIo, Dx, Do, Count1, Winner, Moves)
    ;   % if move couldn't be played for some reason, pick next available and continue
        findall(C, colonne_disponible(Board, C), Cs),
        ( Cs = [H|_] -> jouer_coup(Board, H, Curr, NewBoard), Count1 is Count + 1, (Curr==x->Next=o;Next=x), play_loop(NewBoard, Next, AIx, AIo, Dx, Do, Count1, Winner, Moves)
        ; % no moves
          Winner = draw, Moves = Count
        )
    ).

% Run N games and aggregate results (robust: catch play failures)
bench_games(AI_X, AI_O, DepthX, DepthO, N, Stats) :-
    bench_games_loop(N, AI_X, AI_O, DepthX, DepthO, 0-0-0-0, 0, FinalAcc),
    FinalAcc = WinsX-WinsO-Draws-TotalMoves,
    ( N > 0 -> AvgMoves is TotalMoves / N ; AvgMoves = 0 ),
    Stats = stats{ai_x:AI_X, ai_o:AI_O, depth_x:DepthX, depth_o:DepthO, games:N, wins_x:WinsX, wins_o:WinsO, draws:Draws, avg_moves:AvgMoves}.

bench_games_loop(0, _AI_X, _AI_O, _Dx, _Do, Acc, _I, Acc) :- !.
bench_games_loop(N, AI_X, AI_O, Dx, Do, Acc, I, Final) :-
    N > 0,
    Starter = (I mod 2 =:= 0 -> x ; o),
    % protect game execution from exceptions
    ( catch(play_one_game(AI_X, AI_O, Dx, Do, Starter, Winner0, Moves0), _, (Winner0 = draw, Moves0 = 0)) -> true ; (Winner0 = draw, Moves0 = 0) ),
    % normalize Winner
    ( var(Winner0) -> Winner = draw ; Winner = Winner0 ),
    Moves = Moves0,
    Acc = WX-WO-DT-TM,
    ( Winner == x -> WX1 is WX + 1, WO1 = WO, DT1 = DT
    ; Winner == o -> WO1 is WO + 1, WX1 = WX, DT1 = DT
    ; % draw or other
      DT1 is DT + 1, WX1 = WX, WO1 = WO
    ),
    TM1 is TM + Moves,
    N1 is N - 1,
    I1 is I + 1,
    bench_games_loop(N1, AI_X, AI_O, Dx, Do, WX1-WO1-DT1-TM1, I1, Final).

% Convenience: run a sweep over depths for minimax vs some opponent
bench_depth_sweep(AI, Opp, Depths, N, Results) :-
    maplist({AI,Opp,N}/[D,Stat]>>bench_games(AI, Opp, D, 0, N, Stat), Depths, Results).

% Helper to pretty print stats
print_stats(Stats) :-
    format('~w vs ~w (depths ~w/~w) - games: ~w~n', [Stats.ai_x, Stats.ai_o, Stats.depth_x, Stats.depth_o, Stats.games]),
    format('wins_x: ~w, wins_o: ~w, draws: ~w, avg_moves: ~2f~n', [Stats.wins_x, Stats.wins_o, Stats.draws, Stats.avg_moves]).

% Example runners
% ?- bench_games(random, minimax, 0, 4, 100, S), print_stats(S).
% ?- bench_games(niveau1, minimax, 0, 3, 200, S), print_stats(S).
% ?- bench_depth_sweep(minimax, random, [1,2,3,4], 100, Results).% returns list of stats

