:- module(minimax, [count/1, cuts/1,
    minimax/4,
    board_map/2,
    change_board_provinces/3,
    board_provinces/2,
    change_board_map/3,
    board_player/2,
    board_state/2]).
:- use_module([game, hex, province, eval]).

% Board struct ====================================================================
board(_Map, Provinces, Player, State) :-
    player(Player),
    is_list(Provinces),
    state(State).
state(X):-member(X,[play, win]).

:- dynamic(count/1).
count(0).
update_count(Count):-
    retractall(count(_)),
    assert(count(Count)).

:- dynamic(cuts/1).
cuts(0).
update_cuts(Cuts):-
    retractall(cuts(_)),
    assert(cuts(Cuts)).

% Check/Get hex Map
board_map(board(Map, _, _, _),Map).
change_board_map(board(_, Provinces, Player, State),NewMap,board(NewMap, Provinces, Player, State)).
% Check/Get hex Provinces
board_provinces(board(_, Provinces, _, _),Provinces).
change_board_provinces(board(Map, _, Player, State),NewProvinces,board(Map, NewProvinces, Player, State)).
% Check/Get hex Player
board_player(board(_, _, Player, _),Player).
% Check/Get hex State
board_state(board(_, _, _, State),State).

% Finds the best move using minimax with alpha-beta pruning
% minimax(+Board, +AlphaBeta, +Depth, -BestBoardVal)
minimax(Board, AlphaBeta, Depth, [BestBoard, Val]) :-
    Depth > 0,
    setof(NextBoard, move(Board, NextBoard), NextBoards), !,
    length(NextBoards, NextBoardsLength),
    % count(Count),NewCount is Count + NextBoardsLength,update_count(NewCount),
    format('(~w) Found ~w moves.',[Depth, NextBoardsLength]),nl,
    best_board(NextBoards, AlphaBeta, Depth, [BestBoard, Val])
;   % The depth has expired or there are no available moves
    eval(Board,Val)
    %format(' ~w ', Val)
    .

% Finds the best board from a list of possible successors, whose value always
% remains within the Alpha and Beta limits
% best_board(+Boards, +AlphaBeta, +Depth, -BestBoardVal)
best_board([Board|TBoards], [Alpha, Beta], Depth, BestBoardVal) :-
    NewDepth is Depth - 1,
    minimax(Board, [Alpha, Beta], NewDepth, [_, Val]), % Get (Val)
    best_board_(TBoards, [Alpha, Beta], [Board, Val], Depth, BestBoardVal).

% Evaluate one Board checking for any alpha or beta cuts
% best_board_(+LeftBoards, +AlphaBeta, +BoardVal, +Depth, -EnoughBoardVal)
best_board_([], _, BoardVal, _, BoardVal) :- !.
best_board_(_LeftBoards, [Alpha, Beta], [Board, Val], _, [Board, Val]) :-
    % Check beta test condition
    is_turn(Board, min), 
    Val > Beta,
    % length(LeftBoards, LeftBoardsLength),
    % cuts(Cuts),NewCuts is Cuts + LeftBoardsLength,update_cuts(NewCuts), 
    !
    ;
    % Check alpha test condition
    is_turn(Board, max),
    Val < Alpha, 
    % length(LeftBoards, LeftBoardsLength),
    % cuts(Cuts),NewCuts is Cuts + LeftBoardsLength,update_cuts(NewCuts), 
    !.
best_board_(LeftBoards, AlphaBeta, BoardVal, Depth, EnoughBoardVal)  :-
  update_alpha_beta(AlphaBeta, BoardVal, NewAlphaBeta),
  best_board(LeftBoards, NewAlphaBeta, Depth, OtherBestBoardVal),
  better(BoardVal, OtherBestBoardVal, EnoughBoardVal).

% Updates the values of Alpha and Beta
% update_alpha_beta(+AlphaBeta, +BoardVal, -NewAlphaBeta) ========================
update_alpha_beta([Alpha, Beta], [Board, Val], [Val, Beta])  :-
    % MIN increases Alpha
    is_turn(Board, min), Val > Alpha, !.
update_alpha_beta([Alpha, Beta], [Board, Val], [Alpha, Val])  :-
    % MAX decreases Beta
    is_turn(Board, max), Val < Beta, !.
update_alpha_beta(AlphaBeta, _, AlphaBeta).

% Returns the best configuration based on the player's turn
% Note: in case of equality, a random board is choosen
% better(+BoardVal1, +BoardVal2, -BestBoardVal)
better([Board, Val], [_, Val1], [Board, Val]) :-
    is_turn(Board, min),
    Val > Val1, !
    ;
    is_turn(Board, max),
    Val < Val1, !.
better(BoardVal1, BoardVal2, BoardVal):-
    random_member(BoardVal, [BoardVal1, BoardVal2]).

% red is MIN and blue is MAX
is_turn(Board, min):- board_player(Board, red), !.
is_turn(Board, max):- board_player(Board, blue).
