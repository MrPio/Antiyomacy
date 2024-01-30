:- module(minimax, [minimax/4,
    board_map/2,
    change_board_provinces/3,
    board_provinces/2,
    change_board_map/3,
    board_player/2,
    board_state/2]).
:- use_module([game, hex, province, eval]).

% State struct ====================================================================
board(_Map, Provinces, Player, State) :-
    player(Player),
    is_list(Provinces),
    state(State).
state(X):-member(X,[play, win]).

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

% Calculating Minimax with alpha-beta pruning
% minimax(+Board, +AlphaBeta, +Depth, -BestBoardVal)
minimax(Board, AlphaBeta, Depth, [BestBoard, Val]) :-
    Depth > 0,
    setof(NextBoard, move(Board, NextBoard), NextBoards), !,
    length(NextBoards, NextBoardsLength),
    %format('(~w) Found ~w moves.',[Depth, NextBoardsLength]),nl,
    best_board(NextBoards, AlphaBeta, Depth, [BestBoard, Val])
;   % The depth has expired or there are no available moves
    eval(Board,Val)
    %format(' ~w ', Val)
    .

% Finds the BestBoard from a list of possible successors, whose Val always
% remains within the Alpha and Beta limits
% best_board(+Boards, +AlphaBeta, +Depth, -BestBoardVal)
best_board([Board|TBoards], [Alpha, Beta], Depth, BestBoardVal) :-
    NewDepth is Depth - 1,
    minimax(Board, [Alpha, Beta], NewDepth, [_, Val]), % Get (Val)
    best_board_(TBoards, [Alpha, Beta], [Board, Val], Depth, BestBoardVal).

% Evaluate one Board checking for any alpha or beta cuts
% best_board_(+LeftBoards, +AlphaBeta, +BoardVal, +Depth, -EnoughBoardVal)
best_board_([], _, BoardVal, _, BoardVal) :- !.
best_board_(_, [Alpha, Beta], [Board, Val], _, [Board, Val]) :-
    % Check beta test condition
    is_turn(Board, min), 
    Val > Beta, !
    ;
    % Check alpha test condition
    is_turn(Board, max),
    Val < Alpha, !.
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
% better(+BoardVal1, +BoardVal2, -BestBoardVal)
better([Board, Val], [_, Val1], [Board, Val]) :-
    is_turn(Board, min),
    Val > Val1, !
    ;
    is_turn(Board, max),
    Val < Val1, !.
better(_, BoardVal, BoardVal).

% red is MIN and blue is MAX
is_turn(Board, min):- board_player(Board, red), !.
is_turn(Board, max):- board_player(Board, blue).
