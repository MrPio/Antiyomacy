:- module(minimax, [start_time/1, update_start_time/1,
    minimax/4,
    board/5,
    board_map/2,
    change_board_provinces/3,
    board_provinces/2,
    change_board_map/3,
    board_player/2,
    board_state/2,
    board_conquests/2]).
:- use_module([game, hex, province, eval, utils]).

:- dynamic(start_time/1).
update_start_time(Time):-
    retractall(time(_)),
    assert(start_time(Time)).

% Board struct ====================================================================
board(_Map, Provinces, Player, State, Conquests) :-
    player(Player),
    is_list(Provinces),
    state(State),
    Conquests = [RedConquests, BlueConquests].
state(X) :- member(X, [play, win]).
% Check/Get hex Map
board_map(board(Map, _, _, _, _),Map).
change_board_map(board(_, Provinces, Player, State, Conquests),NewMap,board(NewMap, Provinces, Player, State, Conquests)).
% Check/Get hex Provinces
board_provinces(board(_, Provinces, _, _, _),Provinces).
change_board_provinces(board(Map, _, Player, State, Conquests),NewProvinces,board(Map, NewProvinces, Player, State, Conquests)).
% Check/Get hex Player
board_player(board(_, _, Player, _, _),Player).
% Check/Get hex State
board_state(board(_, _, _, State, _),State).
% Check/Get RedConquests
board_conquests(board(_, _, _, _, Conquests),Conquests).

% Predicate to change the player of a board
% change_board_player(+OldBoard, +NewPlayer, -NewBoard)
change_board_player(board(Map, Provinces, _, State, Conquests), NewPlayer, board(Map, Provinces, NewPlayer, State, Conquests)).


% Finds the best move using minimax with alpha-beta pruning
% minimax(+Board, +AlphaBeta, +Depth, -BestBoardVal)
minimax(Board, AlphaBeta, Depth, [BestBoard, Val]) :-
    Depth > 0,
    start_time(T1), get_time(T2),
    T is T2-T1, T < 10,
    setof(NextBoard, move(Board, NextBoard), NextBoards), !,

    % Print the number of possible moves
    % length(NextBoards, NextBoardsLength),
    %format('(~w) Found ~w moves.',[Depth, NextBoardsLength]),nl,
    
    % Update the count
    % count(Count),NewCount is Count + NextBoardsLength,update_count(NewCount),

    % Print all the possible boards
    % foreach((member(board(Map, Provinces, _, State, Conquests), NextBoards)), (print_map(Map), print_provinces(Provinces), writeln(State), writeln(Conquests))),
    
    best_board(NextBoards, AlphaBeta, Depth, [BestBoard, Val])
;   % The depth has expired or there are no available moves
    eval(Board,Val).

% Finds the best board from a list of possible successors, whose value always
% remains within the Alpha and Beta limits
% best_board(+Boards, +AlphaBeta, +Depth, -BestBoardVal)
best_board([Board|TBoards], [Alpha, Beta], Depth, BestBoardVal) :-
    NewDepth is Depth - 1,
    minimax(Board, [Alpha, Beta], NewDepth, [_, Val]), % Get (Val)
    best_board_(TBoards, [Alpha, Beta], [Board, Val], Depth, BestBoardVal).

% Evaluates one Board checking for any alpha or beta cuts
% best_board_(+LeftBoards, +AlphaBeta, +BoardVal, +Depth, -EnoughBoardVal)
best_board_([], _, BoardVal, _, BoardVal) :- !.
best_board_(_LeftBoards, [Alpha, Beta], [Board, Val], _, [Board, Val]) :-
    % Check beta test condition
    is_turn(Board, min), 
    Val > Beta, !
;   % Check alpha test condition
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

% Returns the best game state based on who is playing
% Note: in case of equality, a random board is chosen
% better(+BoardVal1, +BoardVal2, -BestBoardVal)
better([Board, Val], [_, Val1], [Board, Val]) :-
    is_turn(Board, min),
    Val > Val1, !
    ;
    is_turn(Board, max),
    Val < Val1, !.
better([_, Val], [Board, Val1], [Board, Val1]) :-
    is_turn(Board, min),
    Val < Val1, !
    ;
    is_turn(Board, max),
    Val > Val1, !.
better(BoardVal1, BoardVal2, BoardVal):-
    random_member(BoardVal, [BoardVal1, BoardVal2]).

% red is MIN and blue is MAX
is_turn(Board, min):- board_player(Board, red), !.
is_turn(Board, max):- board_player(Board, blue).
