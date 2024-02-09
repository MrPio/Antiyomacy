:- module(game, [move/2, play/0, province_move/7, unit_move/9, resource_buy/9, ask_province_move/7]).
:- use_module([test, utils, map, hex, province, unit, building, economy, eval, minimax, io, gui]).
:- use_module(library(pce)).

% Asks the user to choose a move for each of their provinces
% ask_provinces_moves(+board(Map, Provinces, HumanPlayer, _, Conquests), -board(NewMap, NewProvinces, NewPlayer, NewState, NewConquests)):-
ask_provinces_moves(board(Map, Provinces, HumanPlayer, _, Conquests), board(NewMap, NewProvinces, NewPlayer, NewState, NewConquests)):-
    % Select the provinces owned by the human player
    include([In]>>(province_owner(In, HumanPlayer)), Provinces, HumanProvinces), !,
    % Ask the user for moves for each province
    pipe(ask_province_move, [Map, Provinces, Conquests], HumanProvinces, [NewMap, NewProvinces, NewConquests]),
    % Change the player turn
    other_player(HumanPlayer, NewPlayer),
    % Check if the human player has won
    (   has_won(NewMap, NewProvinces, HumanPlayer)
    ->  NewState = win
    ;   NewState = play
    ).

% Asks the user for a move for a specific province
% ask_province_move(+Map, +Provinces, +[RedConq, BlueConq], +Province, -NewMap, -NewProvinces, -[NewRedConq, NewBlueConq]):-
ask_province_move(Map, Provinces, [RedConq, BlueConq], Province, NewMap, NewProvinces, [NewRedConq, NewBlueConq]):-
    % Print province information
    print_provinces([Province]),
    % Ask the user to choose a move
    writeln('Choose a move:\n1) Displace a unit\n2) Purchase a resource\n3) Do nothing'),
    ask_choice([displace, buy, skip], MoveChoice),

    % Based on the user's choice, ask for move information
    (   MoveChoice == displace
        ->  ask_displace(Map, Province, CoordFrom, CoordTo, NewUnitName, Cancel),
        (   Cancel == true
        ->  ask_province_move(Map, Provinces, [RedConq, BlueConq], Province, NewMap, NewProvinces, [NewRedConq, NewBlueConq])
        ;   get_hex(Map, CoordFrom, HexFrom), get_hex(Map, CoordTo, DestHex),
            displace_unit(Map, Provinces, Province, HexFrom, DestHex, NewUnitName, NewMap, NewProvinces, _)
        )
    ;   MoveChoice == buy
    ->  ask_purchase(Map, Province, ResName, Coord, Cancel),
        (   Cancel == true
        ->  ask_province_move(Map, Provinces, [RedConq, BlueConq], Province, NewMap, NewProvinces, [NewRedConq, NewBlueConq])
        ;   get_hex(Map, Coord, DestHex),
            buy_and_place(Map, Provinces, Province, ResName, DestHex, NewMap, NewProvinces, _)
        )
    ;   MoveChoice == skip
    ->  [NewMap, NewProvinces, NewRedConq, NewBlueConq] = [Map, Provinces, RedConq, BlueConq]
    ),

    % Update the conquest counts in the case of an invasion 
    (   Cancel == false,
        member(MoveChoice, [displace, buy]), % Check
        province_owner(Province, Player), % Get
        other_player(Player, Enemy), % Get
        hex_owner(DestHex, Enemy)
    ->  (   Player == red
        ->  NewRedConq is RedConq + 1,
            NewBlueConq = BlueConq
        ;   NewRedConq = RedConq,
            NewBlueConq is BlueConq + 1
        )
    ;   [NewRedConq, NewBlueConq] = [RedConq, BlueConq]
    ).

% Get one possible move for each province (❓non-deterministic❓)
% move(+Board, -NextBoard)
move(board(Map, Provinces, Player, _, Conquests), board(NewMap, NewProvinces, NewPlayer, NewState, NewConquests)):-
    % Select all the player's provinces
    include([In]>>(province_owner(In, Player)), Provinces, ProvincesOfPlayer),!,
    % Select the new player
    other_player(Player, NewPlayer),
    pipe(province_move, [Map, Provinces, Conquests], ProvincesOfPlayer, [NewMap, NewProvinces, NewConquests]),
    % Determine if the game has ended
    (   has_won(NewMap, NewProvinces, Player)
    ->  NewState = win
    ;   NewState = play
    ).

% Get one possible move for a given province (❓non-deterministic❓)
% province_move(+Map, +Provinces, +Province, -NewMap, -NewProvinces)
province_move(Map, Provinces, Conquests, Province, NewMap, NewProvinces, NewConquests):-
    (   % Choose one possible displace move for one owned unit =======================================
        province_hexes(Province, Hexes), % Get
        % Note: the following was commented to forbid multiple displace action on the same province
        % include([In]>>(\+ hex_unit(In, none)), Hexes, HexesWithUnit),
        % pipe(unit_move, [Map, Provinces, Province, Conquests], HexesWithUnit, [NewMap1, NewProvinces1, Province1, NewConquests]),
        member(HexWithUnit, Hexes),
        \+ hex_unit(HexWithUnit, none),
        unit_move(Map, Provinces, Province, Conquests, HexWithUnit, NewMap, NewProvinces, _, NewConquests)
    ;
        % Choose one possible purchase moves ==========================================================
        % Note: the following was commented to forbid multiple resources purchases
        % findall(R, (check_buys(Province, R, _)), ResourcesSets),
        findall([R], (check_buy(Province, R, _)), ResourcesSet),
        append(ResourcesSet, [[]],ResourcesSets),
        % Select one purchase move
        member(ResourceSet, ResourcesSets),
        (   ResourceSet \= []
        ->  pipe(resource_buy, [Map, Provinces, Province, Conquests], ResourceSet, [NewMap, NewProvinces, _, NewConquests])
        ;   [NewMap, NewProvinces, NewConquests] = [Map, Provinces, Conquests]
        )
    ).

% Get one possible move for a given unit and apply it (❓non-deterministic❓)
% unit_move(+Map, +Provinces, +Province, +HexWithUnit, -NewMap, -NewProvinces, -NewProvince)
unit_move(Map, Provinces, Province, [RedConq, BlueConq], HexWithUnit, NewMap, NewProvinces, NewProvince, [NewRedConq, NewBlueConq]):-
    % The unit remains still
    NewMap = Map,
    NewProvinces = Provinces,
    NewProvince = Province,
    NewRedConq = RedConq,
    NewBlueConq = BlueConq
;   % The unit moves to another hex
    hex_unit(HexWithUnit, Unit), % Get
    province_owner(Province, Player), % Get
    other_player(Player, Enemy), % Get
    findall([Dest, NewUnit], unit_placement(Map, Province, Unit, Dest, NewUnit), Dests),
    member([DestHex, NewUnitName], Dests),
    DestHex \= HexWithUnit,
    % Update the conquest counts in the case of an invasion 
    (   hex_owner(DestHex, Enemy)
    ->  (   Player == red
        ->  NewRedConq is RedConq + 1,
            NewBlueConq = BlueConq
        ;   NewRedConq = RedConq,
            NewBlueConq is BlueConq + 1
        )
    ;   [NewRedConq, NewBlueConq] = [RedConq, BlueConq]
    ),
    displace_unit(Map, Provinces, Province, HexWithUnit, DestHex, NewUnitName, NewMap, NewProvinces, NewProvince).

% Purchase the given resource and place it in one possible location (❓non-deterministic❓)
% resource_buy(+Map, +Provinces, +Province, +ResourceName, -NewMap, -NewProvinces, -NewProvince)
resource_buy(Map, Provinces, Province, [RedConq, BlueConq], ResourceName, NewMap, NewProvinces, NewProvince, [NewRedConq, NewBlueConq]):-
    (   % The resource to be placed is a unit:
        unit(ResourceName, _, _, _, _), % Check
        findall(Dest, unit_placement(Map, Province, ResourceName, Dest, _), Dests)
    ;   % The resource to be placed is a building:
        building(ResourceName, _, _, _), % Check
        findall(Dest, building_placement(Map, Province, ResourceName, Dest), Dests)
    ),
    member(DestHex, Dests),
    % Update the conquest counts in the case of an invasion 
    province_owner(Province, Player), % Get
    other_player(Player, Enemy), % Get
    (   hex_owner(DestHex, Enemy)
    ->  (   Player == red
        ->  NewRedConq is RedConq + 1,
            NewBlueConq = BlueConq
        ;   NewRedConq = RedConq,
            NewBlueConq is BlueConq + 1
        )
    ;   [NewRedConq, NewBlueConq] = [RedConq, BlueConq]
    ),
    buy_and_place(Map, Provinces, Province, ResourceName, DestHex, NewMap, NewProvinces, NewProvince).

% ==================================================================================================
% Plays the game
play:-
    % Ask the user to choose a game mode
    writeln('Welcome to Antitomacy!'),
    writeln('Choose a game mode:\n1) User vs AI\n2) AI vs AI'),
    ask_choice([user_vs_ai, ai_vs_ai], GameMode), nl,
    
    % Generate the map and spawn the provinces
    generate_random_map(MapWithoutProvinces, false),
    spawn_provinces(MapWithoutProvinces, Map),
    print_map(Map),
    find_provinces(Map, [ProvinceRed, ProvinceBlue]),

    % Assign start money
    change_province_money(ProvinceRed, 12, ProvinceRed2),
    change_province_money(ProvinceBlue, 12, ProvinceBlue2),

    % test_map4(Map, Provinces),

    % Determine who will be the first player
    (   GameMode == user_vs_ai
    ->  writeln('Choose a color:\n1) red\n2) blue'),
        ask_choice([red, blue], HumanPlayer), nl
    ;   % If the game mode is AI vs AI, there is no human player
        HumanPlayer = none
    ), 

    % TODO HERE: Initialize the gui
    game_loop(board(Map, [ProvinceRed2, ProvinceBlue2], red, play, [0, 0]), HumanPlayer, red).

game_loop(Board, HumanPlayer, StartPlayer) :-
    board_player(Board, Player), % Get
    format('It is ~w turn: ===========================', Player), nl,
    
    % Depending on the playing player, the minimax searches for a move or the user is asked to commit their move
    (   Player == HumanPlayer 
    ->  % Loop through each province owned by the user
        ask_provinces_moves(Board, NewBoardBeforeIncome)
    ;   % The cpu is playing, so the minimax will be used to choose a move
        get_time(StartTime), update_start_time(StartTime),
        minimax(Board, [-999999, 999999], 3, [NewBoardBeforeIncome, Val]),
        format('Value = ~w', Val), nl
    ),
    % At the end of both players turn, apply the income on all the provinces
    (   Player == StartPlayer, !,
        NewBoard = NewBoardBeforeIncome
    ;   writeln('Applying income ========================='),
        board_map(NewBoardBeforeIncome, NewMapBeforeIncome), % Get
        board_provinces(NewBoardBeforeIncome, NewProvincesWithoutIncome), % Get    
        apply_incomes(NewMapBeforeIncome, NewProvincesWithoutIncome, NewMap, NewProvinces),
        % Create the new board
        change_board_map(NewBoardBeforeIncome, NewMap, NewBoardWithMap),
        change_board_provinces(NewBoardWithMap, NewProvinces, NewBoard)
    ),
    % board_map(NewBoard, MapToPrint),
    % TODO HERE: gui(MapToPrint),
    print_board(NewBoard),
    % Check if the playing player has won the game, if so, end the game
    (   board_state(NewBoard, win) % Check
    ->  format('~w won the game! ---------------------', Player),nl
    ;   game_loop(NewBoard, HumanPlayer, StartPlayer)
    ), !.