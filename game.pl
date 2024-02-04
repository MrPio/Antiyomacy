:- module(game, [move/2, play/0, province_move/7, unit_move/9, resource_buy/9]).
:- use_module([test, utils, map, hex, province, unit, building, economy, eval, minimax, io]).

/* TODO:
    ? Logic for choosing the first colour to play (Federico)
    • Use the already written predicates to randomly generate map and provinces at startup. (Valerio)
    -------------------------------------------------------------------------------------------------
    X Logic to handle user buy and place and unit displace (Federico)
    X Loop input for each provinces, print province counter (Federico)
    X Territory color choice menu (Federico)
    X I/O handling(Federico)
    X Vertical cut with time instead of horizontal cut with depth (Valerio)
    X New eval function for province money and units (Federico)
    X Ensure the tests are working (Valerio)
    X The eval function should reward in case of victory (Federico)
    x Benchmark the move/2 predicate (Valerio)
    x Make sure that update_province is called only when needed (Valerio)
    X Upgrade the evaluation function (Federico)
    X Start game adding some money to the provinces (Valerio)
    X Complete the move/2 predicate handling both purchase actions (Valerio)
    X Write the game controller which calls the minimax module (Valerio)
    X Write the has_won/3 predicate (Federico)
    X When a province becomes smaller than 2 hexes, it must be destroyed immediately
      and its owner should become 'none'. (Federico)
    X Write the minimax algorithm with alpha beta pruning (Valerio)
    X Write a first draft of the evaluation function (Valerio)
    X Province merge and split (very hard!) (Valerio)
    X Two units of the same level may join together to form a stronger unit. (Federica)
    X Any unit can move up to 4 hexes in a turn, provided that all but the last hex are their
      own Province. (Federico)
    X Test province split due to enemy attack. Money should split based on provinces size. (Federico)
    X At the beginning of the game, at least two provinces are randomly generated
      and located far apart. (Valerio)
    X Baron and Knight should ignore the enemy towers. (Federico)
    X Province money managment and bankruptcy. (Valerio)
    X Test units attack. (Valerio)
    X Multiple purchase actions if enough money. (Valerio)
    X Farm should be placed inside the province and near other farms. The first farm can be
    placed anywhere inside the province (Federico)
    X The farm cost should increase with the number of already bought farms,
    so a predicate to count the number of farms in a given province is needed. (Federica)
    X The newly placed unit should not move before the next turn; that is because it can be
    placed directly outside the province boundary already at purchase time. (Valerio)
    X When applying a move, should I update both the Map and the Province or just the former?
    (Valerio)
 */

/* Things to write in the paper:
    • Vertical cut with time instead of horizontal cut with depth
    • Trees cannot randomly spawn during the gameplay. Instead they can spawn at the beginning
    of the game, or supply centers can be introduced.
    X yall library was used to define a lambda expressions
    X When going bankrupt, buildings are not destroyed. This means that it is possible
      to have a negative income even after killing all units
    • Library "clpfd" was used to achieve two-way unifications.
    • Tests were used to prevent any issues with code updates affecting existing functionalities.
    X The terrain on the map was randomly generated using the random walker's algorithm.
    X To follow the CBDP philosophy, module/2 and use_module/1 were used instead of consult/1.
*/

% Asks the user to choose a move for each of their provinces
ask_provinces_moves(_, _).
% ask_provinces_moves(+board(Map, Provinces, HumanPlayer, _, Conquests), -board(NewMap, NewProvinces, NewPlayer, NewState, NewConquests)):-
ask_provinces_moves(board(Map, Provinces, HumanPlayer, _, Conquests), board(NewMap, NewProvinces, NewPlayer, NewState, NewConquests)):-
    % Select provinces owned by the human player
    include([In]>>(province_owner(In, HumanPlayer)), Provinces, HumanProvinces),!,
    % Calculate the total number of provinces owned by the human player
    length(HumanProvinces, TotHumanProvinces),
    % Ask the user for moves for each province
    ask_province_moves(Map, Provinces, Conquests, HumanProvinces, NewMap, NewProvinces, NewConquests, 1, TotHumanProvinces),
    % Determine the other player
    other_player(HumanPlayer, NewPlayer),
    % Check if the human player has won
    (   has_won(NewMap, NewProvinces, HumanPlayer)
    ->  NewState = win
    ;   NewState = play
    ).


% Iterates through each province owned by the human player
% ask_province_moves(+Map, +Provinces, +Conquests, +[HumanProvince|Rest], -NewMap, -NewProvinces, -NewConquests, -CurrentNumber, +TotHumanProvinces):-
ask_province_moves(_,_,_,[],_,_,_,_,_).
ask_province_moves(Map, Provinces, Conquests, [HumanProvince|Rest], NewMap, NewProvinces, NewConquests, CurrentNumber, TotHumanProvinces):-
    % Ask the user for a move for the current province
    ask_province_move(Map, Provinces, Conquests, HumanProvince, NewMap, NewProvinces, NewConquests, CurrentNumber, TotHumanProvinces),
    % Update the current province counter
    CurrentNumber1 is CurrentNumber + 1,
    % Recursively continue asking moves for the remaining provinces
    ask_province_moves(Map, Provinces, Conquests, Rest, NewMap, NewProvinces, NewConquests, CurrentNumber1, TotHumanProvinces).


% Asks the user for a move for a specific province
% ask_province_move(+Map, +Provinces, +[RedConq, BlueConq], +HumanProvince, -NewMap, -NewProvinces, -[NewRedConq, NewBlueConq], -CurrentNumber, +TotHumanProvinces):-
ask_province_move(Map, Provinces, [RedConq, BlueConq], HumanProvince, NewMap, NewProvinces, [NewRedConq, NewBlueConq], CurrentNumber, TotHumanProvinces):-
    % Print province information
    format('Province number: ~w/~w', [CurrentNumber, TotHumanProvinces]),nl,
    format('Province description: ~w~n', [HumanProvince]),
    % Get the color of the human player
    province_owner(HumanProvince, Player),
    % Take user move
    player_move(MoveChoice),

    % Based on the user's choice, ask for move information
    (   MoveChoice == 1 % Code for Displace
    ->  writeln('Player chose Displace'),
        displace_input([X1,Y1], [X2,Y2]),
        get_hex(Map, [X1,Y1], HexWithUnit),
        get_hex(Map, [X2,Y2], DestHex),
        DestHex \= HexWithUnit,
        % The unit moves to another hex
        hex_unit(HexWithUnit, Unit), % Get
        province_owner(HumanProvince, Player), % Get
        other_player(Player, Enemy), % Get
        % Check valid move
        unit_placement(Map, HumanProvince, Unit, DestHex, NewUnitName),
        % Update the conquest counts in the case of an invasion 
        (   hex_owner(DestHex, Enemy)
        ->  (   Player == red
            ->  NewRedConq is RedConq + 1,
                NewBlueConq = BlueConq
            ;   NewRedConq = RedConq,
                NewBlueConq is BlueConq + 1
            )
        ;   NewRedConq = RedConq,
            NewBlueConq = BlueConq
        ),
        displace_unit(Map, Provinces, HumanProvince, HexWithUnit, DestHex, NewUnitName, NewMap, NewProvinces, _)
    ; MoveChoice == 2 % Code for Purchase
    ->  writeln('Player chose Purchase'),
        purchase_input(ResName,[X,Y]),
        % Get DestHex
        get_hex(Map, [X,Y], DestHex),
        is_building(ResName, IsBuilding),
        (IsBuilding == 1 ->
            % Check legal move
            building_placement(Map, HumanProvince, ResName, DestHex)
        ;   
            % Check legal move
            unit_placement(Map, HumanProvince, ResName, DestHex, NewUnitName) % Check & Get
        ),
        % Update the conquest counts in the case of an invasion 
        province_owner(HumanProvince, Player), % Get
        other_player(Player, Enemy), % Get
        (   hex_owner(DestHex, Enemy)
        ->  (   Player == red
            ->  NewRedConq is RedConq + 1,
                NewBlueConq = BlueConq
            ;   NewRedConq = RedConq,
        )
                NewBlueConq is BlueConq + 1
        ;   NewRedConq = RedConq,
            NewBlueConq = BlueConq
        ),
        buy_and_place(Map, Provinces, HumanProvince, ResName, DestHex, NewMap, NewProvinces, _)
    ; MoveChoice == 3 % Code for Skip turn
    ->  writeln('Player chose Skip move for this province'),
        NewMap = Map,
        NewProvinces = Provinces,
        [NewRedConq, NewBlueConq] = [RedConq, BlueConq]
    ; writeln('Invalid choice')
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
    ;   NewRedConq = RedConq,
        NewBlueConq = BlueConq
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
    ;   NewRedConq = RedConq,
        NewBlueConq = BlueConq
    ),
    buy_and_place(Map, Provinces, Province, ResourceName, DestHex, NewMap, NewProvinces, NewProvince).

% ==================================================================================================
% Plays the game
play:-
    writeln('Welcome to Antitomacy!'),
    % generate_random_map(Map, false),
    % spawn_provinces(Map, MapWithProvinces),
    % print_map(MapWithProvinces),
    % find_provinces(MapWithProvinces, Provinces),
    test_map4(MapWithProvinces, [ProvinceRedSorted, ProvinceBlueSorted]),
    
    % Choose the game mode and, if human wants to play, asks color of the human player
    ask_game_mode(HumanPlayer),
    % Determine who will be the first player
    random_member(FirstPlayer,[red, blue]),

    game_loop(board(MapWithProvinces, [ProvinceRedSorted, ProvinceBlueSorted], FirstPlayer, play, [0, 0]), HumanPlayer).

game_loop(Board, HumanPlayer) :-
    board_player(Board, Player), % Get
    format('It is ~w turn:', Player),nl,
    
    % Depending on the playing player, the minimax searches for a move or the user is asked to commit their move
    (   Player == HumanPlayer 
    ->  % Loop through each province owned by the user
        ask_provinces_moves(Board, NewBoard),
        board_map(NewBoard, NewMap), % Get
        board_provinces(NewBoard, NewProvinces), % Get    
        % Print the board
        print_map(NewMap),
        print_provinces(NewProvinces),
        game_loop(NewBoard, HumanPlayer)
    ;   % The cpu is playing, so the minimax will be used to choose a move
        get_time(StartTime), update_start_time(StartTime),
        minimax(Board, [-999999, 999999], 3, [NewBoard, Val]),
        board_map(NewBoard, NewMap), % Get
        board_provinces(NewBoard, NewProvinces), % Get    

        % At the end of both players turn, apply the income on all the provinces
        apply_incomes(NewMap, NewProvinces, NewMapWithIncome, NewProvincesWithIncome),
        change_board_map(NewBoard, NewMapWithIncome, NewBoardWithMap),
        change_board_provinces(NewBoardWithMap, NewProvincesWithIncome, NewBoardWithIncome)
    ),

    % Calculate and print the elapsed time
    get_time(EndTime), ElapsedTime is truncate((EndTime-StartTime)*1000000),
    format('Total time = ~w micro', ElapsedTime), nl,

    % Print the board
    print_map(NewMapWithIncome),
    print_provinces(NewProvincesWithIncome),
    format('Value = ~w', Val), nl,

    % Check if the playing player has won the game, if so, end the game
    (   board_state(NewBoardWithIncome, State), % Get
        State = win
    ->  format('~w won the game! ---------------------', Player),nl
    ;   game_loop(NewBoardWithIncome, HumanPlayer)
    ),!.
