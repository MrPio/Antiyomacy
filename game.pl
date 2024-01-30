:- module(game, [move/2, play/0, test/0]).
:- use_module([utils, map, hex, province, unit, building, economy, eval, minimax]).

/* TODO:
    • Make sure that update_province is called only when needed (Valerio)
    • Benchmark the move/2 predicate (Valerio)
    • Upgrade the evaluation function (Federico)
    -------------------------------------------------------------------------------------------------
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
    • Trees cannot randomly spawn during the gameplay. Instead they can spawn at the beginning
    of the game, or supply centers can be introduced.
    • yall library was used to define a lambda expressions
    • When going bankrupt, buildings are not destroyed. This means that it is possible
      to have a negative income even after killing all units
    • Library "clpfd" was used to achieve two-way unifications.
    • Tests were used to prevent any issues with code updates affecting existing functionalities.
    • The terrain on the map was randomly generated using the random walker's algorithm.
    • To follow the CBDP philosophy, module/2 and use_module/1 were used instead of consult/1.
*/

% Checks if the player (Player) has won by ensuring that at least 80% of the hexes (excluding sea tiles)
% or there are no more provinces owned by the other player.
% has_won(+Map, +Provinces, +Player)
has_won(Map, Provinces, Player) :-
    % Get all hexes on the map that do not have 'sea' in their tile
    get_non_sea_hexes(Map, NonSeaHexes),

    % Calculate the total hexes on the map (excluding sea)
    length(NonSeaHexes, TotalHexes),

    % Calculate the total size of the player's provinces
    include([In]>>(province_owner(In, Player)), Provinces, ProvincesOfPlayer),
    maplist(province_size, ProvincesOfPlayer, PlayerSizes),
    sum_list(PlayerSizes, PlayerTotalSize),

    % Calculate the percentage of player's hexes compared to the total (excluding sea)
    Percentage is (PlayerTotalSize / TotalHexes) * 100,

    % Check if the percentage is at least 80% or there are no more provinces owned by the other player
    (Percentage >= 80 ; % OR
     % Check if there are no more provinces owned by the other player
     \+ (member(Province, Provinces), province_owner(Province, OtherPlayer), OtherPlayer \= Player)),
    !. % Cut to prevent backtracking

% Get the other player
other_player(red, blue).
other_player(blue, red).

% Get one possible move (non-deterministic)
% move(+Board, -NextBoard)
move(board(Map, Provinces, Player, _), board(NewMap, NewProvinces, NewPlayer, NewState)):-
    % Select all the player's provinces
    include([In]>>(province_owner(In, Player)), Provinces, ProvincesOfPlayer),!,
    % Select the new player
    player(NewPlayer), NewPlayer \= Player,
    move_(Map, Provinces, ProvincesOfPlayer, NewMap, NewProvinces),
    % Determine if the game has ended
    (   has_won(NewMap, NewProvinces, Player)
    ->  NewState = win
    ;   NewState = play
    ).

% province_move(+Map, +Provinces, +ProvincesOfPlayer, -NewMap, -NewProvinces)
move_(Map, Provinces, [], Map, Provinces).
move_(Map, Provinces, [ProvinceOfPlayer|T], NewMap, NewProvinces):-
    province_move(Map, Provinces, ProvinceOfPlayer, NewMap1, NewProvinces1),
    move_(NewMap1, NewProvinces1, T, NewMap, NewProvinces).

% Get one possible move for a given province (non-deterministic)
% province_move(+Map, +Provinces, +Province, -NewMap, -NewProvinces)
province_move(Map, Provinces, Province, NewMap, NewProvinces):-
    % Choose one possible displace move for each owned unit =======================================
    province_hexes(Province, Hexes), % Get
    % Select hexes with unit
    include([In]>>(\+ hex_unit(In, none)), Hexes, HexesWithUnit),
    province_move_(Map, Provinces, Province, HexesWithUnit, NewMap1, NewProvinces1, NewProvince1),
    % Choose one possible purchase moves ==========================================================
    findall(R, (check_buys(Province, R, _)), ResourcesSets),
    % Select one purchase move
    member(ResourceSet, ResourcesSets),
    (   ResourceSet \= []
    ->  province_buy_(NewMap1, NewProvinces1, NewProvince1, ResourceSet, NewMap, NewProvinces, _)
    ;   [NewMap, NewProvinces] = [NewMap1, NewProvinces1]
    ).

% Displace all the given units
% province_move_(+Map, +Provinces, +Province, +HexesWithUnit, -NewMap, -NewProvinces)
province_move_(Map, Provinces, Province, [], Map, Provinces, Province).
province_move_(Map, Provinces, Province, [HexWithUnit|T], NewMap, NewProvinces, NewProvince):-
    unit_move(Map, Provinces, Province, HexWithUnit, NewMap1, NewProvinces1, NewProvince1),
    province_move_(NewMap1, NewProvinces1, NewProvince1, T, NewMap, NewProvinces, NewProvince).

% Purchase all the given resources on the given province
% province_move_(+Map, +Provinces, +Province, +ResourcesSets, -NewMap, -NewProvinces)
province_buy_(Map, Provinces, Province, [], Map, Provinces, Province).
province_buy_(Map, Provinces, Province, [Resource|T], NewMap, NewProvinces, NewProvince):-
    resource_buy(Map, Provinces, Province, Resource, NewMap1, NewProvinces1, NewProvince1),
    province_buy_(NewMap1, NewProvinces1, NewProvince1, T, NewMap, NewProvinces, NewProvince).


% Apply one possible move for a given unit (non-deterministic)
% unit_move(+Map, +Provinces, +Province, +HexWithUnit, -NewMap, -NewProvinces, -NewProvince)
unit_move(Map, Provinces, Province, HexWithUnit, NewMap, NewProvinces, NewProvince):-
    % The unit remains still
    NewMap = Map,
    NewProvinces = Provinces,
    NewProvince = Province
;   % The unit moves to another hex
    hex_unit(HexWithUnit, Unit), % Get
    findall(Dest, unit_placement(Map, Province, Unit, Dest, _), Dests),
    member(DestHex, Dests),
    DestHex \= HexWithUnit,
    displace_unit(Map, Provinces, Province, HexWithUnit, DestHex, NewMap, NewProvinces, NewProvince).

% Purchase the given resource and place it in one possible location (non-deterministic)
% resource_buy(+Map, +Provinces, +Province, +ResourceName, -NewMap, -NewProvinces, -NewProvince)
resource_buy(Map, Provinces, Province, ResourceName, NewMap, NewProvinces, NewProvince):-
    (   % The resource to be placed is a unit:
        unit(ResourceName, _, _, _, _), % Check
        findall(Dest, unit_placement(Map, Province, ResourceName, Dest, _), Dests)
    ;   % The resource to be placed is a building:
        building(ResourceName, _, _, _), % Check
        findall(Dest, building_placement(Map, Province, ResourceName, Dest), Dests)
    ),
    member(DestHex, Dests),
    buy_and_place(Map, Provinces, Province, ResourceName, DestHex, NewMap, NewProvinces, NewProvince).



% Play the game
play:-
    % generate_random_map(Map, false),
    % spawn_provinces(Map, MapWithProvinces),
    % print_map(MapWithProvinces),
    % find_provinces(MapWithProvinces, Provinces),
    generate_random_map(_, true),
    RedHexes=[[0,0],[1,0]],
    BlueHexes=[[3,4],[4,4]],
    Buildings=[],
    Units=[],
    foreach(member(Coord,RedHexes),set_owner(Coord,red)),
    foreach(member(Coord,BlueHexes),set_owner(Coord,blue)),
    foreach(member(Coord-Building,Buildings),set_building(Coord,Building)),
    foreach(member(Coord-Unit,Units),set_unit(Coord,Unit)),
    map(MapWithProvinces),
    print_map(MapWithProvinces),
    find_provinces(MapWithProvinces, Provinces),
    writeln('==============================================='),

    Provinces=[ProvinceRed, ProvinceBlue],
    change_province_money(ProvinceRed, 12, ProvinceRed2),
    change_province_money(ProvinceBlue, 12, ProvinceBlue2),
    update_province(MapWithProvinces, ProvinceRed2, ProvinceRedSorted),
    update_province(MapWithProvinces, ProvinceBlue2, ProvinceBlueSorted),
    print_provinces([ProvinceRedSorted, ProvinceBlueSorted]),
    
    game_loop(board(MapWithProvinces, [ProvinceRedSorted, ProvinceBlueSorted], blue, play)).

game_loop(Board):-
    board_player(Board, Player), % Get
    format('It is ~w turn:', Player),nl,
    get_char(_), skip_line,
    get_time(StartTime),
    minimax(Board, [-999999, 999999], 2, [NewBoard, _]),
    get_time(EndTime),
    Time is EndTime-StartTime,
    writeln(Time),

    % Apply income
    board_map(NewBoard, NewMap), % Get
    board_provinces(NewBoard, NewProvinces), % Get
    include([In]>>(province_owner(In, Player)),NewProvinces, NewProvincesOfPlayer),
    exclude([In]>>(province_owner(In, Player)),NewProvinces, NewProvincesOfEnemy),
    apply_incomes(NewMap, NewProvincesOfPlayer, NewMapWithIncome, NewProvincesOfPlayerWithIncome),
    append(NewProvincesOfEnemy, NewProvincesOfPlayerWithIncome, NewProvincesWithIncome),
    change_board_map(NewBoard, NewMapWithIncome, NewBoardWithMap),
    change_board_provinces(NewBoardWithMap, NewProvincesWithIncome, NewBoardWithIncome),

    print_map(NewMapWithIncome),
    print_provinces(NewProvincesWithIncome),
    (   board_state(NewBoardWithIncome, State), % Get
        State = win
    ->  format('~w won the game! ---------------------', Player),nl
    ;   game_loop(NewBoardWithIncome)
    ).

% Run all the tests
test:-
    test_province,
    test_placements,
    test_purchases,
    test_farm,
    test_attack,
    test_end_turn,
    test_destroy_tower,
    test_displace_with_merge,
    test_buy_and_merge,
    test_share_money,
    test_manhattan_distance,
    test_destroy_province,
    nl, writeln('-- All the tests have succeeded! ---'), nl, !.

% Generate the following test map
%    0 1 2 3 4
% 0 |r| |r| | |
% 1 |r|r|r| | |
% 2 | | |b| | |
% 3 | | |b|b|b|
% 4 | | | |b| |
test_map(Map, [ProvinceRed, ProvinceBlue]):-
    generate_random_map(_, true),
    % Manually populating the map
    RedHexes=[[0,0],[1,0],[1,1],[1,2],[0,2]],
    BlueHexes=[[2,2],[3,2],[3,3],[3,4],[4,3]],
    Buildings=[[2,2]-tower],
    Units=[[0,0]-peasant],
    foreach(member(Coord,RedHexes),set_owner(Coord,red)),
    foreach(member(Coord,BlueHexes),set_owner(Coord,blue)),
    foreach(member(Coord-Building,Buildings),set_building(Coord,Building)),
    foreach(member(Coord-Unit,Units),set_unit(Coord,Unit)),
    map(Map),
    find_provinces(Map, [ProvinceRed, ProvinceBlue]).

% Generate the following test map
%    0 1 2 3 4
% 0 | |b| | | |
% 1 | |b|b| | |
% 2 | | |b|r|r|
% 3 |r|r|b| | |
% 4 |r| |b| | |
test_map2(Map, [ProvinceBlue, ProvinceRedEst, ProvinceRedWest]):-
    generate_random_map(_, true),
    % Manually populating the map
    RedHexes=[[2,3],[2,4],[3,0],[3,1],[4,0]],
    BlueHexes=[[0,1],[1,1],[1,2],[2,2],[3,2],[4,2]],
    foreach(member(Coord,RedHexes),set_owner(Coord,red)),
    foreach(member(Coord,BlueHexes),set_owner(Coord,blue)),
    map(Map),
    find_provinces(Map, [ProvinceBlue, ProvinceRedEst, ProvinceRedWest]).

% Generate the following test map
%    0 1 2 3 4
% 0 |r| |r|b|b|
% 1 |r|r|r|b|b|
% 2 |b|b|b|b|b|
% 3 |b|b|b|b|b|
% 4 |b|b|b|b|b|
test_map3(Map, [ProvinceRed, ProvinceBlue]):-
    generate_random_map(_, true),
    % Manually populating the map
    RedHexes=[[0,0],[1,0],[1,1],[1,2],[0,2]],
    BlueHexes=[[0,3],[0,4],[1,3],[1,4],[2,0],[2,1],[2,2],[2,3],[2,4],[3,0],[3,1],[3,2],[3,3],[3,4],[4,0],[4,1],[4,2],[4,3],[4,4]],
    Units=[[0,0]-peasant],
    foreach(member(Coord,RedHexes),set_owner(Coord,red)),
    foreach(member(Coord,BlueHexes),set_owner(Coord,blue)),
    foreach(member(Coord-Unit,Units),set_unit(Coord,Unit)),
    map(Map),
    find_provinces(Map, [ProvinceRed, ProvinceBlue]).

test_province:-
    nl,writeln('test_province ======================================================'),
    test_map(Map, [ProvinceRed, ProvinceBlue]),

    % Test: find_provinces
    write('Testing provinces calculation... '),
    province_hexes(ProvinceRed, ProvinceRedHexes), % Get
    province_hexes(ProvinceBlue, ProvinceBlueHexes), % Get
    same_elements(ProvinceRedHexes, [hex(7   ,[1,2],_,red,none,none),hex(2,[0,2],_,red,none,none),hex(6,[1,1],_,red,none,none),hex(5,[1,0],_,red,none,none),hex(0,[0,0],_,red,none,peasant)]),
    same_elements(ProvinceBlueHexes, [hex(19,[3,4],_,blue,none,none),hex(23,[4,3],_,blue,none,none),hex(18,[3,3],_,blue,none,none),hex(17,[3,2],_,blue,none,none),hex(12,[2,2],_,blue,tower,none)]),
    writeln('Ok!'),

    % Test: outer_border
    write('Testing province boundary calculation... '),
    outer_border(Map,ProvinceRed,ProvinceRedBoundary),
    same_elements(ProvinceRedBoundary, [hex(1,[0,1],_,none,none,none),hex(3,[0,3],_,none,none,none),hex(8,[1,3],_,none,none,none),hex(11,[2,1],_,none,none,none),hex(12,[2,2],_,blue,tower,none),hex(13,[2,3],_,none,none,none),hex(10,[2,0],_,none,none,none)]),
    writeln('Ok!'),

    % Test: inner_border
    write('Testing province inner_border calculation... '),
    inner_border(Map,ProvinceRed,InnerBorder),
    same_elements(InnerBorder, [hex(7,[1,2],_,red,none,none),hex(2,[0,2],_,red,none,none),hex(6,[1,1],_,red,none,none),hex(5,[1,0],_,red,none,none),hex(0,[0,0],_,red,none,peasant)]),
    writeln('Ok!'),

    % Test: get_income
    write('Testing province income calculation... '),
    get_income(ProvinceBlue, 4),
    writeln('Ok!').

test_placements:-
    nl,writeln('test_placements ======================================================'),
    test_map(Map, [ProvinceRed, _]),

    % Test: unit_placement
    write('Testing units placements... '),
    findall(Dest,unit_placement(Map, ProvinceRed, peasant, Dest, _), DestList),
    same_elements(DestList, [hex(0,[0,0],_,red,none,peasant),hex(2,[0,2],_,red,none,none),hex(5,[1,0],_,red,none,none),hex(1,[0,1],_,none,none,none),hex(3,[0,3],_,none,none,none),hex(10,[2,0],_,none,none,none)]),
    writeln('Ok!'),

    % Test: building_placement
    write('Testing buildings placements... '),
    findall(Dest, building_placement(Map, ProvinceRed, tower, Dest), DestList2),
    same_elements(DestList2, [hex(2,[0,2],_,red,none,none),hex(7,[1,2],_,red,none,none),hex(6,[1,1],_,red,none,none),hex(5,[1,0],_,red,none,none)]),
    writeln('Ok!'),

    % Test: tower_nearby
    write('Testing nearby towers detection... '),
    tower_nearby(Map,[2,1],red),
    writeln('Ok!').

test_purchases:-
    nl,writeln('test_purchases ======================================================'),
    test_map(Map, [ProvinceRed, ProvinceBlue]),

    % Test: check_buys
    write('Testing purchase actions listing... '),
    change_province_money(ProvinceBlue, 24, ProvinceBlue2),
    findall(Resource, (check_buys(ProvinceBlue2, Resource, _)), ResourcesList),
    same_elements(ResourcesList, [[farm,farm],[farm,peasant],[farm],[tower],[peasant,farm],[peasant,peasant],[peasant],[spearman],[]]),
    writeln('Ok!'),

    % Test: buy_and_place
    write('Testing purchase action... '),
    change_province_money(ProvinceRed, 16, ProvinceRed2),
    get_hex(Map, [0,1], NewUnitHex),
    buy_and_place(Map, [ProvinceRed2, ProvinceBlue2], ProvinceRed2, peasant, NewUnitHex, Map2, _, ProvinceRed3),
    province_hexes(ProvinceRed3,ProvinceRed3Hexes),
    same_elements(ProvinceRed3Hexes, [hex(7,[1,2],_,red,none,none),hex(2,[0,2],_,red,none,none),hex(6,[1,1],_,red,none,none),hex(1,[0,1],_,red,none,peasant),hex(5,[1,0],_,red,none,none),hex(0,[0,0],_,red,none,peasant)]),
    writeln('Ok!'),
    print_map(Map2),

    % Test: displace_unit
    write('Testing unit displacement action... '),
    get_hex(Map2, [0,0], UnitDisplaceFrom),
    get_hex(Map2, [2,0], UnitDisplaceTo),
    displace_unit(Map2, [ProvinceRed2, ProvinceBlue2], ProvinceRed3, UnitDisplaceFrom, UnitDisplaceTo, Map3, _, ProvinceRed4),
    province_hexes(ProvinceRed4,ProvinceRed4Hexes),
    same_elements(ProvinceRed4Hexes, [hex(10,[2,0],_,red,none,peasant),hex(0,[0,0],_,red,none,none),hex(5,[1,0],_,red,none,none),hex(1,[0,1],_,red,none,peasant),hex(6,[1,1],_,red,none,none),hex(2,[0,2],_,red,none,none),hex(7,[1,2],_,red,none,none)]),
    writeln('Ok!'),
    print_map(Map3),

    % Test: wrong displace_unit
    write('Testing wrong unit displacement action near enemy tower... '),
    get_hex(Map2, [0,1], UnitDisplaceFrom1),
    get_hex(Map2, [2,1], UnitDisplaceTo1),
    \+ displace_unit(Map3, [ProvinceRed4, ProvinceBlue2], ProvinceRed4, UnitDisplaceFrom1, UnitDisplaceTo1, _, _, _),
    writeln('Ok!').

test_farm:-
    nl,writeln('test_farm ======================================================'),
    test_map(Map, [ProvinceRed, ProvinceBlue]),

    % Test: province_count
    write('Buying two farms and testing farm counting... '),
    % Purchasing 2 farms for Blue province
    change_province_money(ProvinceBlue,26,ProvinceBlue2),
    get_hex(Map, [4,3], BlueFarmHex),
    get_hex(Map, [3,4], BlueFarmHex2),
    buy_and_place(Map, [ProvinceRed, ProvinceBlue2], ProvinceBlue2, farm, BlueFarmHex, Map2, _, ProvinceBlue3),
    buy_and_place(Map2, [ProvinceRed, ProvinceBlue3], ProvinceBlue3, farm, BlueFarmHex2, Map3, _, ProvinceBlue4),
    print_map(Map3),
    province_count(ProvinceBlue4, farm, FarmCount),
    FarmCount=2,
    writeln('Ok!'),

    % Test: first farm placement in red province
    write('Testing first farm placement in Red province... '),
    change_province_money(ProvinceRed,12,ProvinceRed2),
    get_hex(Map3, [1,0], RedFarmHex),
    buy_and_place(Map3, [ProvinceRed2, ProvinceBlue4], ProvinceRed2, farm, RedFarmHex, Map4, _, _),
    print_map(Map4),
    writeln('Ok!').
test_attack:-
    nl,writeln('test_attack ======================================================'),
    test_map(Map, [ProvinceRed, ProvinceBlue]),

    % Test blue unit attack
    write('Testing blue spearman attack red peasant... '),
    change_province_money(ProvinceBlue, 20, ProvinceBlue2),
    get_hex(Map, [1,2], BlueSpearmanHex),
    buy_and_place(Map, [ProvinceRed, ProvinceBlue2], ProvinceBlue2, spearman, BlueSpearmanHex, Map2, _, ProvinceBlue3),
    print_map(Map2),

    get_hex(Map2, [1,2], BlueSpearmanHexFrom),
    get_hex(Map2, [1,1], BlueSpearmanHexTo),
    displace_unit(Map2, [ProvinceRed, ProvinceBlue3], ProvinceBlue3, BlueSpearmanHexFrom, BlueSpearmanHexTo, Map3, _, ProvinceBlue4),
    print_map(Map3),

    get_hex(Map3, [1,1], BlueSpearmanHexFrom2),
    get_hex(Map3, [0,0], RedPeasantHex),
    displace_unit(Map3, [ProvinceRed, ProvinceBlue4], ProvinceBlue4, BlueSpearmanHexFrom2, RedPeasantHex, Map4, _, ProvinceBlue5),
    print_map(Map4),
    province_hexes(ProvinceBlue5, ProvinceBlue5Hexes),
    same_elements(ProvinceBlue5Hexes, [hex(0,[0,0],_,blue,none,spearman),hex(7,[1,2],_,blue,none,none),hex(6,[1,1],_,blue,none,none),hex(17,[3,2],_,blue,none,none),hex(12,[2,2],_,blue,tower,none),hex(23,[4,3],_,blue,none,none),hex(18,[3,3],_,blue,none,none),hex(19,[3,4],_,blue,none,none)]),
    writeln('Ok!').

test_end_turn:-
    nl,writeln('test_end_turn ======================================================'),
    test_map(Map, [ProvinceRed, _]),

    % Test apply_income without bankrupt
    write('Testing red province income calculation... '),
    apply_income(Map, ProvinceRed, Map2, ProvinceRed2),
    province_money(ProvinceRed2, 3), % Check
    writeln('Ok!'),

    % Test apply_income with bankrupt
    write('Testing red province bankrupt... '),
    set_unit(Map2, [1,0], baron, Map3),
    update_province(Map3, ProvinceRed2, ProvinceRed3),
    get_income(ProvinceRed3, IncomeRed),
    IncomeRed = -15,
    apply_income(Map3, ProvinceRed3, _, ProvinceRed4),
    province_count(ProvinceRed4, peasant, 0), % Check
    province_count(ProvinceRed4, spearman, 0), % Check
    province_money(ProvinceRed4, 0), % Check
    get_income(ProvinceRed4, 5), % Check
    writeln('Ok!').

test_destroy_tower :-
    nl,writeln('test_destroy_tower ======================================================'),
    test_map(Map, [ProvinceRed, ProvinceBlue]),

    writeln('Testing wrong purchase: red spearman near blue tower... '),
    change_province_money(ProvinceRed, 30, ProvinceRed1),
    get_hex(Map, [1,2], DestHex),
    \+ buy_and_place(Map, [ProvinceRed1, ProvinceBlue], ProvinceRed1, spearman, DestHex, _, _, _),
    writeln('Spearman succesfully not placed!'),nl,

    writeln('Testing purchase baron and blue tower destruction action by red... '),
    buy_and_place(Map, [ProvinceRed1, ProvinceBlue], ProvinceRed1, baron, DestHex, Map1, _, ProvinceRed2),
    print_map(Map1),
    writeln('Baron succesfully placed! '),

    get_hex(Map1, [1,2], BaronHex),
    get_hex(Map1, [2,2], ToHex),
    displace_unit(Map1, [ProvinceRed2, ProvinceBlue], ProvinceRed2, BaronHex, ToHex, Map2, _, _),
    print_map(Map2),
    writeln('Blue tower succesfully destroyed! '),
    writeln('Ok!').

test_displace_with_merge:-
    nl,writeln('test_displace_with_merge ======================================================'),
    test_map(Map, [ProvinceRed, ProvinceBlue]),
    print_map(Map),
    % Test: displace_unit with merge
    write('Testing merge units with displacement... '),
    change_province_money(ProvinceRed, 40, ProvinceRed2),
    get_hex(Map, [0,1], NewUnitHex),
    buy_and_place(Map, [ProvinceRed2, ProvinceBlue], ProvinceRed2, baron, NewUnitHex, Map2, _, ProvinceRed3),
    print_map(Map2),
    get_hex(Map2, [0,0], FromHex),
    get_hex(Map2, [0,1], DestHex),
    displace_unit(Map2, [ProvinceRed3, ProvinceBlue], ProvinceRed3, FromHex, DestHex, NewMap, _, _),
    print_map(NewMap),
    writeln('Ok!').

test_buy_and_merge:-
    nl,writeln('test_buy_and_merge ======================================================'),
    test_map(Map, [ProvinceRed, ProvinceBlue]),
    print_map(Map),
    % Test: buy_and_place with merge
    write('Testing buy and merge units... '),
    change_province_money(ProvinceRed, 100, ProvinceRed2),
    get_hex(Map, [0,0], NewUnitHex),
    \+ buy_and_place(Map, [ProvinceRed2, ProvinceBlue], ProvinceRed2, knight, NewUnitHex, _, _, _),
    buy_and_place(Map, [ProvinceRed2, ProvinceBlue], ProvinceRed2, baron, NewUnitHex, NewMap2, _,_),
    get_hex(NewMap2, [0,0], NewUnitHex2), % Get
    hex_unit(NewUnitHex2, knight), % Check
    print_map(NewMap2),
    writeln('Ok!').

test_share_money:-
    nl,writeln('test_share_money ======================================================'),
    test_map2(Map, [ProvinceBlue, ProvinceRedEst, ProvinceRedWest]),
    print_map(Map),

    writeln('testing attack with merge + split'),
    change_province_money(ProvinceBlue, 13, ProvinceBlue1),
    change_province_money(ProvinceRedEst, 17, ProvinceRedEst1),
    change_province_money(ProvinceRedWest, 50, ProvinceRedWest1),
    get_hex(Map, [2,2], RedAttackHex),
    buy_and_place(Map, [ProvinceBlue1, ProvinceRedEst1, ProvinceRedWest1], ProvinceRedWest1, baron, RedAttackHex, Map2, NewProvinces, _),
    same_elements(NewProvinces,[
        province(red,[hex(14,[2,4],_,red,none,none),hex(13,[2,3],_,red,none,none),hex(12,[2,2],_,red,none,baron),hex(16,[3,1],_,red,none,none),hex(15,[3,0],_,red,none,none),hex(20,[4,0],_,red,none,none)],37),
        province(blue,[hex(7,[1,2],_,blue,none,none),hex(1,[0,1],_,blue,none,none),hex(6,[1,1],_,blue,none,none)],7),
        province(blue,[hex(22,[4,2],_,blue,none,none),hex(17,[3,2],_,blue,none,none)],5)
    ]),
    print_map(Map2),
    writeln('Ok!').

test_manhattan_distance:-
    nl,writeln('test_manhattan_distance ======================================================'),
    test_map(Map, [ProvinceRed, ProvinceBlue]),

    % Purchase Peasant
    writeln('Purchasing Paesant:'),
    change_province_money(ProvinceBlue, 10, ProvinceBlue1),
    get_hex(Map, [3,4], BluePeasantHex),
    buy_and_place(Map, [ProvinceRed, ProvinceBlue1], ProvinceBlue1, peasant, BluePeasantHex, Map1, _, ProvinceBlue2),
    print_map(Map1),

    % First check
    get_hex(Map1, [3,4], BluePeasantHexFrom),
    get_hex(Map1, [3,1], BluePeasantHexTo), % legal
    manhattan_distance(BluePeasantHexFrom, BluePeasantHexTo, Distance1),
    Distance1 =< 4,
    write('Legal move from [3,4] to [3,1], distance:'), write(Distance1),
    displace_unit(Map1, [ProvinceRed, ProvinceBlue2], ProvinceBlue2, BluePeasantHexFrom, BluePeasantHexTo, Map2, _, ProvinceBlue3),
    print_map(Map2),

    % Second check
    get_hex(Map2, [3,1], BluePeasantHexFrom1),
    get_hex(Map2, [3,0], BluePeasantHexTo1),
    manhattan_distance(BluePeasantHexFrom1, BluePeasantHexTo1, Distance2),
    Distance2 =< 4,
    write('Legal move from [3,1] to [3,0], distance:'), write(Distance2),
    displace_unit(Map2, [ProvinceRed, ProvinceBlue3], ProvinceBlue3, BluePeasantHexFrom1, BluePeasantHexTo1, Map3, _, ProvinceBlue4),
    print_map(Map3),

    % Third check
    get_hex(Map3, [3,0], BluePeasantHexFrom2),
    get_hex(Map3, [2,4], BluePeasantHexTo2), % illegal
    manhattan_distance(BluePeasantHexFrom2, BluePeasantHexTo2, Distance3),
    Distance3 > 4,
    write('Illegal move from [3,0] to [2,4], distance:'), write(Distance3),
    \+ displace_unit(Map3, [ProvinceRed, ProvinceBlue4], ProvinceBlue4, BluePeasantHexFrom2, BluePeasantHexTo2, _, _, _),
    print_map(Map3),

    % Fourth check
    get_hex(Map3, [3,0], BluePeasantHexFrom3),
    get_hex(Map3, [3,4], BluePeasantHexTo3), % legal
    manhattan_distance(BluePeasantHexFrom3, BluePeasantHexTo3, Distance4),
    Distance4 =< 4,
    write('Legal move from [3,0] to [3,4], distance:'), write(Distance4),
    displace_unit(Map3, [ProvinceRed, ProvinceBlue4], ProvinceBlue4, BluePeasantHexFrom3, BluePeasantHexTo3, Map4, _, _),
    print_map(Map4),

    writeln('Ok!').

test_destroy_province:-
    nl,writeln('test_destroy_province ======================================================'),
    test_map2(Map, [ProvinceBlue, ProvinceRedEst, ProvinceRedWest]),
    print_map(Map),
        
    % Purchase Peasant
    writeln('Purchasing Paesant:'),
    change_province_money(ProvinceRedWest, 10, ProvinceRedWest1),
    get_hex(Map, [3,2], RedPeasantHex),
    % Destroy blue hex after splitting
    buy_and_place(Map, [ProvinceBlue, ProvinceRedWest1], ProvinceRedWest1, peasant, RedPeasantHex, Map1, _, _ProvinceRedWest2),
    print_map(Map1),
    writeln('Ok!').

test_has_won:-
    nl,writeln('test_has_won ======================================================'),
    test_map3(Map, [ProvinceRed, ProvinceBlue]),
    print_map(Map),
    % Check blue has not won, now he has 19/25=76% hexes
    writeln('Checking victory...'),
    \+ has_won(Map, [ProvinceRed, ProvinceBlue], blue),
    writeln('Blue has not won'),

    % Purchase Peasant
    writeln('Purchasing Paesant:'),
    change_province_money(ProvinceBlue, 10, ProvinceBlue1),
    get_hex(Map, [1,2], BluePeasantHex),
    buy_and_place(Map, [ProvinceRed, ProvinceBlue1], ProvinceBlue1, peasant, BluePeasantHex, Map1, _, ProvinceBlue2),
    print_map(Map1),

    % Check blue has won, now he has 20/25=80% hexes
    writeln('Checking victory...'),
    has_won(Map1, [ProvinceRed, ProvinceBlue2], blue),
    writeln('Blue has now won'),

    writeln('Ok!').