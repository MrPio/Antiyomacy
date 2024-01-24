:- use_module([printer, map, hex, province, unit, building, economy]).

/* TODO:
    • Two units of the same level may join together to form a stronger unit. (Federica)
    • Province merge and split in one predicate (if possible) (Valerio)
    -------------------------------------------------------------------------------------------------
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

% Play the game
play:-
    generate_random_map(Map, false),
    spawn_provinces(Map, MapWithProvinces),
    print_map(MapWithProvinces).

% Test the code
test:-
    test_province,
    test_placements,
    test_purchases,
    test_farm,
    test_attack,
    test_end_turn,
    test_destroy_tower,
    test_merge,
    test_buy_and_merge,
    test_divide_money_after_attack,
    test_manhattan_distance,
    nl, writeln('-- All the tests have succeeded! ---'), nl, !.

% Generates the following map
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
    buy_and_place(Map, ProvinceRed2, peasant, NewUnitHex, Map2, ProvinceRed3),
    province_hexes(ProvinceRed3,ProvinceRed3Hexes),
    same_elements(ProvinceRed3Hexes, [hex(7,[1,2],_,red,none,none),hex(2,[0,2],_,red,none,none),hex(6,[1,1],_,red,none,none),hex(1,[0,1],_,red,none,peasant),hex(5,[1,0],_,red,none,none),hex(0,[0,0],_,red,none,peasant)]),
    writeln('Ok!'),
    print_map(Map2),

    % Test: displace_unit
    write('Testing unit displacement action... '),
    get_hex(Map2, [0,0], UnitDisplaceFrom),
    get_hex(Map2, [2,0], UnitDisplaceTo),
    displace_unit(Map2, ProvinceRed3, UnitDisplaceFrom, UnitDisplaceTo, Map3, ProvinceRed4),
    province_hexes(ProvinceRed4,ProvinceRed4Hexes),
    same_elements(ProvinceRed4Hexes, [hex(10,[2,0],_,red,none,peasant),hex(0,[0,0],_,red,none,none),hex(5,[1,0],_,red,none,none),hex(1,[0,1],_,red,none,peasant),hex(6,[1,1],_,red,none,none),hex(2,[0,2],_,red,none,none),hex(7,[1,2],_,red,none,none)]),
    writeln('Ok!'),
    print_map(Map3),

    % Test: wrong displace_unit
    write('Testing wrong unit displacement action near enemy tower... '),
    get_hex(Map2, [0,1], UnitDisplaceFrom1),
    get_hex(Map2, [2,1], UnitDisplaceTo1),
    \+ displace_unit(Map3, ProvinceRed4, UnitDisplaceFrom1, UnitDisplaceTo1, _, _),
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
    buy_and_place(Map, ProvinceBlue2, farm, BlueFarmHex, Map2, ProvinceBlue3),
    buy_and_place(Map2, ProvinceBlue3, farm, BlueFarmHex2, Map3, ProvinceBlue4),
    print_map(Map3),
    province_count(ProvinceBlue4, farm, FarmCount),
    FarmCount=2,
    writeln('Ok!'),

    % Test: first farm placement in red province
    write('Testing first farm placement in Red province... '),
    change_province_money(ProvinceRed,12,ProvinceRed2),
    get_hex(Map3, [1,0], RedFarmHex),
    buy_and_place(Map3, ProvinceRed2, farm, RedFarmHex, Map4, _),
    print_map(Map4),
    writeln('Ok!').
test_attack:-
    nl,writeln('test_attack ======================================================'),
    test_map(Map, [_, ProvinceBlue]),

    % Test blue unit attack
    write('Testing blue spearman attack red peasant... '),
    change_province_money(ProvinceBlue, 20, ProvinceBlue2),
    get_hex(Map, [1,2], BlueSpearmanHex),
    buy_and_place(Map, ProvinceBlue2, spearman, BlueSpearmanHex, Map2, ProvinceBlue3),
    print_map(Map2),

    get_hex(Map2, [1,2], BlueSpearmanHexFrom),
    get_hex(Map2, [1,1], BlueSpearmanHexTo),
    displace_unit(Map2, ProvinceBlue3, BlueSpearmanHexFrom, BlueSpearmanHexTo, Map3, ProvinceBlue4),
    print_map(Map3),

    get_hex(Map3, [1,1], BlueSpearmanHexFrom2),
    get_hex(Map3, [0,0], RedPeasantHex),
    displace_unit(Map3, ProvinceBlue4, BlueSpearmanHexFrom2, RedPeasantHex, Map4, ProvinceBlue5),
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
    test_map(Map, [ProvinceRed, _]),
    
    writeln('Testing wrong purchase: red spearman near blue tower... '),
    change_province_money(ProvinceRed, 30, ProvinceRed1),
    get_hex(Map, [1,2], DestHex),
    \+ buy_and_place(Map, ProvinceRed1, spearman, DestHex, _, _),
    writeln('Spearman succesfully not placed!'),nl,

    writeln('Testing purchase baron and blue tower destruction action by red... '),
    buy_and_place(Map, ProvinceRed1, baron, DestHex, Map1, ProvinceRed2),
    print_map(Map1),
    writeln('Baron succesfully placed! '),
    
    get_hex(Map1, [1,2], BaronHex),
    get_hex(Map1, [2,2], ToHex),
    displace_unit(Map1, ProvinceRed2, BaronHex, ToHex, Map2, _),
    print_map(Map2),
    writeln('Blue tower succesfully destroyed! '),
    writeln('Ok!').

test_merge:-
    nl,writeln('test_merge ======================================================'),
    test_map(Map, [ProvinceRed, _]),
    print_map(Map),
    % Test: displace_unit with merge
    write('Testing merge units with displacement... '),
    change_province_money(ProvinceRed, 40, ProvinceRed2),
    get_hex(Map, [0,1], NewUnitHex),
    buy_and_place(Map, ProvinceRed2, baron, NewUnitHex, Map2, ProvinceRed3),
    print_map(Map2),
    get_hex(Map2, [0,0], FromHex),
    get_hex(Map2, [0,1], DestHex),
    displace_unit(Map2, ProvinceRed3, FromHex, DestHex, NewMap, _),
    print_map(NewMap),
    writeln('Ok!').

test_buy_and_merge:-
    nl,writeln('test_buy_and_merge ======================================================'),
    test_map(Map, [ProvinceRed, _]),
    print_map(Map),
    % Test: buy_and_place with merge
    write('Testing buy and merge units... '),
    change_province_money(ProvinceRed, 100, ProvinceRed2),
    get_hex(Map, [0,0], NewUnitHex),
    \+ buy_and_place(Map, ProvinceRed2, knight, NewUnitHex, _, _),
    buy_and_place(Map, ProvinceRed2, baron, NewUnitHex, NewMap2, _),
    get_hex(NewMap2, [0,0], NewUnitHex2), % Get
    hex_unit(NewUnitHex2, knight), % Check
    print_map(NewMap2),
    writeln('Ok!').

test_divide_money_after_attack:-
    nl,writeln('test_divide_money_after_attack ======================================================'),
    test_map(Map, [_ProvinceRed, ProvinceBlue]),

    % Initial setup
    writeln('Initial Map:'),
    print_map(Map),

    change_province_money(ProvinceBlue, 20, ProvinceBlue2),
    get_hex(Map, [1,2], BlueSpearmanHex),
    buy_and_place(Map, ProvinceBlue2, spearman, BlueSpearmanHex, Map2, ProvinceBlue3),
    
    find_province(Map2, [1,0], ProvinceRed2),

    get_hex(Map2, [1,2], BlueSpearmanHexFrom),
    get_hex(Map2, [1,1], BlueSpearmanHexTo),
    displace_unit(Map2, ProvinceBlue3, BlueSpearmanHexFrom, BlueSpearmanHexTo, Map3, _ProvinceBlue4),
    writeln('Map after the attack:'),
    print_map(Map3),

    % Find the two new red provinces after the attack
    find_province(Map3, [1,0], NewProvinceRed1),
    find_province(Map3, [0,2], NewProvinceRed2),

    % Divide the money after the attack
    change_province_money(ProvinceRed2, 29, ProvinceRed3),
    writeln('Red province Total Money:'), writeln('29'),
    divide_money_after_attack(ProvinceRed3, NewProvinceRed1, NewProvinceRed2, NewProvinceRed1Updated, NewProvinceRed2Updated),

    % Check if the money has been correctly divided
    province_money(NewProvinceRed1Updated, Money1),
    province_money(NewProvinceRed2Updated, Money2),
    TotalMoney is Money1 + Money2,
    
    % Print the results
    writeln('Checking the division of money after the attack...'),
    writeln('New Total Money:'), writeln(TotalMoney),
    writeln('Money in NewProvinceRed1:'), writeln(Money1),
    writeln('Money in NewProvinceRed2:'), writeln(Money2),

    writeln('Ok!').

% Test Manhattan Distance <= 4 in Displace Unit
test_manhattan_distance:-
    nl,writeln('test_manhattan_distance ======================================================'),
    test_map(Map, [_ProvinceRed, ProvinceBlue]),

    % Purchase Peasant
    writeln('Purchasing Paesant:'),
    change_province_money(ProvinceBlue, 10, ProvinceBlue1),
    get_hex(Map, [3,4], BluePeasantHex),
    buy_and_place(Map, ProvinceBlue1, peasant, BluePeasantHex, Map1, ProvinceBlue2),
    print_map(Map1),

    % First check
    get_hex(Map1, [3,4], BluePeasantHexFrom),
    get_hex(Map1, [3,1], BluePeasantHexTo), % legal
    manhattan_distance([3,4], [3,1], Distance1),
    Distance1 =< 4,
    write('Legal move from [3,4] to [3,1], distance:'), write(Distance1),
    displace_unit(Map1, ProvinceBlue2, BluePeasantHexFrom, BluePeasantHexTo, Map2, ProvinceBlue3),
    print_map(Map2),

    % Second check
    get_hex(Map2, [3,1], BluePeasantHexFrom1),
    get_hex(Map2, [3,0], BluePeasantHexTo1),
    manhattan_distance([3,1], [3,0], Distance2),
    Distance2 =< 4,
    write('Legal move from [3,1] to [3,0], distance:'), write(Distance2),
    displace_unit(Map2, ProvinceBlue3, BluePeasantHexFrom1, BluePeasantHexTo1, Map3, ProvinceBlue4),
    print_map(Map3),

    % Third check
    get_hex(Map3, [3,0], BluePeasantHexFrom2),
    get_hex(Map3, [2,4], BluePeasantHexTo2), % illegal
    manhattan_distance([3,0], [2,4], Distance3),
    Distance3 > 4,
    write('Illegal move from [3,0] to [2,4], distance:'), write(Distance3),
    \+ displace_unit(Map3, ProvinceBlue4, BluePeasantHexFrom2, BluePeasantHexTo2, _, _),
    print_map(Map3),

    % Fourth check
    get_hex(Map3, [3,0], BluePeasantHexFrom3),
    get_hex(Map3, [3,4], BluePeasantHexTo3), % legal
    manhattan_distance([3,0], [3,4], Distance4),
    Distance4 =< 4,
    write('Legal move from [3,0] to [3,4], distance:'), write(Distance4),
    displace_unit(Map3, ProvinceBlue4, BluePeasantHexFrom3, BluePeasantHexTo3, Map4, _),
    print_map(Map4),

    writeln('Ok!').

