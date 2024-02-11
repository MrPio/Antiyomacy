:- module(test,[test/0, test_map4/2]).
:- use_module([utils, map, hex, province, unit, building, economy, eval, minimax, io]).

% Run all the tests
test:-
    retractall(map_size(_)), assertz(map_size(5)),
    test_province,
    test_placements,
    test_purchases,
    test_farm,
    test_attack,
    test_end_turn,
    test_destroy_tower,
    test_displace_with_merge,
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
% Generate the following test map
%    0 1 2 3 4
% 0 |r| | | | |
% 1 |r| | | | |     
% 2 | | | | | |
% 3 | | | | |b|
% 4 | | | | |b|
test_map4(MapWithProvinces, [ProvinceRedSorted, ProvinceBlueSorted]):-
    generate_random_map(_, true),
    % Manually populating the map
    RedHexes=[[0,0],[1,0]],
    BlueHexes=[[4,4],[3,4]],
    Units=[],
    Buildings=[],
    foreach(member(Coord,RedHexes),set_owner(Coord,red)),
    foreach(member(Coord,BlueHexes),set_owner(Coord,blue)),
    foreach(member(Coord-Building,Buildings),set_building(Coord,Building)),
    foreach(member(Coord-Unit,Units),set_unit(Coord,Unit)),
    map(MapWithProvinces),
    print_map(MapWithProvinces),
    find_provinces(MapWithProvinces, Provinces),
    Provinces=[ProvinceRed, ProvinceBlue],
    change_province_money(ProvinceRed, 12, ProvinceRed2),
    change_province_money(ProvinceBlue, 12, ProvinceBlue2),
    update_province(MapWithProvinces, ProvinceRed2, ProvinceRedSorted),
    update_province(MapWithProvinces, ProvinceBlue2, ProvinceBlueSorted),
    print_provinces([ProvinceRedSorted, ProvinceBlueSorted]).

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
    get_income(ProvinceBlue, 2),
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
    displace_unit(Map2, [ProvinceRed3, ProvinceBlue2], ProvinceRed3, UnitDisplaceFrom, UnitDisplaceTo, peasant, Map3, _, ProvinceRed4),
    province_hexes(ProvinceRed4,ProvinceRed4Hexes),
    same_elements(ProvinceRed4Hexes, [hex(10,[2,0],_,red,none,peasant),hex(0,[0,0],_,red,none,none),hex(5,[1,0],_,red,none,none),hex(1,[0,1],_,red,none,peasant),hex(6,[1,1],_,red,none,none),hex(2,[0,2],_,red,none,none),hex(7,[1,2],_,red,none,none)]),
    writeln('Ok!'),
    print_map(Map3),

    % Test: wrong displace_unit
    write('Testing wrong unit displacement action near enemy tower... '),
    get_hex(Map2, [2,1], UnitDisplaceTo1),
    \+ unit_placement(Map3, ProvinceRed4, peasant, UnitDisplaceTo1, _),
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
    displace_unit(Map2, [ProvinceRed, ProvinceBlue3], ProvinceBlue3, BlueSpearmanHexFrom, BlueSpearmanHexTo, spearman, Map3, _, ProvinceBlue4),
    print_map(Map3),

    get_hex(Map3, [1,1], BlueSpearmanHexFrom2),
    get_hex(Map3, [0,0], RedPeasantHex),
    displace_unit(Map3, [ProvinceRed, ProvinceBlue4], ProvinceBlue4, BlueSpearmanHexFrom2, RedPeasantHex, spearman, Map4, _, ProvinceBlue5),
    print_map(Map4),
    province_hexes(ProvinceBlue5, ProvinceBlue5Hexes),
    same_elements(ProvinceBlue5Hexes, [hex(0,[0,0],_,blue,none,spearman),hex(7,[1,2],_,blue,none,none),hex(6,[1,1],_,blue,none,none),hex(17,[3,2],_,blue,none,none),hex(12,[2,2],_,blue,tower,none),hex(23,[4,3],_,blue,none,none),hex(18,[3,3],_,blue,none,none),hex(19,[3,4],_,blue,none,none)]),
    writeln('Ok!').

test_end_turn:-
    nl,writeln('test_end_turn ======================================================'),
    test_map(Map, [ProvinceRed, _]),

    % Test apply_income without bankrupt
    % write('Testing red province income calculation... '),
    % apply_income(Map, ProvinceRed, Map2, ProvinceRed2),
    % province_money(ProvinceRed2, 3), % Check
    % writeln('Ok!'),

    % Test apply_income with bankrupt
    write('Testing red province bankrupt... '),
    set_unit(Map, [0,0], knight, Map2),
    set_unit(Map2, [1,0], knight, Map3),
    update_province(Map3, ProvinceRed, ProvinceRed3),
    apply_income(Map3, ProvinceRed3, _, ProvinceRed4),
    province_count(ProvinceRed4, peasant, 0), % Check
    province_count(ProvinceRed4, spearman, 0), % Check
    province_money(ProvinceRed4, 0), % Check
    get_income(ProvinceRed4, 3), % Check
    writeln('Ok!').

test_destroy_tower :-
    nl,writeln('test_destroy_tower ======================================================'),
    test_map(Map, [ProvinceRed, ProvinceBlue]),

    writeln('Testing wrong purchase: red spearman near blue tower... '),
    change_province_money(ProvinceRed, 30, ProvinceRed1),
    get_hex(Map, [1,2], DestHex),
    \+ unit_placement(Map, ProvinceRed1, spearman, DestHex, _),
    writeln('Spearman succesfully not placed!'),nl,

    writeln('Testing purchase baron and blue tower destruction action by red... '),
    buy_and_place(Map, [ProvinceRed1, ProvinceBlue], ProvinceRed1, baron, DestHex, Map1, _, ProvinceRed2),
    print_map(Map1),
    writeln('Baron succesfully placed! '),

    get_hex(Map1, [1,2], BaronHex),
    get_hex(Map1, [2,2], ToHex),
    displace_unit(Map1, [ProvinceRed2, ProvinceBlue], ProvinceRed2, BaronHex, ToHex, baron, Map2, _, _),
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
    displace_unit(Map2, [ProvinceRed3, ProvinceBlue], ProvinceRed3, FromHex, DestHex, peasant, NewMap, _, _),
    print_map(NewMap),
    writeln('Ok!').

test_share_money:-
    nl,writeln('test_share_money ======================================================'),
    test_map2(Map, [ProvinceBlue, ProvinceRedEst, ProvinceRedWest]),
    print_map(Map),

    print_provinces([ProvinceBlue, ProvinceRedEst, ProvinceRedWest]),
    writeln('testing attack with provinces merge + split'),
    change_province_money(ProvinceBlue, 13, ProvinceBlue1),
    change_province_money(ProvinceRedEst, 17, ProvinceRedEst1),
    change_province_money(ProvinceRedWest, 50, ProvinceRedWest1),
    get_hex(Map, [2,2], RedAttackHex),
    buy_and_place(Map, [ProvinceBlue1, ProvinceRedEst1, ProvinceRedWest1], ProvinceRedWest1, baron, RedAttackHex, Map2, NewProvinces, _),
    same_elements(NewProvinces,[
        province(red,
            [   hex(12,[2,2],_,red,none,baron),
                hex(13,[2,3],_,red,none,none),
                hex(14,[2,4],_,red,none,none),
                hex(15,[3,0],_,red,none,none),
                hex(16,[3,1],_,red,none,none),
                hex(20,[4,0],_,red,none,none)
            ],
        37),
        province(blue,
            [ hex(7,[1,2],_,blue,none,none),
                hex(1,[0,1],_,blue,none,none),
                hex(6,[1,1],_,blue,none,none)
            ],
        7),
        province(blue,
            [ hex(22,[4,2],_,blue,none,none),
                hex(17,[3,2],_,blue,none,none)
            ],
        5)
    ]),
    print_map(Map2),
    print_provinces(NewProvinces),
    writeln('Ok!').

test_manhattan_distance:-
    nl,writeln('test_manhattan_distance ======================================================'),
    test_map(Map, [_ProvinceRed, _ProvinceBlue]),

    % First check
    get_hex(Map, [3,4], BluePeasantHexFrom),
    get_hex(Map, [3,1], BluePeasantHexTo), % legal
    manhattan_distance(BluePeasantHexFrom, BluePeasantHexTo, Distance1),
    Distance1 =< 4,
    write('Legal move from [3,4] to [3,1], distance:'), write(Distance1),
    % Second check
    get_hex(Map, [3,1], BluePeasantHexFrom1),
    get_hex(Map, [3,0], BluePeasantHexTo1),
    manhattan_distance(BluePeasantHexFrom1, BluePeasantHexTo1, Distance2),
    Distance2 =< 4,
    write('Legal move from [3,1] to [3,0], distance:'), write(Distance2),

    % Third check
    get_hex(Map, [3,0], BluePeasantHexFrom2),
    get_hex(Map, [2,4], BluePeasantHexTo2), % illegal
    manhattan_distance(BluePeasantHexFrom2, BluePeasantHexTo2, Distance3),
    Distance3 > 4,
    write('Illegal move from [3,0] to [2,4], distance:'), write(Distance3),

    % Fourth check
    get_hex(Map, [3,0], BluePeasantHexFrom3),
    get_hex(Map, [3,4], BluePeasantHexTo3), % legal
    manhattan_distance(BluePeasantHexFrom3, BluePeasantHexTo3, Distance4),
    Distance4 =< 4,
    write('Legal move from [3,0] to [3,4], distance:'), write(Distance4),

    writeln('Ok!').

test_destroy_province:-
    nl,writeln('test_destroy_province ======================================================'),
    test_map2(Map, [ProvinceBlue, _ProvinceRedEst, ProvinceRedWest]),
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

test_io:-
    nl,writeln('test_io ======================================================'),
    writeln('Test color choice'),
    ask_color(_),
    writeln('Test move choice'),
    player_move(_),
    writeln('Test displace_input'),
    displace_input(_, _),
    writeln('Test purchase_input'),
    purchase_input(_,_),
    writeln('Ok!').