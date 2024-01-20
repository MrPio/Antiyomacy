:- use_module([printer, map, province, unit, building, economy]).

/* TODO:
    • Baron and Knight should ignore the enemy towers. (Federico)
    • At the beginning of the game, the two provinces are randomly generated and located far apart.
    • Test units attack. (Valerio)
    • Province money managment and bankruptcy. (Valerio)
    • Test province split due to enemy attack. Money should split based on provinces size.
    • Two units of the same level may join together to form a stronger unit.
    • Trees cannot randomly spawn during the gameplay. Instead they can spawn at the beginning
    of the game, or supply centers can be introduced.
    -------------------------------------------------------------------------------------------------
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
    • Library "clpfd" was used to achieve two-way unifications.
    • Tests were used to prevent any issues with code updates affecting existing functionalities.
    • The terrain on the map was randomly generated using the random walker's algorithm.
    • To follow the CBDP philosophy, module/2 and use_module/1 were used instead of consult/1.
*/


% Test the code
test:-
    test_province,
    test_placements,
    test_purchases,
    test_farm,
    nl, writeln('-- All the tests have succeeded! ---'), nl, !.

% Generates the following map
%    0 1 2 3 4
% 0 |r| |r| | |
% 1 |r|r|r| | |
% 2 | | |b| | |
% 3 | | |b|b|b|
% 4 | | | |b| |
test_map(Map):-
    generate_random_map(_),

    % Manually populating the map
    write('Populating the map... '),
    RedHexes=[[0,0],[1,0],[1,1],[1,2],[0,2]],
    BlueHexes=[[2,2],[3,2],[3,3],[3,4],[4,3]],
    Buildings=[[2,2]-tower],
    Units=[[0,0]-peasant],
    foreach(member(Coord,RedHexes),set_owner(Coord,red)),
    foreach(member(Coord,BlueHexes),set_owner(Coord,blue)),
    foreach(member(Coord-Building,Buildings),set_building(Coord,Building)),
    foreach(member(Coord-Unit,Units),set_unit(Coord,Unit)),
    map(Map), print_map(Map).

test_province:-
    test_map(Map),

    % Test: find_provinces
    write('Testing provinces calculation... '),
    find_provinces(Map, [ProvinceRed, ProvinceBlue]),
    ProvinceRed = province(red,[hex(2,[0,2],_,red,none,none),hex(7,[1,2],_,red,none,none),hex(6,[1,1],_,red,none,none),hex(5,[1,0],_,red,none,none),hex(0,[0,0],_,red,none,peasant)],0),
    ProvinceBlue = province(blue,[hex(19,[3,4],_,blue,none,none),hex(23,[4,3],_,blue,none,none),hex(18,[3,3],_,blue,none,none),hex(17,[3,2],_,blue,none,none),hex(12,[2,2],_,blue,tower,none)],0),
    writeln('Ok!'),

    % Test: outer_border
    write('Testing province boundary calculation... '),
    outer_border(Map,ProvinceRed,ProvinceRedBoundary),
    ProvinceRedBoundary=[hex(1,[0,1],_,none,none,none),hex(3,[0,3],_,none,none,none),hex(8,[1,3],_,none,none,none),hex(11,[2,1],_,none,none,none),hex(12,[2,2],_,blue,tower,none),hex(13,[2,3],_,none,none,none),hex(10,[2,0],_,none,none,none)],
    writeln('Ok!'),

    % Test: inner_border
    write('Testing province inner_border calculation... '),
    inner_border(Map,ProvinceRed,InnerBorder),
    InnerBorder = [hex(2,[0,2],_,red,none,none),hex(7,[1,2],_,red,none,none),hex(6,[1,1],_,red,none,none),hex(5,[1,0],_,red,none,none),hex(0,[0,0],_,red,none,peasant)],
    writeln('Ok!'),

    % Test: get_income
    write('Testing province income calculation... '),
    get_income(ProvinceBlue, 4),
    writeln('Ok!').

test_placements:-
    test_map(Map),
    find_provinces(Map, [ProvinceRed, _]),

    % Test: unit_placement
    write('Testing units placements... '),
    findall(Dest,unit_placement(Map, ProvinceRed, peasant, Dest),DestList),
    DestList = [hex(2,[0,2],_,red,none,none),hex(5,[1,0],_,red,none,none),hex(1,[0,1],_,none,none,none),hex(3,[0,3],_,none,none,none),hex(10,[2,0],_,none,none,none)],
    writeln('Ok!'),

    % Test: building_placement
    write('Testing buildings placements... '),
    findall(Dest, building_placement(Map, ProvinceRed, tower, Dest), DestList2),
    DestList2 = [hex(2,[0,2],_,red,none,none),hex(7,[1,2],_,red,none,none),hex(6,[1,1],_,red,none,none),hex(5,[1,0],_,red,none,none)],
    writeln('Ok!'),

    % Test: tower_nearby
    write('Testing nearby towers detection... '),
    tower_nearby(Map,[2,1],red),
    writeln('Ok!').

test_purchases:-
    test_map(Map),
    find_provinces(Map, [ProvinceRed, ProvinceBlue]),

    % Test: check_buys
    write('Testing purchase actions listing... '),
    change_province_money(ProvinceBlue, 24, ProvinceBlue2),
    findall(Resource, (check_buys(ProvinceBlue2, Resource, _)), ResourcesList),
    ResourcesList = [[farm,farm],[farm,peasant],[farm],[tower],[peasant,farm],[peasant,peasant],[peasant],[spearman],[]],
    writeln('Ok!'),

    % Test: buy_and_place
    write('Testing purchase action... '),
    change_province_money(ProvinceRed, 16, ProvinceRed2),
    get_hex(Map, [0,1], NewUnitHex),
    buy_and_place(Map, ProvinceRed2, peasant, NewUnitHex, Map2, ProvinceRed3),
    ProvinceRed3 = province(red,[hex(7,[1,2],_,red,none,none),hex(2,[0,2],_,red,none,none),hex(6,[1,1],_,red,none,none),hex(1,[0,1],_,red,none,peasant),hex(5,[1,0],_,red,none,none),hex(0,[0,0],_,red,none,peasant)],6),
    writeln('Ok!'),
    print_map(Map2),

    % Test: displace_unit
    write('Testing unit displacement action... '),
    get_hex(Map2, [0,0], UnitDisplaceFrom),
    get_hex(Map2, [2,0], UnitDisplaceTo),
    displace_unit(Map2, ProvinceRed3, UnitDisplaceFrom, UnitDisplaceTo, Map3, ProvinceRed4),
    ProvinceRed4 = province(red,[hex(10,[2,0],_,red,none,peasant),hex(0,[0,0],_,red,none,none),hex(5,[1,0],_,red,none,none),hex(1,[0,1],_,red,none,peasant),hex(6,[1,1],_,red,none,none),hex(2,[0,2],_,red,none,none),hex(7,[1,2],_,red,none,none)],6),
    writeln('Ok!'),
    print_map(Map3),

    % Test: wrong displace_unit
    write('Testing wrong unit displacement action near enemy tower... '),
    get_hex(Map2, [0,1], UnitDisplaceFrom1),
    get_hex(Map2, [2,1], UnitDisplaceTo1),
    \+ displace_unit(Map3, ProvinceRed4, UnitDisplaceFrom1, UnitDisplaceTo1, _, _),
    writeln('Ok!').

test_farm:-
    test_map(Map),
    find_provinces(Map, [ProvinceRed, ProvinceBlue]),

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