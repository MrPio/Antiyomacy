:- use_module([printer, map, province, unit, building, economy]).

% Test the code
%    0 1 2 3 4
% 0 |r| |r| | |
% 1 |r|r|r| | |
% 2 | | |b| | |
% 3 | | |b|b|b|
% 4 | | | |b| |
test:-
    write('Generating random terrain... '),
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
    map(Map), print_map(Map),

    % Test: find_provinces
    write('Testing provinces calculation... '),
    find_provinces([ProvinceRed, ProvinceBlue]),
    ProvinceRed = province(red,[hex(2,[0,2],_,red,none,none),hex(7,[1,2],_,red,none,none),hex(6,[1,1],_,red,none,none),hex(5,[1,0],_,red,none,none),hex(0,[0,0],_,red,none,peasant)],0),
    ProvinceBlue = province(blue,[hex(19,[3,4],_,blue,none,none),hex(23,[4,3],_,blue,none,none),hex(18,[3,3],_,blue,none,none),hex(17,[3,2],_,blue,none,none),hex(12,[2,2],_,blue,tower,none)],0),
    writeln('Ok!'),

    % Test: province_boundary
    write('Testing province boundary calculation... '),
    province_boundary(Map,ProvinceRed,ProvinceRedBoundary),
    ProvinceRedBoundary=[hex(1,[0,1],_,none,none,none),hex(3,[0,3],_,none,none,none),hex(8,[1,3],_,none,none,none),hex(11,[2,1],_,none,none,none),hex(12,[2,2],_,blue,tower,none),hex(13,[2,3],_,none,none,none),hex(10,[2,0],_,none,none,none)],
    writeln('Ok!'),

    % Test: frontier
    write('Testing province frontier calculation... '),
    frontier(Map,ProvinceRed,Frontier),
    Frontier = [hex(2,[0,2],_,red,none,none),hex(7,[1,2],_,red,none,none),hex(6,[1,1],_,red,none,none),hex(5,[1,0],_,red,none,none),hex(0,[0,0],_,red,none,peasant)],
    writeln('Ok!'),

    % Test: get_income
    write('Testing province income calculation... '),
    get_income(ProvinceBlue, 4),
    writeln('Ok!'),

    % Test: move_unit
    write('Testing units moves... '),
    get_hex([0,0],HexRedWithUnit),
    findall(Dest,move_unit(Map,ProvinceRed,HexRedWithUnit,Dest),DestList),
    DestList = [hex(2,[0,2],_,red,none,none),hex(5,[1,0],_,red,none,none),hex(1,[0,1],_,none,none,none),hex(3,[0,3],_,none,none,none),hex(10,[2,0],_,none,none,none)],
    writeln('Ok!'),

    % Test: tower_nearby
    write('Testing nearby towers detection... '),
    tower_nearby(Map,[2,1],red),
    writeln('Ok!'),

    % Test: buy
    write('Testing buy actions... '),
    change_province_money(ProvinceBlue,24,ProvinceBlue2),
    findall(Building,(buy(ProvinceBlue2,Building)),BuildingsList),
    BuildingsList = [farm,tower,peasant,spearman],
    writeln('Ok!'),

    nl, writeln('-- All the tests have succeeded! ---'), nl, !.
