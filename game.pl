:- consult("map.pl").
:- consult("province.pl").
:- consult("hex.pl").
:- consult("unit.pl").
:- consult("building.pl").
:- consult("economy.pl").
:- consult("printer.pl").
:- use_module(printer).
:- use_module(map).
:- use_module(province).
:- use_module(unit).
:- use_module(building).

% Test the code
%    0 1 2 3 4
% 0 |r| |r| | |
% 1 |r|r|r| | |
% 2 | | |b| | |
% 3 | | |b|b|b|
% 4 | | | |b| |
test:-
    generate_random_map(_),

    % Manually populating the map
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
    find_provinces([ProvinceRed, ProvinceBlue]),
    ProvinceRed= province(red,[hex(2,[0,2],_,red,none,none),hex(7,[1,2],_,red,none,none),hex(6,[1,1],_,red,none,none),hex(5,[1,0],_,red,none,none),hex(0,[0,0],_,red,none,peasant)],0),
    ProvinceBlue = province(blue,[hex(19,[3,4],_,blue,none,none),hex(23,[4,3],_,blue,none,none),hex(18,[3,3],_,blue,none,none),hex(17,[3,2],_,blue,none,none),hex(12,[2,2],_,blue,tower,none)],0),

    % Test: province_boundary
    province_boundary(Map,ProvinceRed,ProvinceRedBoundary),
    ProvinceRedBoundary=[hex(1,[0,1],_,none,none,none),hex(3,[0,3],_,none,none,none),hex(8,[1,3],_,none,none,none),hex(11,[2,1],_,none,none,none),hex(12,[2,2],_,blue,tower,none),hex(13,[2,3],_,none,none,none),hex(10,[2,0],_,none,none,none)],
    
    % Test: tower_nearby
    tower_nearby(Map,[2,1],red),

    % Test: tower_nearby
    get_income(ProvinceBlue, 4),

    % Test: buy
    change_province_money(ProvinceBlue,24,ProvinceBlue2),
    findall(Building,(buy(ProvinceBlue2,Building)),BuildingsList),
    BuildingsList = [farm,tower,peasant,spearman],

    % Test: frontier
    frontier(Map,ProvinceRed,Frontier),
    Frontier = [hex(2,[0,2],_,red,none,none),hex(7,[1,2],_,red,none,none),hex(6,[1,1],_,red,none,none),hex(5,[1,0],_,red,none,none),hex(0,[0,0],_,red,none,peasant)],

    % Test: move_unit
    get_hex([0,0],HexRedWithUnit),
    findall(Dest,move_unit(Map,ProvinceRed,HexRedWithUnit,Dest),DestList),
    DestList = [hex(2,[0,2],_,red,none,none),hex(5,[1,0],_,red,none,none),hex(1,[0,1],_,none,none,none),hex(3,[0,3],_,none,none,none),hex(10,[2,0],_,none,none,none)].
