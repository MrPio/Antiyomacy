:- consult("map.pl").
:- consult("province.pl").
:- consult("hex.pl").
:- consult("unit.pl").
:- consult("building.pl").
:- consult("economy.pl").
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
play:-
    generate_random_map(Map),
    set_owner(Map,0,0,red,Map1),
    set_owner(Map1,1,0,red,Map2),
    set_owner(Map2,1,1,red,Map3),
    set_owner(Map3,1,2,red,Map4),
    set_owner(Map4,0,2,red,Map5),
    set_owner(Map5,2,2,blue,Map6),
    set_owner(Map6,3,2,blue,Map7),
    set_owner(Map7,3,3,blue,Map8),
    set_owner(Map8,3,4,blue,Map9),
    set_owner(Map9,4,3,blue,Map10),
    set_building(Map10,2,2,tower,Map11),
    set_unit(Map11,0,0,peasant,Map12),
    find_provinces(Map12,[ProvinceRed,ProvinceBlue]),
    % province_boundary(Map12,ProvinceRed,B),
    % tower_nearby(Map12,0,0,red),
    % get_income(ProvinceBlue,P2Income),
    % change_province_money(ProvinceBlue,24,P),
    % findall(B,(buy(P,B)),BList),
    % frontier(Map12,ProvinceRed,Frontier),
    get_hex(Map12,0,0,HexRedWithUnit),
    findall(Dest,move_unit(Map12,ProvinceRed,HexRedWithUnit,Dest),DestList),
    write(DestList),
    nl.
