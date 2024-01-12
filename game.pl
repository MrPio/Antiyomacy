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

play:-
    generate_random_map(Map),
    set_owner(Map,0,0,red,Map1),
    set_owner(Map1,1,0,red,Map2),
    set_owner(Map2,1,1,red,Map3),
    set_owner(Map3,1,2,red,Map4),
    set_owner(Map4,0,2,red,Map5),
    set_owner(Map5,2,2,blue,Map6),
    set_owner(Map6,2,1,blue,Map7),
    set_building(Map7,2,2,strong_tower,Map8),
    find_provinces(Map8,[P1| _]),
    province_boundary(Map7,P1,B),
    tower_nearby(Map8,0,0,red).
