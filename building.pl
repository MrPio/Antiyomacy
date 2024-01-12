:- use_module("province").
:- module(building, [building/4,
    tower_nearby/4]).

% Building enum =========================================
% building(Name, Protection, Cost, Income) 
building(farm, 0, 12, 4).
building(tower, 2, 15, -1).
building(strong_tower, 3, 35, -6).
tower(X):-member(X,[tower,strong_tower]).

% Checks if there is an enemy tower nearby that prevents a unit move 
tower_nearby(Map,X,Y, Player):-
    % Look for towers
    boundary8(Map, X, Y, Hexes),
    member(Hex, Hexes),
    \+ hex_owner(Hex, Player),
    hex_building(Hex,tower)
    ;
    % Look for strong towers
    boundary24(Map, X, Y, Hexes),
    write(Hexes),
    member(Hex, Hexes),
    \+ hex_owner(Hex, Player),
    hex_building(Hex,strong_tower).
