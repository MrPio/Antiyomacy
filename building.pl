:- module(building, [building/4,
    tower_nearby/3,
    building_placement/3]).
:- use_module([hex, province]).

% Building enum ==============================================
% building(Name, Protection, Cost, Income) 
building(farm,         0,     12,    4).
building(tower,        2,     15,   -1).
building(strong_tower, 3,     35,   -6).
tower(X) :- member(X,[tower, strong_tower]).

% Checks if there is an enemy tower nearby that prevents a unit move 
% tower_nearby(+Map, +Coord, +ToHex)
tower_nearby(Map, [X, Y], Player) :-
    % Look for enemy towers
    boundary8(Map, [X, Y], Hexes),
    member(Hex, Hexes),
    \+ hex_owner(Hex, Player),
    hex_building(Hex, tower)
    ;
    % Look for enemy strong towers
    boundary24(Map, [X, Y], Hexes),
    member(Hex, Hexes),
    \+ hex_owner(Hex, Player),
    hex_building(Hex, strong_tower).

% Moves ======================================================
% Checks/Get a building valid location on the given province
% This is useful to list all the possible placements moves for a given building
% building_placement(+Map, +Province, ?ToHex)
building_placement(Map, Province, Hex) :-
    % Find one possible destination
    buildings_location(Map, Province, Hex),
    % The destination should not host any units or building
    hex_unit(Hex, none), % Get
    hex_building(Hex, none). % Get