:- module(building, [building/4,
    building_cost/3,
    count_farms_in_province/2,
    tower_nearby/3,
    building_placement/3]).
:- use_module([hex, province]).

% Building enum ==============================================
% building(Name, Protection, BaseCost, Income) 
building(farm,         0,     12,      4).
building(tower,        2,     15,     -1).
building(strong_tower, 3,     35,     -6).
tower(X) :- member(X,[tower, strong_tower]).

building_cost(BuildingName, Province, Cost) :-
    building(BuildingName, _, BaseCost, _),
    (BuildingName==farm, 
    count_farms_in_province(Province, FarmCount),
    Cost is BaseCost + 2 * FarmCount,
      format('Costo per farm: ~w (FarmCount: ~w, BaseCost: ~w)\n', [Cost, FarmCount, BaseCost])
    ;
    BuildingName \= farm,
    Cost is BaseCost).

% count_farms_in_province(+Province, -FarmCount)
count_farms_in_province(province(_, Hexes, _), FarmCount) :-
    % Use findall to create a list of 1s for each farm in the province
    (findall(Hex, (
        member(Hex, Hexes),
        hex_building(Hex, farm)
    ), Farms) -> sumlist(Farms, FarmCount) ; FarmCount = 0).

% Checks if there is an enemy tower nearby that prevents a unit move 
% tower_nearby(+Map, +Coord, +ToHex)
tower_nearby(Map, [X, Y], Player) :-
    % Look for enemy towers
    near8(Map, [X, Y], Hexes),
    member(Hex, Hexes),
    \+ hex_owner(Hex, Player),
    hex_building(Hex, tower)
    ;
    % Look for enemy strong towers
    near24(Map, [X, Y], Hexes),
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