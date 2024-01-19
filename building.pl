:- module(building, [building/4,
    building_cost/3,
    count_farms_in_province/2,
    tower_nearby/3,
    farm_nearby/3,
    building_placement/4]).
:- use_module([hex, province]).

% Building enum ==============================================
% building(Name, Protection, BaseCost, Income) 
building(farm,         0,     12,      4).
building(tower,        2,     15,     -1).
building(strong_tower, 3,     35,     -6).
tower(X) :- member(X,[tower, strong_tower]).

building_cost(BuildingName, Province, Cost) :-
    building(BuildingName, _, BaseCost, _),
    (
        BuildingName==farm, 
        count_farms_in_province(Province, FarmCount),
        Cost is BaseCost + 2 * FarmCount
        ;
        BuildingName \= farm,
        Cost is BaseCost
        ).

% count_farms_in_province(+Province, -FarmCount)
count_farms_in_province(province(_, Hexes, _), FarmCount) :-
    % Use findall to create a list of 1s for each farm in the province
    findall(Hex, (
        member(Hex, Hexes),
        hex_building(Hex, farm)
    ), HexesWithFarm) -> length(HexesWithFarm, FarmCount) ; FarmCount = 0.

% Checks if there is an enemy tower nearby that prevents a unit move 
% tower_nearby(+Map, +Coord, +Player)
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

% Checks if there is a farm nearby 
% farm_nearby(+Map, +Coord, +Province)
farm_nearby(Map, [X, Y], Province) :-
    % Select one hex from the adjacent ones
    near8(Map, [X, Y], NearbyHexes),
    member(Hex, NearbyHexes),
    % Ensure the selected hex is inside the province
    province_hexes(Province, ProvinceHexes),
    member(Hex, ProvinceHexes),
    % Check if the hex hosts a farm
    hex_building(Hex, farm).
    

% Moves ======================================================
% Checks/Get a building valid location on the given province
% This is useful to list all the possible placements moves for a given building
% building_placement(+Map, +Province, +BuildingName, ?ToHex)
building_placement(Map, Province, farm, Hex) :-
    % Find one possible destination / Check the destination validity on the province
    buildings_location(Map, Province, farm, Hex),
    % The destination should not host any units or buildings
    hex_unit(Hex, none), % Check
    hex_building(Hex, none), % Check
    (
        % This should be the first farm placed in the province
        count_farms_in_province(Province, 0) % Check
        ;
        % Otherwise, it should be placed near another farm
        hex_coord(Hex, HexCoord), % Get
        farm_nearby(Map, HexCoord, Province)
    ).
building_placement(Map, Province, BuildingName, Hex) :-
    % Find one possible destination / Check the destination validity on the province
    buildings_location(Map, Province, BuildingName, Hex),
    % The destination should not host any units or buildings
    hex_unit(Hex, none), % Check
    hex_building(Hex, none). % Check