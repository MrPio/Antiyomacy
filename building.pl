:- module(building, [building/4,
    building_cost/3,
    tower_nearby/3,
    strong_tower_nearby/3,
    farm_nearby/3,
    building_placement/4]).
:- use_module([map, hex, province]).

% Building enum ==============================================
% building(Name, Protection, BaseCost, Income) 
building(farm,         0,     12,      4).
building(tower,        2,     15,     -1).
building(strong_tower, 3,     35,     -6).
tower(X) :- member(X,[tower, strong_tower]).

% Calculates the cost of the building based on its type
% building_cost(+BuildingName, +Province, -Cost)
building_cost(farm, Province, Cost) :-
    building(farm, _, BaseCost, _),
    province_count(Province, farm, FarmCount),
    Cost is BaseCost + 2 * FarmCount, !.
building_cost(BuildingName, _, Cost) :- 
    building(BuildingName, _, Cost, _).

% Checks if there is an enemy tower nearby that prevents a unit move 
% tower_nearby(+Map, +Coord, +Player)
tower_nearby(Map, [X, Y], Player) :-
    % Look for enemy towers
    near8(Map, [X, Y], Hexes),
    (member(Hex, Hexes); get_hex(Map, [X,Y], Hex)),
    \+ hex_owner(Hex, Player),
    hex_building(Hex, tower).

% Checks if there is a enemy strong_tower nearby that prevents a unit move 
% tower_nearby(+Map, +Coord, +Player)
strong_tower_nearby(Map, [X, Y], Player) :-
    % Look for enemy strong towers
    near24(Map, [X, Y], Hexes),
    (member(Hex, Hexes); get_hex(Map, [X,Y], Hex)),
    \+ hex_owner(Hex, Player),
    hex_building(Hex, strong_tower).

% Checks if there is a farm nearby, useful to check where a farm can be placed
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
% Checks/Gets a building valid location on the given province
% This is useful to list all the possible placements moves for a given building
% Note: It is assumed that a building that is not a farm can only be constructed
%       on the inner border of the province, not within it. Farms, on the other hand,
%       should be placed near other farms
% building_placement(+Map, +Province, +BuildingName, ?ToHex) [non-deterministic]
building_placement(Map, Province, farm, Hex) :-
    % Find one possible destination / Check the destination validity on the province
    province_hexes(Province, Hexes),
    member(Hex, Hexes),
    % The destination should not host any units or buildings
    hex_unit(Hex, none), % Check
    hex_building(Hex, none), % Check
    (
        % This should be the first farm placed in the province
        province_count(Province, farm, 0) % Check
        ;
        % Otherwise, it should be placed near another farm
        hex_coord(Hex, HexCoord), % Get
        farm_nearby(Map, HexCoord, Province)
    ).
building_placement(Map, Province, _, Hex) :-
    % Find one possible destination / Check the destination validity on the province
    inner_border(Map, Province, InnerBorder),
    member(Hex, InnerBorder),
    % The destination should not host any units or buildings
    hex_unit(Hex, none), % Check
    hex_building(Hex, none). % Check