:- module(unit, [unit/5,
    unit_placement/4]).
:- use_module([hex, province, building]).

% Unit enum =================================================
% unit(Name, Strength, Protection, Cost, Income)
unit(peasant,   1,          1,      10,   -2).
unit(spearman,  2,          2,      20,   -6).
unit(baron,     3,          3,      30,   -18).
unit(knight,    4,          3,      40,   -36).

% Determine whether one unit or one building can be destroyed by another unit
stronger(UnitName1, UnitName2) :-
    unit(UnitName1, Strength1, _, _, _), % Get
    unit(UnitName2, _, Protection2, _, _), % Get
    Strength1>Protection2.
stronger(UnitName1, BuildingName1) :-
    unit(UnitName1, Strength1, _, _, _), % Get
    building(BuildingName1, Protection2, _, _), % Get
    Strength1>Protection2.
    
% Moves ======================================================
% Checks/Get a unit valid location on the given province
% This is useful to list all the possible displacement moves for a given unit
% Note: the FromHex hex is not a valid destination as the unit resides within it
% unit_placement(+Map, +Province, +UnitName, ?Hex)
unit_placement(Map, Province, UnitName, Hex) :-
    unit(UnitName, _, _, _, _), % Check
    % Find one possible destination
    units_location(Map, Province, Hex),
    hex_unit(Hex, UnitAtDest), % Get
    hex_owner(Hex, OwnerAtDest), % Get
    hex_building(Hex, BuildAtDest), % Get
    province_owner(Province, Player), % Get
    hex_coord(Hex, [X, Y]), % Get
    % The destination should not host any allied units or stronger enemy units
    (   UnitAtDest == none 
    ;   OwnerAtDest \= Player,
        stronger(UnitName, UnitAtDest)
    ),
    % The destination should not host a building
    (   BuildAtDest == none
    ;   OwnerAtDest \= Player,
        stronger(UnitName, BuildAtDest)
    ),
    % Depending on the unit type, the destination should not be located 
    % near an enemy tower or strong_tower
    (   \+ member(UnitName, [baron, knight]),
        \+ tower_nearby(Map, [X, Y], Player),
        \+ strong_tower_nearby(Map, [X, Y], Player)
    ;   UnitName == baron,
        \+ strong_tower_nearby(Map, [X, Y], Player)
    ;   UnitName == knight
    ).
