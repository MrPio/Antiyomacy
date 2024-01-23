:- module(unit, [unit/5,
    unit_placement/4,
    unit_merge/3]).
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
% Checks/Gets a unit valid location on the given province
% This is useful to list all the possible displacement moves for a given unit
% Note: It is assumed that a unit can only be placed on the inner or outer
%       border of a province, not within it.
% Note: the FromHex hex is not a valid destination as the unit resides within it
% unit_placement(+Map, +Province, +UnitName, ?Hex) [non-deterministic]
unit_placement(Map, Province, UnitName, Hex) :-
    unit(UnitName, Strength1, _, _, _), % Check
    % Find one possible destination
    % The destination should be in one of those two inner and outer borders
    inner_border(Map, Province, InnerBorder),
    outer_border(Map, Province, OuterBorder),
    (member(Hex, InnerBorder); member(Hex, OuterBorder)),
    % The destination should not host any allied units or stronger enemy units
    hex_unit(Hex, UnitAtDest), % Get
    hex_owner(Hex, OwnerAtDest), % Get
    hex_building(Hex, BuildAtDest), % Get
    province_owner(Province, Player), % Get
    hex_coord(Hex, [X, Y]), % Get
    (   UnitAtDest == none
    ;   OwnerAtDest \= Player,
        stronger(UnitName, UnitAtDest)
    ;   % Condition to merge units
        OwnerAtDest == Player,
        unit(UnitAtDest, Strength2, _, _, _),
        unit_merge(Strength1, Strength2, _)
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



% unit_merge(+Strength1, +Strength2, -MergedUnit)
% Determines the merged unit based on the sum of the strengths of two units
unit_merge(Strength1, Strength2, MergedUnit) :-
   TotalStrength is Strength1 + Strength2,
    unit(MergedUnit, TotalStrength, _, _, _).
   


