:- module(unit, [unit/5,
    unit_placement/5,
    unit_mergeable/3,
    max_displacement_distance/1]).
:- use_module([hex, province, building, utils]).

% Unit enum =================================================
% unit(Name, Strength, Protection, Cost, Income)
unit(peasant,   1,          1,      10,   -2).
unit(spearman,  2,          2,      20,   -6).
unit(baron,     3,          3,      30,   -18).
unit(knight,    4,          3,      40,   -36).

max_displacement_distance(2).
% Determine whether one unit or one building can be destroyed by another unit
% Note: If any of the two unit names is none, it fails
stronger(UnitName1, UnitName2) :-
    unit(UnitName1, Strength1, _, _, _), unit(UnitName2, _, Protection2, _, _),
    Strength1>Protection2.
stronger(UnitName, BuildingName) :-
    unit(UnitName, Strength1, _, _, _), building(BuildingName, Protection2, _, _),
    Strength1>Protection2.

% Moves ======================================================
% Checks/Gets a unit valid location on the given province
% This is useful to list all the possible displacement moves for a given unit
% Note: It is assumed that a unit can only be placed on the inner or outer
%       border of a province, not within it.
% Note: the FromHex hex is not a valid destination as the unit resides within it
%       But in case of merge, it can happen for a unit to merge with itself, so 
%       the caller should check that the destination differs from the original position
%       in the case of a displace action
% Note: this predicate only checks the validity of the placement, it does not
%       edit the map in any way.
% unit_placement(+Map, +Province, +UnitName, ?Hex, -NewUnitName) (❓non-deterministic❓)
unit_placement(Map, Province, UnitName, Hex, NewUnitName) :-
    hex_unit(Hex, UnitAtDestName), % Get
    hex_owner(Hex, OwnerAtDest), % Get
    hex_building(Hex, BuildAtDest), % Get
    province_owner(Province, Player), % Get
    hex_coord(Hex, [X, Y]), % Get
    unit(UnitName, UnitStrength, _, _, _), % Check & Get
    % The destination should be in one of those two inner and outer borders: (Geography)
    inner_border(Map, Province, InnerBorder),
    outer_border(Map, Province, OuterBorder),!,

    (member(Hex, InnerBorder); member(Hex, OuterBorder)),

    % The destination should not host any allied units or stronger enemy units: (Invade; Kill; Merge)
    (   % The destination does not host any units (Invade)
        UnitAtDestName == none,
        NewUnitName = UnitName
    ;   % The destination hosts an enemy weaker unit (Kill)
        OwnerAtDest \= Player,
        stronger(UnitName, UnitAtDestName),
        NewUnitName = UnitName
    ;   % The destination hosts a mergeable allied unit (Merge)
        OwnerAtDest == Player,
        unit(UnitAtDestName, UnitAtDestStrength, _, _, _),
        unit_mergeable(UnitStrength, UnitAtDestStrength, NewUnitName)
    ),
    % The destination should not host a non-destroyable building:
    (   % The destination does not host any buildings (Invade)
        BuildAtDest == none
    ;   % The destination hosts an enemy destroyable building (Kill)
        OwnerAtDest \= Player,
        stronger(UnitName, BuildAtDest)
    ),
    % Depending on the unit type, the destination should not be located
    % near an enemy tower or strong_tower:
    (   \+ member(UnitName, [baron, knight]),
        \+ tower_nearby(Map, [X, Y], Player),
        \+ strong_tower_nearby(Map, [X, Y], Player)
    ;   UnitName == baron,
        \+ strong_tower_nearby(Map, [X, Y], Player)
    ;   UnitName == knight
    ).


% Determines the merged unit based on the sum of the strengths of two units
% unit_mergeable(+Strength1, +Strength2, -MergedUnit)
unit_mergeable(Strength1, Strength2, MergedUnit) :-
   TotalStrength is Strength1 + Strength2,
    unit(MergedUnit, TotalStrength, _, _, _).
   


