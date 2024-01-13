:- module(unit, [unit/5,
    move_unit/4]).
:- use_module([hex, province, building]).

% Unit enum ========================================
% unit(Name, Strength, Protection, Cost, Income)
unit(peasant,   1,          1,      10,   -2).
unit(spearman,  2,          2,      20,   -6).
unit(baron,     3,          3,      30,   -18).
unit(knight,    4,          3,      40,   -36).

% Determine whether one unit or one building can be destroyed by another unit
stronger(unit(_, Strength1, _, _, _), unit(_, _, Protection2, _, _)) :-
    Strength1>Protection2.
stronger(unit(_, Strength1, _, _, _), building(_, Protection2, _, _)) :-
    Strength1>Protection2.
    
% Moves ======================================================
% Checks whether a given unit can be moved to a certain hex
% This is useful to list all the possible displacement moves for a given unit
move_unit(Map, Province, FromHex, ToHex):-
    province_hexes(Province, ProvinceHexes), % Get
    member(FromHex, ProvinceHexes), % Check
    hex_unit(FromHex, Unit), % Get
    unit(Unit,_,_,_,_), % Check
    % Calculate the province inner and outer boundaries
    frontier(Map, Province, Frontier),
    province_boundary(Map, Province, Boundary),
    % The destination should be in one of those two boundaries
    (member(ToHex, Frontier); member(ToHex, Boundary)),
    % The destination should not host any allied units or stronger enemy units
    hex_unit(ToHex, UnitAtDest), % Get
    hex_owner(ToHex, OwnerAtDest), % Get
    province_owner(Province, Player), % Get
    (UnitAtDest==none; OwnerAtDest\=Player, stronger(Unit, UnitAtDest)),
    % The destination should not host a building
    hex_building(ToHex, BuildAtDest), % Get
    (BuildAtDest==none; OwnerAtDest\=Player, stronger(Unit, BuildAtDest)),
    % The destination should not be located near an enemy tower
    hex_coord(ToHex, [ToX, ToY]), % Get
    \+ tower_nearby(Map, [ToX, ToY], Player).

