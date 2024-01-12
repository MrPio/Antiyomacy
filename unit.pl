:- use_module("province").
:- module(unit, [unit/5,
    move_unit/4]).

% Unit enum ========================================
% unit(Name, Strength, Protection, Cost, Income)
unit(peasant, 1, 1, 10, -2).
unit(spearman, 2, 2, 20, -6).
unit(baron, 3, 3, 30, -18).
unit(knight, 4, 3, 40, -36).

% Moves ======================================================
% Check whether a given unit can be moved to a certain hex
% This is useful to list all the possible displacement moves for a given unit
move_unit(Map, Province, FromHex, ToHex):-
    province_hexes(Province, ProvinceHexes), % Get
    member(FromHex, ProvinceHexes), % Check
    hex_coord(FromHex,[FromX,FromY]), % Get
    hex_unit(FromHex, Unit), % Get
    unit(Unit,_,_,_,_), % Check
    % Calculate the province inner and outer boundaries
    frontier(Map, Province, Frontier),
    province_boundary(Map, Province, Boundary),
    % The destination should be in one of those two boundaries
    (member(ToHex, Frontier); member(ToHex, Boundary)),
    % The destination should not host another unit
    hex_unit(ToHex, none), % Check TODO: EXPAND
    % The destination should not host a building
    hex_building(ToHex, none), % Check TODO: EXPAND
    % The destination should not be located near an enemy tower
    province_owner(Province, Player), % Get
    hex_coord(ToHex, [ToX, ToY]), % Get
    \+ tower_nearby(Map, ToX, ToY, Player).

