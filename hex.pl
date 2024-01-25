:- module(hex, [tile/1,
                sea/1,
                terrain/1,
                owner/1,
                hex/6,
                hex_index/2,
                hex_coord/2,
                hex_tile/2,
                change_hex_tile/3,
                hex_owner/2,
                change_hex_owner/3,
                hex_building/2,
                change_hex_building/3,
                hex_unit/2,
                change_hex_unit/3,
                manhattan_distance/3,
                hexes_adjacent4/2,
                hexes_adjacent8/2]).
:- use_module([unit, building]).

% Tile enum
tile(X) :-sea(X); terrain(X).
sea(ocean).
terrain(X) :- member(X,[desert, wood, rock, sheep, clay, hay]).

% Owners enum
owner(X) :-member(X,[none, red, blue]).

% Hex struct ====================================================================
hex(Index,[X,Y], Tile, Owner, Building, Unit) :- 
    number(Index),
    X>=0,Y>=0,
    tile(Tile),
    owner(Owner),
    \+ (building(Building,_,_,_), unit(Unit,_,_,_,_)).

% Check/Get hex index
hex_index(hex(Index,_,_,_,_,_),Index).

% Check/Get hex coordinates
hex_coord(hex(_,Coord,_,_,_,_), Coord).

% Check/Get hex tile type
% Note: Checker is expected to be a functor, so it is called with the meta-predicate call/2
hex_tile(hex(_,_,Tile,_,_,_), Checker) :- call(Checker,Tile).
% Change an hex tile type
change_hex_tile(hex(Index,Coord,_,Owner,Building,Unit),NewTile,hex(Index,Coord,NewTile,Owner,Building,Unit)).

% Check/Get hex owner
hex_owner(hex(_,_, _, Owner,_,_), Owner).
% Change an hex owner
change_hex_owner(hex(Index,Coord,Tile,_,Building,Unit),NewOwner,hex(Index,Coord,Tile,NewOwner,Building,Unit)).

% Check/Get hex building
hex_building(hex(_,_, _, _,Building,_), Building).
% Change an hex building
change_hex_building(hex(Index,Coord,Tile,Owner,_,Unit),NewBuilding,hex(Index,Coord,Tile,Owner,NewBuilding,Unit)).

% Check/Get hex unit
hex_unit(hex(_,_, _, _,_,Unit), Unit).
% Change an hex unit
change_hex_unit(hex(Index,Coord,Tile,Owner,Building,_),NewUnit,hex(Index,Coord,Tile,Owner,Building,NewUnit)).

% Checks if one hex is in the near8 of another hex
% hexes_adjacent4(+Hex1, +Hex2)
hexes_adjacent4(Hex1, Hex2):-
    manhattan_distance(Hex1, Hex2, 1).

% Checks if one hex is in the near4 of another hex
% hexes_adjacent8(+Hex1, +Hex2)
hexes_adjacent8(Hex1, Hex2):-
    hex_coord(Hex1, [X1, Y1]), % Get
    hex_coord(Hex2, [X2, Y2]), % Get
    Width is X2 - X1,
    Height is Y2 - Y1,
    between(-1, 1, Width),
    between(-1, 1, Height).
    

% Calculate the Manhattan distance between two hexes
% manhattan_distance(+Hex1, +Hex2, -Distance)
manhattan_distance(Hex1, Hex2, Distance) :-
    hex_coord(Hex1, [X1, Y1]), % Get
    hex_coord(Hex2, [X2, Y2]), % Get
    Distance is abs(X2 - X1) + abs(Y2 - Y1).