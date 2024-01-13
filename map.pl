:- use_module(library(random)).
:- use_module(printer).
:- use_module(hex).
:- module(map, [matrix/3,
                generate_random_map/1,
                empty_map/1,
                map_size/1,
                inside_map/1,
                get_hex/2,
                check_tile/2,
                set_tile/2,
                set_owner/2,
                set_building/2,
                set_unit/2]).
% Map parameters
:- dynamic(map/1).
map_size(5).
smooth(2).
walkers(X):-map_size(MapSize), smooth(Smooth),X is MapSize /16 * Smooth.
walker_steps(X):-map_size(MapSize),smooth(Smooth),X is MapSize*8 / Smooth *99. % This makes a map without sea tiles

% Update the stored map
update_map(Map):-
    retractall(map(_)),
    assert(map(Map)).

% Generate a matrix Size x Size and fill it with Value
matrix(Size,Matrix, Value):-
    length(Matrix, Size),
    maplist(same_length(Matrix), Matrix),
    maplist(maplist(=(Value)), Matrix).

% Generates a random map
generate_random_map(Map) :-
    map_size(MapSize),
    walkers(Walkers),
    % Generate a map with only sea tiles
    empty_map(EmptyMap),
    update_map(EmptyMap),
    % Spawn a bunch of walkers to place terrain tiles
    MaxX is MapSize-1, MaxY = MaxX,
    random_walkers(EmptyMap, MaxX, MaxY,Walkers, Map),
    update_map(Map), !.

% Generates an empty map of the specified size ========================
empty_map(Map) :-
    map_size(MapSize),
    length(Map, MapSize),
    empty_rows(MapSize, Map,0),!.

% Generates an empty row of the specified size
empty_rows(_, [],_).
empty_rows(Size, [Row|Rest],Count) :-
    length(Row, Size),
    empty_hex(Row, Size, Count,0),
    NewCount is Count+1,
    empty_rows(Size, Rest,NewCount).

empty_hex([],_,_,_).
empty_hex([Hex|Rest], Size, RowCount,ColCount):-
    hex_tile(Hex, sea),
    Index is RowCount*Size + ColCount,
    Hex=hex(Index, [RowCount,ColCount],_,none,none,none),
    NewColCount is ColCount +1,
    empty_hex(Rest, Size, RowCount, NewColCount).

% Simulate a bunch of walkers walks ====================================
random_walkers(Map, _, _, Count, Map):-Count=<0.
random_walkers(Map, MaxX, MaxY, Count, ResultMap) :-
    (
        % If there is at least one terrain tile, choose one of them as the new walker spawn point
        findall([X, Y], (nth0(X, Map, Row), nth0(Y, Row, Hex), hex_tile(Hex,terrain)), TerrainHexes),
        random_member([X,Y],TerrainHexes)
        ;
        % Else, choose the center of the map as the new walker spawn point
        map_size(MapSize),
        X is round(MapSize/2), Y=X
    ),
    % Randomly calculate the new walker lifespan
    walker_steps(MaxWalkerSteps), MinWalkerSteps is MaxWalkerSteps /2,
    random_between(MinWalkerSteps,MaxWalkerSteps,Steps),
    % Spawn the new walker
    walk(Map, X,Y,Steps,NewMap),
    % Spawn the remaining walkers
    NewCount is Count - 1,
    random_walkers(NewMap, MaxX, MaxY,NewCount, ResultMap).

% Simulate a walker random walk
walk(Map, _, _, Count, Map):-Count=<0.
walk(Map, X, Y, Count, NewMap) :-
    findall(Terrain,terrain(Terrain), Terrains),
    random_member(Terrain,Terrains),
    % Change the sea hex to a desert hex at the walker location
    set_tile([X, Y], Terrain),
    map(UpdatedMap),
    % Choose a random direction and move along it
    random_move(X,Y,NewX,NewY),
    (
      % If the new position dwells within the map boundaries, continue the walk
      inside_map([NewX, NewY]),!,
      (
          % If the next step falls on a sea tile, decrease the walker lifespan
          check_tile([NewX,NewY],sea),
          NewCount is Count-1
          ;
          % Else if there is at least one sea tile, do not decrease the walker lifespan if
          sea_in_map(Map),
          NewCount=Count
          ;
          % Else, kill the walker
          NewCount = 0
      ),
      walk(UpdatedMap, NewX, NewY, NewCount, NewMap)
      ;
      % Else, choose another step direction
      walk(Map,X,Y,Count,NewMap)
    ).

% Move in one of the four directions (up, down, left, right)
move(X, Y, NewX, Y) :- NewX is X - 1; NewX is X + 1.
move(X, Y, X, NewY) :- NewY is Y - 1; NewY is Y + 1.
% Randomly choose one of the four directions
random_move(X,Y,NewX,NewY) :-
    findall([NewX,NewY],move(X,Y,NewX,NewY),Moves),
    random_member(Move,Moves),
    [NewX,NewY]=Move.

% Replace the Nth element of List with El ==============================
replace_nth(N, List, El, Result) :-
    nth0(N, List, _, Before),
    nth0(N, Result, El, Before).

% Returns the Hex at a given location on the map
get_hex([X, Y],Hex):-
    inside_map([X,Y]),
    map(Map),
    nth0(X, Map, Row),
    nth0(Y, Row, Hex).

% Checks or returns the tile in a given location
check_tile([X, Y],Tile):-
    get_hex([X,Y],Hex),
    hex_tile(Hex,Tile).

% Change an hex tile type
set_tile([X, Y], Tile) :-
    get_hex([X, Y], Hex),
    map(Map),
    nth0(X, Map, Row),
    change_hex_tile(Hex, Tile, NewHex),
    replace_nth(Y, Row, NewHex, NewRow),
    replace_nth(X, Map, NewRow, UpdatedMap),
    update_map(UpdatedMap).

% Change an hex owner
set_owner([X, Y], Owner) :-
    get_hex([X, Y], Hex),
    map(Map),
    nth0(X, Map, Row),
    change_hex_owner(Hex, Owner, NewHex),
    replace_nth(Y, Row, NewHex, NewRow),
    replace_nth(X, Map, NewRow, UpdatedMap),
    update_map(UpdatedMap).

% Change an hex building
set_building([X, Y], Building) :-
    get_hex([X, Y], Hex),
    map(Map),
    nth0(X, Map, Row),
    change_hex_building(Hex, Building, NewHex),
    replace_nth(Y, Row, NewHex, NewRow),
    replace_nth(X, Map, NewRow, UpdatedMap),
    update_map(UpdatedMap).

% Change an hex unit
set_unit([X, Y], Unit) :-
    map(Map),
    get_hex([X, Y], Hex),
    nth0(X, Map, Row),
    change_hex_unit(Hex, Unit, NewHex),
    replace_nth(Y, Row, NewHex, NewRow),
    replace_nth(X, Map, NewRow, UpdatedMap),
    update_map(UpdatedMap).

% Check if a location dwells within the map boundaries
inside_map([X, Y]) :-
    map_size(MapSize),
    X >= 0, X < MapSize,
    Y >= 0, Y < MapSize.

% Check if the map contains at least one sea tile
sea_in_map(Map):-
    member(Row,Map),
    member(Hex,Row),
    hex_tile(Hex,sea).