:- module(map, [matrix/3,
                index2coord/2,
                generate_random_map/2,
                empty_map/1,
                map_size/1,
                map/1, last_board/1,
                update_map/1, update_last_board/1,
                inside_map/1,
                get_hex/3,
                check_tile/3,
                set_tile/2, set_tile/4,
                set_owner/2, set_owner/4,
                set_building/2, set_building/4,
                set_unit/2, set_unit/4,
                destroy_units/3,
                spawn_provinces/2,
                free_hexes/3,
                free_hex/3,
                has_won/3]).
:- use_module(library(random)).
:- use_module(library(clpfd)).
:- use_module([utils, hex, province]).

% Map parameters
:- dynamic([map/1, last_board/1, map_size/1]).

smooth(2).
walkers(X) :-       map_size(MapSize), smooth(Smooth), X is MapSize / 16 * Smooth.
walker_steps(X) :-  map_size(MapSize), smooth(Smooth), X is MapSize *  8 / Smooth.

% Convert index to coordinate and vice versa.
% Note: this can also be used as checker
% index2coord(?Index, ?Coord)
index2coord(Index, [X, Y]) :-
    map_size(MapSize),
    X #= Index // MapSize,
    Y #= Index mod MapSize,
    inside_map([X, Y]).

% Update the stored map
update_map(Map) :-
    retractall(map(_)),
    assertz(map(Map)).
% Update the stored board
update_last_board(Board) :-
    retractall(last_board(_)),
    assertz(last_board(Board)).

% Generate a matrix Size x Size and fill it with Value
matrix(Size, Matrix, Value) :-
    length(Matrix, Size),
    maplist(same_length(Matrix), Matrix),
    maplist(maplist(=(Value)), Matrix).

% Generates a random map
% generate_random_map(+Map, +Empty)
generate_random_map(Map, Empty) :-
    map_size(MapSize),
    walkers(Walkers),
    % Generate a map with only sea tiles
    empty_map(EmptyMap),
    update_map(EmptyMap),
    (   Empty
    ->  % Fill the map with terrain tiles
        MaxIndex is MapSize^2 -1,
        foreach((
            between(0, MaxIndex, Index),
            index2coord(Index, Coord)
        ),set_tile(Coord, desert)),
        map(Map)
    ;   % Spawn a bunch of walkers to place terrain tiles
        MaxX is MapSize-1, MaxY = MaxX,
        random_walkers(EmptyMap, [MaxX, MaxY],Walkers, Map),
        update_map(Map)
    ), !.

% Generates an empty map of the specified size ========================
empty_map(Map) :-
    map_size(MapSize),
    length(Map, MapSize),
    empty_rows(Map, MapSize, 0),!.

% Generates an empty row of the specified size
empty_rows([], _, _).
empty_rows([Row|Rest], Size, Count) :-
    length(Row, Size),
    empty_hex(Row, Size, Count, 0),
    NewCount is Count + 1,
    empty_rows(Rest, Size, NewCount).

% Generates an empty map's cell
empty_hex([],_,_,_).
empty_hex([Hex|Rest], Size, RowCount, ColCount) :-
    hex_tile(Hex, sea),
    Index is RowCount*Size + ColCount,
    Hex = hex(Index, [RowCount,ColCount],_,none,none,none),
    NewColCount is ColCount +1,
    empty_hex(Rest, Size, RowCount, NewColCount).

% Simulate a bunch of walkers walks ====================================
random_walkers(Map, _, Count, Map) :-Count=<0.
random_walkers(Map, [MaxX, MaxY], Count, ResultMap) :-
    (   % If there is at least one terrain tile, choose one of them as the new walker spawn point
        findall([X, Y], (nth0(X, Map, Row), nth0(Y, Row, Hex), hex_tile(Hex,terrain)), TerrainHexes),
        random_member([X,Y],TerrainHexes)
    ;   % Else, choose the center of the map as the new walker spawn point
        map_size(MapSize),
        X is round(MapSize/2), Y = X
    ),
    % Randomly calculate the new walker lifespan
    walker_steps(MaxWalkerSteps), MinWalkerSteps is MaxWalkerSteps /2,
    random_between(MinWalkerSteps,MaxWalkerSteps,Steps),
    % Spawn the new walker
    % Note: the yall library is used here to define a lambda expression
    StepAction = [ActionMap, ActionCoord, ActionNewMap] >>
        (   % Place a random terrain tile on the walker location
            findall(Terrain, terrain(Terrain), Terrains),
            random_member(Terrain, Terrains),
            % Change the sea hex to a desert hex at the walker location
            set_tile(ActionMap, ActionCoord, Terrain, ActionNewMap)
        ),
    WalkableCoordCondition = [ActionMap, ActionCoord] >> check_tile(ActionMap, ActionCoord, sea),
    walk(Map, [X,Y], StepAction, WalkableCoordCondition, Steps, NewMap),
    % Spawn the remaining walkers
    NewCount is Count - 1,
    random_walkers(NewMap, [MaxX, MaxY],NewCount, ResultMap).

% Simulate a walker random walk
% walk(+Map, +Coord, :StepAction, :WalkableCoordCondition, +Count, -NewMap)
walk(Map, _, _, _, Count, Map) :- Count =< 0.
walk(Map, [X, Y], StepAction, WalkableCoordCondition, Count, NewMap) :-
    % Invoke the action on the current coordinate
    call(StepAction, Map, [X, Y], UpdatedMap),
    % Choose a random direction and move along it
    random_move([X,Y],[NewX,NewY]),
    (   % If the new position dwells within the map boundaries, continue the walk
        inside_map([NewX, NewY]),
        (   call(WalkableCoordCondition, UpdatedMap, [NewX,NewY])
        ->  % If the next step falls on a walkable tile, decrease the walker lifespan
            NewCount is Count - 1
        ;   sea_in_map(Map)
        ->  % Else if there is at least one sea tile, do not decrease the walker lifespan
            NewCount=Count
        ;   % Else, kill the walker
            NewCount = 0
        ),
        walk(UpdatedMap, [NewX, NewY],StepAction, WalkableCoordCondition, NewCount, NewMap)
    ;   % Else, choose another step direction
        walk(Map,[X,Y],StepAction, WalkableCoordCondition, Count,NewMap)
    ).

% Move in one of the four directions (up, down, left, right)
walker_move([X, Y], [NewX, Y]) :- NewX is X - 1; NewX is X + 1.
walker_move([X, Y], [X, NewY]) :- NewY is Y - 1; NewY is Y + 1.
% Randomly choose one of the four directions
random_move([X, Y], [NewX, NewY]) :-
    findall([NewX,NewY],walker_move([X,Y],[NewX,NewY]),Moves),
    random_member(Move, Moves),
    [NewX,NewY]=Move.

% Replace the Nth element of List with El ==============================
replace_nth(N, List, El, Result) :-
    nth0(N, List, _, Before),
    nth0(N, Result, El, Before).

% Returns the Hex at a given coordinate or index on the map
% Time: ~ 3 micro
% get_hex(+Map, ?Coord, ?Hex)
get_hex(Map, [X, Y], Hex) :-
    !,
    nth0(X, Map, Row),
    nth0(Y, Row, Hex).
get_hex(Map, Index, Hex) :-
    index2coord(Index,[X,Y]), % Get [X,Y]
    nth0(X, Map, Row),
    nth0(Y, Row, Hex).

% Checks or returns the tile in a given location
check_tile(Map, [X, Y], Tile) :-
    get_hex(Map, [X, Y],Hex),
    hex_tile(Hex,Tile).

% Change an hex tile type
set_tile([X, Y], Tile) :-
    map(Map),
    set_tile(Map,[X, Y], Tile, UpdatedMap),
    update_map(UpdatedMap).
set_tile(Map, [X, Y], Tile, UpdatedMap) :-
    get_hex(Map, [X, Y], Hex),
    nth0(X, Map, Row),
    change_hex_tile(Hex, Tile, NewHex),
    replace_nth(Y, Row, NewHex, NewRow),
    replace_nth(X, Map, NewRow, UpdatedMap).

% Change an hex owner
set_owner([X, Y], Owner) :-
    map(Map),
    set_owner(Map, [X, Y], Owner, UpdatedMap),
    update_map(UpdatedMap).
set_owner(Map, [X, Y], Owner, UpdatedMap) :-
    get_hex(Map, [X, Y], Hex),
    nth0(X, Map, Row),
    change_hex_owner(Hex, Owner, NewHex),
    replace_nth(Y, Row, NewHex, NewRow),
    replace_nth(X, Map, NewRow, UpdatedMap).

% Change an hex building
set_building([X, Y], Building) :-
    map(Map),
    set_building(Map, [X, Y], Building, UpdatedMap),
    update_map(UpdatedMap).
set_building(Map, [X, Y], Building, UpdatedMap) :-
    get_hex(Map, [X, Y], Hex),
    nth0(X, Map, Row),
    change_hex_building(Hex, Building, NewHex),
    replace_nth(Y, Row, NewHex, NewRow),
    replace_nth(X, Map, NewRow, UpdatedMap).

% Change an hex unit
set_unit([X, Y], Unit) :-
    map(Map),
    set_unit(Map, [X, Y], Unit, UpdatedMap),
    update_map(UpdatedMap).
set_unit(Map, [X, Y], Unit, UpdatedMap) :-
    get_hex(Map, [X, Y], Hex),
    nth0(X, Map, Row),
    change_hex_unit(Hex, Unit, NewHex),
    replace_nth(Y, Row, NewHex, NewRow),
    replace_nth(X, Map, NewRow, UpdatedMap).

% Check if a location dwells within the map boundaries
inside_map([X, Y]) :-
    map_size(MapSize),
    X >= 0, X < MapSize,
    Y >= 0, Y < MapSize.

% Check if the map contains at least one sea tile
sea_in_map(Map) :-
    member(Row,Map),
    member(Hex,Row),
    hex_tile(Hex,sea).

% Destroy all units located on the specified hexes on the map
% This is useful for handling bankruptcy cases
% destroy_all_units(+Map, +Hexes, -NewMap)
destroy_units(Map, [], Map).
destroy_units(Map, [Hex|Tail], NewMap) :-
    hex_coord(Hex, Coord),
    set_unit(Map, Coord, none, PartialNewMap),
    destroy_units(PartialNewMap, Tail, NewMap).

% Randomly spawns a red and a blue province
% spawn_provinces(+Map, -NewMap)
spawn_provinces(Map, NewMap):-
    map_size(MapSize),
    % Select all the coords of hexes with terrain on the map
    findall([X, Y],
    (   get_hex(Map, [X, Y], Hex),
        hex_tile(Hex, terrain)
    ), CoordsWithTerrain),
    % Select the first and the last hexes as a spawn point for the two provinces
    nth0(0, CoordsWithTerrain, [RedX,RedY]),
    last(CoordsWithTerrain, [BlueX,BlueY]),
    SecondProvinceRed is MapSize -2,
    length(CoordsWithTerrain, CoordsWithTerrainLength),
    SecondProvinceBlue is CoordsWithTerrainLength - SecondProvinceRed,
    nth0(SecondProvinceRed, CoordsWithTerrain, [RedX2,RedY2]),
    nth0(SecondProvinceBlue, CoordsWithTerrain, [BlueX2,BlueY2]),
    % Define the Random Walker action to be invoked at each step
    % Note: the yall library is used here to define a lambda expression
    StepAction = [Player, Action] >>
    (   Action = [ActionMap, ActionCoord, ActionNewMap] >>
        (   % Check if the walker is on a terrain hex
            get_hex(ActionMap, ActionCoord, Hex),
            hex_tile(Hex, terrain), % Check
            % Check if the hex is free or already owned by the player
            % Note: the second case is necessary to avoid cycling in a dead end
            (hex_owner(Hex, Player), ! ; hex_owner(Hex, none)), % Check
            % Change the owner to red player
            set_owner(ActionMap, ActionCoord, Player, ActionNewMap)
        )
    ),
    % Define the condition for a walkable hex
    WalkableCoordCondition = [ActionMap, ActionCoord] >>
        (   check_tile(ActionMap, ActionCoord, terrain),
            get_hex(ActionMap, ActionCoord, Hex),
            hex_owner(Hex, none) % Check
        ),
    % Use the Random Walker algorithm to spawn the red province
    call(StepAction, red, RedStepAction),
    random_between(2, 4, RedSteps), random_between(2, 4, RedSteps2),
    walk(Map, [RedX,RedY], RedStepAction, WalkableCoordCondition, RedSteps, NewMapWithRed),
    % Use the Random Walker algorithm to spawn the blue province
    call(StepAction, blue, BlueStepAction),
    random_between(2, 4, BlueSteps), random_between(2, 4, BlueSteps2),
    walk(NewMapWithRed, [BlueX,BlueY], BlueStepAction, WalkableCoordCondition, BlueSteps, NewMap1),

    (   MapSize > 8
    ->  walk(NewMap1, [RedX2,RedY2], RedStepAction, WalkableCoordCondition, RedSteps2, NewMap2),
        walk(NewMap2, [BlueX2,BlueY2], BlueStepAction, WalkableCoordCondition, BlueSteps2, NewMap)
    ;   NewMap = NewMap1
    ),

    update_map(NewMap),!.

% Sets the specified hexes unit, building and owner in the hex list to none
% free_hexes(+Map, +HexList, -UpdatedMap)
free_hexes(Map, [], Map).
free_hexes(Map, [Hex | T], UpdatedMap) :-
    free_hex(Map, Hex, UpdatedMap1),
    free_hexes(UpdatedMap1, T, UpdatedMap).
% Set a single hex to empty
% free_hex(+Map, +Hex, -UpdatedMap)
free_hex(Map, Hex, UpdatedMap) :-
    hex_coord(Hex, Coord), % Get
    set_owner(Map, Coord, none, MapWithNoneOwner),
    set_building(MapWithNoneOwner, Coord, none, MapWithNoneBuilding),
    set_unit(MapWithNoneBuilding, Coord, none, UpdatedMap).
    

% Checks if the player has won. (❔semi-deterministic❔)
% Note: a player wins if they own at least 80% of the terrain map hexes or there are no more enemy provinces
% has_won(+Map, +Provinces, +Player)
has_won(Map, Provinces, Player) :-
    % Count the terrain hexes on the map
    findall(Coord, (check_tile(Map, Coord, terrain)), NonSeaHexes),
    length(NonSeaHexes, TotalHexes),

    % Calculate the total size of the player's provinces
    include([In]>>(province_owner(In, Player)), Provinces, ProvincesOfPlayer),
    maplist(province_size, ProvincesOfPlayer, PlayerSizes),
    sum_list(PlayerSizes, PlayerTotalSize),

    % Calculate the percentage of player's owned terrain hexes
    Percentage is (PlayerTotalSize / TotalHexes) * 100,

    % Check if the percentage is at least 80% or there are no more provinces owned by the other player
    (   Percentage >= 80, !
    ;   % Check if there are no more enemy provinces
        \+ (member(Province, Provinces), \+ province_owner(Province, Player))
    ).