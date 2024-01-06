:- use_module(library(random)).
:- use_module(printer).

map_size(16).
smooth(2).
sea(0).
desert(1).
wood(2).
walkers(X):-map_size(MapSize), smooth(Smooth),X is MapSize /16 * Smooth.
walker_steps(X):-map_size(MapSize),smooth(Smooth),X is MapSize*8 / Smooth.

% Generates a random map
generate_random_map(Map) :-
    map_size(MapSize),
    walkers(Walkers),
    % Generate a map with only sea tiles
    empty_map(MapSize, EmptyMap),
    % Spawn a bunch of walkers to place terrain tiles
    MaxX is MapSize-1, MaxY = MaxX,
    random_walkers(EmptyMap, MaxX, MaxY,Walkers, Map),
    % Print the map
    print_map(Map),!.

% Generates an empty map of the specified size
empty_map(Size, Map) :-
    length(Map, Size),
    empty_rows(Size, Map),!.

% Generates an empty row of the specified size
empty_rows(_, []).
empty_rows(Size, [Row|Rest]) :-
    length(Row, Size),
    maplist(sea, Row),
    empty_rows(Size, Rest).

% Replace the Nth element of List with El
replace_nth(N, List, El, Result) :-
    nth0(N, List, _, Before),
    nth0(N, Result, El, Before).

% Checks or returns the tile in a given location
check_tile(Map,X,Y,Type):-
    nth0(X, Map, Row),
    nth0(Y, Row, Type).

% Simulate a bunch of walkers walks
random_walkers(Map, _, _, Count, Map):-Count=<0.
random_walkers(Map, MaxX, MaxY, Count, ResultMap) :-
    desert(Desert),
    (
        % If there is at least one terrain tile, choose one of them as the new walker spawn point
        findall([X, Y], (nth0(X, Map, Row), nth0(Y, Row, Desert)), DesertTiles),
        random_member([X,Y],DesertTiles)
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
    desert(Desert),
    sea(Sea),
    % Add a desert at the walker location
    set_tile(Map, X, Y, Desert, UpdatedMap),
    % Choose a random direction and move along it
    random_move(X,Y,NewX,NewY),
    (
      % If the new position dwells within the map boundaries, continue the walk
      inside_map(NewX, NewY),!,
      (
          % If the next step falls on a sea tile, decrease the walker lifespan
          check_tile(Map,NewX,NewY,Sea),
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

% Set the tile at Row, Col to Value in the map
set_tile(Map, X, Y, Value, UpdatedMap) :-
    nth0(X, Map, Row),
    replace_nth(Y, Row, Value, NewRow),
    replace_nth(X, Map, NewRow, UpdatedMap).

% Check if a location dwells within the map boundaries
inside_map(X, Y) :-
    map_size(MapSize),
    X >= 0, X < MapSize,
    Y >= 0, Y < MapSize.

% Check if the map contains at least one sea tile
sea_in_map(Map):-
    sea(Sea),
    member(Row,Map),
    member(Sea,Row).

% Move in one of the four directions (up, down, left, right)
move(X, Y, NewX, Y) :- NewX is X - 1; NewX is X + 1.
move(X, Y, X, NewY) :- NewY is Y - 1; NewY is Y + 1.

% Randomly choose one of the four directions
random_move(X,Y,NewX,NewY) :-
    findall([NewX,NewY],move(X,Y,NewX,NewY),Moves),
    random_member(Move,Moves),
    [NewX,NewY]=Move.
