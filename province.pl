:- use_module(printer).
:- use_module(hex).
:- use_module(map).
:- module(province, [boundary24/4,
                     boundary8/4,
                     boundary4/4,
                     find_provinces/2,
                     find_province/3,
                     province_boundary/3,
                     frontier/3]).

% Province struct ===================================================
province(Owner, Hexes, Money):- 
    owner(Owner), 
    maplist(=(hex(_,_,_,_,_,_)),Hexes),
    integer(Money).
% Change a provinve money amount
change_province_money(province(Owner, Hexes, _), Money, province(Owner, Hexes, Money)).

% Add an income to the province money
province_apply_income(province(Owner,Hexes,Money),Income,province(Owner,Hexes,NewMoney)):-
    NewMoney is Money + Income.

% Search for hexes around the hexes adjacent to the given one
boundary24(Map,X,Y,Boundary):-
    inside_map(X,Y),
    findall(Hex, (
                Left is X-2, Right is X+2, Down is Y-2, Up is Y+2,
                between(Left, Right, X1), between(Down, Up, Y1),
                \+ (X1 = X, Y1 = Y),
                inside_map(X1,Y1),
                nth0(X1, Map, Row),
                nth0(Y1, Row, Hex)
            ), Boundary).

% Search for adjacent hexes around the given one
boundary8(Map,X,Y,Boundary):-
    inside_map(X,Y),
    findall(Hex, (
                Left is X-1, Right is X+1, Down is Y-1, Up is Y+1,
                between(Left, Right, X1), between(Down, Up, Y1),
                \+ (X1 = X, Y1 = Y),
                inside_map(X1,Y1),
                nth0(X1, Map, Row),
                nth0(Y1, Row, Hex)
            ), Boundary).

% Search for orthogonally adjacent hexes around the given one
boundary4(Map,X,Y,Boundary):-
    inside_map(X,Y),
    findall(Hex, (
                Left is X-1, Right is X+1, Down is Y-1, Up is Y+1,
                member([X1,Y1],[[Left,Y],[Right,Y],[X,Down],[X,Up]]),
                inside_map(X1,Y1),
                nth0(X1, Map, Row),
                nth0(Y1, Row, Hex)
            ), Boundary).

% Retrieve the frontier from a given province
frontier(Map, province(_, Hexes, _), Frontier):-   
    findall(Hex,(
        % For each hex in the province...
        member(Hex, Hexes),
        % ...checks that in the midst of its boundary8...
        hex_coord(Hex, [X, Y]),
        boundary8(Map, X, Y, Boundary),
        % ... at least one hex is outside the province
        \+ maplist(member,Boundary,Hexes)
    ), Frontier).
% Caller predicate for find_provinces_
find_provinces(Map,Provinces):-find_provinces_(Map,0,[],Provinces),!.
% Find all the provinces in the map
find_provinces_(_,Index,Provinces,Provinces):-
    % If all the map has been scanned, break the recursion
    map_size(MapSize), Index is MapSize^2.
find_provinces_(Map,Index,Found,Provinces):-
    map_size(MapSize),
    % Calculate the coordinates from the index
    X is truncate(Index/MapSize),
    Y is Index mod MapSize,
    (
      % Check if the current hex belongs to one of the previously found provinces,
      % if that's the case, do not search again for a province around it.
      \+ (member(province(_,Hexes,_),Found), member(Hex,Hexes), hex_coord(Hex,[X,Y])),
      % Search for a province around the hex and append to Found only if it isn't empty
      find_province(Map,[X,Y], Province),
      append(Found, [Province], NewFound)
    ;
      NewFound=Found
    ),
    % Increment the index and continue the search
    NewIndex is Index+1,
    find_provinces_(Map,NewIndex,NewFound,Provinces).

% Find a single province by searching around the given coordinates
find_province(Map, [X,Y], Province) :-
    % Check that the hex has an owner and retrieve it
    hex_owned(Map,X,Y,Owner, Hex),
    % Find all the connected hexes with a breadth first search
    province_bfs(Map,Owner,[Hex],[],Hexes),
    Hexes \= [],
    Province=province(Owner,Hexes,0),!.

% Find a province from a start Hex using Breadth-first-like search
province_bfs(_,_,[],Hexes,Hexes):-!.
province_bfs(Map, Owner, [Hex|Tail], Visited,Hexes) :-
    hex_owner(Hex,Owner),
    hex_coord(Hex,[X,Y]),
    % Scan the neighbor hexes
    % note: use boundary8/4 if the map is made of hexagons
    % else use boundary4/4 if the map is made of squares
    boundary4(Map, X, Y, NeighborHexes),
    % Filter only the valid neighbor hexes
    findall(NeighborHex,(
                % Pick one neighbor hex to validate
                member(NeighborHex, NeighborHexes),
                % To be part of the province, the hex needs to be owned by the same owner
                hex_owner(NeighborHex, Owner),
                % Check that the hex has no already been visited
                \+ member(NeighborHex, Visited)
            ),
            ValidNeighborHexes),
    % All the valid neighbour hexes need to be expanded further
    append(Tail,ValidNeighborHexes,ToVisit),
    province_bfs(Map,Owner,ToVisit, [Hex|Visited],Hexes).

% Checks if an hex is a terrain and has an owner
hex_owned(Map, X, Y, Owner,Hex):-
    get_hex(Map,X,Y,Hex),
    hex_tile(Hex, terrain),
    hex_owner(Hex,Owner),
    \+ Owner = none.


% Caller predicate for province_boundary_
province_boundary(Map,province(_,Hexes,_), Boundary):-province_boundary_(Map,Hexes,Hexes,[],Boundary).
% Find all hexes bordering the given province
province_boundary_(_,[],_,Boundary,Boundary).
province_boundary_(Map,[Hex|RestHexes],ProvinceHexes,Found,Boundary):-
    hex_coord(Hex,[X,Y]),
    % Scan the neighbor hexes
    boundary8(Map,X,Y,NeighborHexes),
    % Filter only the valid neighbor hexes
    findall(NeighborHex,(
                % Pick one neighbor hex to validate
                member(NeighborHex, NeighborHexes),
                % Check if the hex is not a sea, is not inside the Province and
                % has not already been taken
                hex_tile(NeighborHex, terrain),
                \+ member(NeighborHex, ProvinceHexes),
                \+ member(NeighborHex, Found)
            ),
            ValidNeighborHexes),
    append(Found, ValidNeighborHexes, NewFound),
    province_boundary_(Map,RestHexes,ProvinceHexes,NewFound,Boundary).
