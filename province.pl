:- module(province, [province/3,
                     province_owner/2,
                     change_province_owner/3,
                     province_hexes/2,
                     change_province_hexes/3,
                     province_money/2,
                     change_province_money/3,
                     province_size/2,
                     province_count/3,
                     update_province/3,
                     apply_income/4,
                     near24/3,
                     near8/3,
                     near4/3,
                     find_provinces/2,
                     find_provinces/3,
                     find_province/3,
                     outer_border/3,
                     inner_border/3,
                     buy_and_place/8,
                     displace_unit/8]).
:- use_module([utils, map, hex, unit, building, economy]).

% Province struct ===================================================
province(Owner, Hexes, Money) :-
    owner(Owner),
    maplist(=(hex(_,_,_,_,_,_)),Hexes),
    integer(Money).

% Check/Get province owner
province_owner(province(Owner, _, _), Owner).
% Change a province owner
change_province_owner(province(_, Hexes, Money), Owner, province(Owner, Hexes, Money)).

% Check/Get province hexes
province_hexes(province(_, Hexes, _), Hexes).
% Change a province hexes
change_province_hexes(province(Owner, _, Money), Hexes, province(Owner, Hexes, Money)).

% Check/Get province money
province_money(province(_, _, Money), Money).
% Change a province money amount
change_province_money(province(Owner, Hexes, _), Money, province(Owner, Hexes, Money)).

% Check/Get province size
province_size(Province, Size):-
    province_hexes(Province, Hexes),
    length(Hexes, Size).

% Checks or calculates the number of buildings or units owned by the province
% province_count(+Province, +ResourceName, ?Count)
province_count(Province, ResourceName, Count) :-
    province_hexes(Province, Hexes),
    % Retrieve all the hexes with the specified resource
    findall(Hex, (
        member(Hex, Hexes),
        (hex_building(Hex, ResourceName); hex_unit(Hex, ResourceName))
    ), HexesWithRes),
    length(HexesWithRes, Count).

% Reload all the hexes in the old province hex list, remove conquered hexes
% from the enemy and add the hexes conquered by the player
% Note: The the old province money is preserved
% update_province(+Map, +OldProvince, -UpdatedProvince)
update_province(Map, province(Owner, OldHexes, Money), UpdatedProvince) :-
    % Select one hex from the old province
    member(OldHex, OldHexes),
    % Check that the hex has not been conquered by the enemy
    hex_coord(OldHex, [X,Y]), % Get
    get_hex(Map, [X,Y], NewHex), % Get
    hex_owner(NewHex, Owner), % Check
    % Look for new province hexes
    find_province(Map, [X,Y], province(_,NewHexes,_)),
    UpdatedProvince=province(Owner,NewHexes,Money),!.

% Add the income to the province money and go bankrupt if necessary
% Note: this should be called at the end of each province turn
% apply_income(+Map, +Province, -NewMap, -NewProvince)
apply_income(Map, Province, NewMap, NewProvince) :-
    province_hexes(Province, Hexes), % Get
    province_money(Province, Money), % Get
    % Calculate the new money of the province
    get_income(Province, Income),
    NewMoney is Money + Income,
    % If the new money are negative...
    (   NewMoney < 0
        % ...go bankrupt by killing all units and setting the money
        % to 0, regardless of the new income
    ->  destroy_units(Map, Hexes, NewMap),
        change_province_money(Province, 0, ProvinceWithMoney),
        update_province(NewMap, ProvinceWithMoney, NewProvince)
    ;   change_province_money(Province, NewMoney, NewProvince),
        NewMap = Map
    ).

% Search for hexes around the hexes adjacent to the given one
near24(Map, [X,Y], NearHexes) :-
    inside_map([X,Y]),
    findall(Hex, (
                Left is X-2, Right is X+2, Down is Y-2, Up is Y+2,
                between(Left, Right, X1), between(Down, Up, Y1),
                \+ (X1 == X, Y1 == Y),
                inside_map([X1,Y1]),
                nth0(X1, Map, Row),
                nth0(Y1, Row, Hex)
            ), NearHexes).

% Search for adjacent hexes around the given one
near8(Map, [X,Y], NearHexes) :-
    inside_map([X,Y]),
    findall(Hex, (
                Left is X-1, Right is X+1, Down is Y-1, Up is Y+1,
                between(Left, Right, X1), between(Down, Up, Y1),
                \+ (X1 == X, Y1 == Y),
                inside_map([X1,Y1]),
                nth0(X1, Map, Row),
                nth0(Y1, Row, Hex)
            ), NearHexes).

% Search for orthogonally adjacent hexes around the given one
near4(Map, [X,Y], NearHexes) :-
    inside_map([X,Y]),
    findall(Hex, (
                Left is X-1, Right is X+1, Down is Y-1, Up is Y+1,
                member([X1,Y1],[[Left,Y],[Right,Y],[X,Down],[X,Up]]),
                inside_map([X1,Y1]),
                nth0(X1, Map, Row),
                nth0(Y1, Row, Hex)
            ), NearHexes).

% Caller predicates for find_provinces_in_map_ and find_provinces_in_hexes_
% find_provinces(+Map, -Provinces)
find_provinces(Map, Provinces) :- find_provinces_in_map_(Map,0,[],Provinces),!.
find_provinces(Map, Hexes, Provinces) :- find_provinces_in_hexes_(Map,Hexes,[],Provinces),!.
% Find all the provinces in the map
find_provinces_in_map_(_,Index,Provinces,Provinces) :-
    % If all the map has been scanned, break the recursion
    map_size(MapSize), Index is MapSize^2.
find_provinces_in_map_(Map,Index,Found,Provinces) :-
    % Calculate the coordinates from the index
    index2coord(Index, [X,Y]),
    (   % Check if the current hex belongs to one of the previously found provinces,
        % if that's the case, do not search again for a province around it.
        \+ (member(Province, Found),
            province_hexes(Province, Hexes),
            member(Hex, Hexes),
            hex_coord(Hex, [X,Y])),
        % Search for a province around the hex and append to Found only if it isn't empty
        find_province(Map,[X,Y], Province),
        append(Found, [Province], NewFound)
    ;
      NewFound=Found
    ),
    % Increment the index and continue the search
    NewIndex is Index+1,
    find_provinces_in_map_(Map,NewIndex,NewFound,Provinces).

% Find all the provinces in the map
find_provinces_in_hexes_(_,[],Provinces,Provinces).
find_provinces_in_hexes_(Map,[Hex|Tail],Found,Provinces) :-
    % Calculate the coordinates from the index
    hex_coord(Hex, [X,Y]),
    (   % Check if the current hex belongs to one of the previously found provinces,
        % if that's the case, do not search again for a province around it.
        \+ (member(Province, Found),
            province_hexes(Province, Hexes),
            member(Hex, Hexes)),
        % Search for a province around the hex and append to Found only if it isn't empty
        find_province(Map,[X,Y], Province),
        append(Found, [Province], NewFound)
    ;   NewFound=Found
    ),
    % Continue the search
    find_provinces_in_hexes_(Map,Tail,NewFound,Provinces).

% Find a single province by searching around the given coordinates
% find_province(+Map, +Coord, -Province)
find_province(Map, [X,Y], Province) :-
    % Check that the hex has an owner and retrieve it
    hex_owned(Map, [X,Y], Owner, Hex),
    % Find all the connected hexes with a breadth first search
    province_bfs(Map,Owner,[Hex],[],Hexes),
    Hexes \= [],
    Province=province(Owner,Hexes,0),!.

% Find a province from a start hex using Breadth-first-like search
province_bfs(_,_,[],Hexes,Hexes) :-!.
province_bfs(Map, Owner, [Hex|Tail], Visited, Hexes) :-
    % Stop this branch if the hex is not owned by the player
    hex_owner(Hex, Owner), % Check
    hex_coord(Hex, [X,Y]), % Get
    % Scan the neighbor hexes
    % note: We use near8/4 instead of near4/4 because units can move
    %       in the outer border and we don't want to create a new
    %       province after a unit has moved.
    near8(Map, [X, Y], NeighborHexes),
    % Filter only the valid neighbor hexes
    findall(NeighborHex,(
                % Pick one neighbor hex to validate
                member(NeighborHex, NeighborHexes),
                % To be part of the province, the hex needs to be owned by the same owner
                hex_owner(NeighborHex, Owner),
                % Check that the hex has no already been visited
                \+ member(NeighborHex, Visited),
                % Check that the hex has no already been added to the ToVisit list
                \+ member(NeighborHex, Tail)
            ),
            ValidNeighborHexes),
    % All the valid neighbour hexes need to be expanded further
    append(Tail,ValidNeighborHexes,ToVisit),
    province_bfs(Map,Owner,ToVisit, [Hex|Visited],Hexes).

% Checks if an hex is a terrain and has an owner
% hex_owned(+Map, +Coord, -Owner, -Hex)
hex_owned(Map, Coord, Owner, Hex) :-
    get_hex(Map, Coord, Hex),
    hex_tile(Hex, terrain),
    hex_owner(Hex,Owner),
    Owner \= none.

% Caller predicate for outer_border_
% outer_border(+Map, +Province, -OuterBorder)
outer_border(Map, province(_,Hexes,_), OuterBorder) :- outer_border_(Map,Hexes,Hexes,[],OuterBorder).
% Find all hexagons that border the given province externally
outer_border_(_,[],_,OuterBorder,OuterBorder).
outer_border_(Map,[Hex|RestHexes],ProvinceHexes,Found,OuterBorder) :-
    hex_coord(Hex,[X,Y]),
    % Scan the neighbor hexes
    near8(Map, [X,Y],NeighborHexes),
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
    outer_border_(Map,RestHexes,ProvinceHexes,NewFound,OuterBorder).

% Find all hexagons that border the given province internally
inner_border(Map, province(_, Hexes, _), InnerBorder) :-
    findall(Hex,(
        % For each hex in the province...
        member(Hex, Hexes),
        % ...checks that in the midst of its near8...
        hex_coord(Hex, [X, Y]),
        near8(Map, [X, Y], NearHexes),
        % ... at least one hex is outside the province
        \+ maplist(member,NearHexes,Hexes)
    ), InnerBorder).

% Purchase a building or a unit and place it on the map at the given location
% buy_and_place(+Map, +Provinces, +Province, +ResourceName, +DestHex, -NewMap, -NewProvinces, -NewProvince)
buy_and_place(Map, Provinces, Province, ResourceName, DestHex, NewMap, NewProvinces, NewProvince) :-
    % Check if the purchase is economically valid
    check_buy(Province, ResourceName, LeftMoney), % Get LeftMoney
    % Subtract the cost from the province's money (also in the provinces list)
    change_province_money(Province, LeftMoney, ProvinceWithNewMoney),
    op_list(Provinces, [In, Out] >> (In == Province, change_province_money(In, LeftMoney, Out)), ProvincesWithMoney),

    % Check if DestHex is a valid placement destination
    hex_coord(DestHex, [X, Y]), % Get
    (   % The resource to be placed is a unit:
        unit(ResourceName, _, _, _, _), % Check
        place_unit(Map, ProvincesWithMoney, ProvinceWithNewMoney, ResourceName, DestHex, NewMap, NewProvinces, NewProvince), !
    ;   % The resource to be placed is a building:
        building(ResourceName, _, _, _), % Check
        building_placement(Map, Province, ResourceName, DestHex), % Check
        % Place the building on the map
        set_building(Map, [X, Y], ResourceName, NewMap),
        update_province(NewMap, ProvinceWithNewMoney, NewProvince)
    ).

% Displace a unit on a given valid hex
% displace_unit(+Map, +Provinces, +Province, +FromHex, +ToHex, -NewMap, -NewProvinces, -NewProvince)
displace_unit(Map, Provinces, Province, FromHex, ToHex, NewMap, NewProvinces, NewProvince) :-
    % Check if the FromHex contains a unit
    hex_unit(FromHex, UnitName),

    % Ensure that the Manhattan distance is not greater than 4
    manhattan_distance(FromHex, ToHex, ManhattanDistance),
    max_displacement_distance(MaxDisplacementDistance),
    ManhattanDistance =< MaxDisplacementDistance,

    % Apply the displacement on the map:
    hex_coord(FromHex, [FromX, FromY]), % Get
    % Remove the unit from its old hex location
    set_unit(Map, [FromX, FromY], none, NewMapWithoutFromUnit),
    place_unit(NewMapWithoutFromUnit, Provinces, Province, UnitName, ToHex, NewMap, NewProvinces, NewProvince).

% Place a unit on a given hex after checking the validity of the move
% Note: this centralises the logic for both placing and displacing unit moves
% Note: this is for editing the map only
% place_unit(+Map, +Province, +UnitName, +Hex, -NewMap)
place_unit(Map, Provinces, Province, UnitName, Hex, NewMap, NewProvinces, NewProvince):-
    hex_coord(Hex, Coord), % Get
    province_owner(Province, Player), % Get
    province_owner(Province, Player), % Get
    hex_owner(Hex, OwnerBefore), % Get

    % Check if the placement is valid
    unit_placement(Map, Province, UnitName, Hex, NewUnitName), % Check & Get

    % Place the unit
    set_unit(Map, Coord, NewUnitName, MapWithUnit),
    % Ensure the player now owns the hex.
    set_owner(MapWithUnit, Coord, Player, MapWithUnitOwned),
    % Destroy any enemy building in the destination hex.
    set_building(MapWithUnitOwned, Coord, none, NewMap),

    % Update the player province
    update_province(NewMap, Province, NewProvince),

    % Check for player merge in case of conquest or invasion
    (   (OwnerBefore == none; OwnerBefore \= Player)
    ->  check_for_merge(NewMap, Provinces, NewProvince, Hex, NewProvincesMerge)
    ;   NewProvincesMerge = Provinces
    ),
    % Check for enemy split in case of invasion
    (   (OwnerBefore \= none, OwnerBefore \= Player)
    ->  check_for_split(NewMap, NewProvincesMerge, NewProvince, Hex, NewProvinces)
    ;   NewProvinces = NewProvincesMerge
    ).

% Checks if a province has been merged
% Note: This should be called both in the case of an invasion and conquest
% Note: This won't fail in case there hasn't been any merge
% check_for_merge(+Map, +OldProvinces, +NewProvince, +Hex, -NewProvinces):-
check_for_merge(Map, OldProvinces, NewProvince, Hex, NewProvinces):-
    province_owner(NewProvince, Player), % Get
    hex_coord(Hex, HexCoord), % Get

    % Find all the player's old provinces that have at least one hex in the near8 of the conquered hex
    % Note: there is always at least one province that fulfils this condition
    near8(Map, HexCoord, NearHexes),
    findall(Province,
        (   member(NearHex, NearHexes),
            member(Province, OldProvinces),
            province_owner(Province, Player), % Check
            province_hexes(Province, ProvinceHexes), % Get
            member(NearHex, ProvinceHexes)
        ), ProvincesToRemove),

    % Remove the duplicates
    filter(ProvincesToRemove, [X, Partial] >> (\+ member(X, Partial)), ProvincesToRemoveFiltered),

    % Add money to new province from provinces to remove
    share_money(ProvincesToRemoveFiltered, [NewProvince], NewProvinceWithMoney),

    % Update the new provinces list
    exclude([In] >> (member(X, ProvincesToRemoveFiltered)), OldProvinces, ProvincesWithoutNewProvince),
    append(ProvincesWithoutNewProvince, NewProvinceWithMoney, NewProvinces), !.

% Checks if a province has been split
% Note: This should be called only in the case of an invasion attack
% Note: This won't fail in case there hasn't been any split
% check_for_split(+Map, +OldProvinces, +NewProvince, +Hex, -NewProvinces)
check_for_split(Map, OldProvinces, NewProvince, Hex, NewProvinces) :-
    province_owner(NewProvince, Player), % Get
    hex_coord(Hex, HexCoord), % Get

    % Select all the enemy hexes in the nearby8 of the invaded hex
    near8(Map, HexCoord, NearHexes),
    include([In] >> (hex_owner(X, Owner), Owner\=none, Owner\=Player), NearHexes, NearEnemyHexes),
    length(NearEnemyHexes, NearEnemyHexesSize), % Get
    (   % Check if the near8 of the conquered hex contains at least 2 non-adjacent hexes
        NearEnemyHexesSize >= 2,
        nth0(0, NearEnemyHexes, Hex1), % Get
        member(Hex2, NearEnemyHexes), % Get
        Hex1 \= Hex2,
        \+ hexes_adjacent8(Hex1, Hex2),

        % Select an hex inside the near8 and check that it is not connected to all other enemy hexes in the near8
        hex_coord(Hex1,Coord1), % Get
        find_province(Map, Coord1, TestProvince),
        province_hexes(TestProvince, TestProvinceHexes), % Check
        \+ subset(NearEnemyHexes, TestProvinceHexes), !,

        % Now I can say for sure there has been a split on the enemy province
        find_provinces(Map, NearEnemyHexes, NewEnemyProvinces),

        % Select the split province in the old provinces list
        member(OldSplitProvince, OldProvinces), % Get
        province_hexes(OldSplitProvince, OldSplitProvinceHexes), % Get
        member(Hex, OldSplitProvinceHexes), % Check

        % Calculate the money for the new enemy provinces from the old split province
        share_money([OldSplitProvince], NewEnemyProvinces, NewEnemyProvincesWithMoney)
    ;
        % There hasn't been a split, but the old enemy province still needs to be recalculated due to the invasion
        % Select the split province in the old provinces list
        member(OldSplitProvince, OldProvinces), % Get
        province_hexes(OldSplitProvince, OldSplitProvinceHexes), % Get
        member(Hex, OldSplitProvinceHexes), % Check
        update_province(Map, OldSplitProvince, NewEnemyProvince),
        NewEnemyProvincesWithMoney = [NewEnemyProvince]
    ),

    % TODO here: Remove enemy provinces smaller than 2 hexes and free their hexes. Add NewMap attribute
    % exclude([In] >> (province_size(X, 1)), NewEnemyProvincesWithMoney, NewEnemyProvincesToRemove)

    % Update the new provinces list
    exclude([In] >> (X==OldSplitProvince), OldProvinces, ProvincesWithoutSplitProvince),
    append(ProvincesWithoutSplitProvince, NewEnemyProvincesWithMoney, NewProvinces), !.
