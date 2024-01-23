:- module(province, [province/3,
                     province_owner/2,
                     change_province_owner/3,
                     province_hexes/2,
                     change_province_hexes/3,
                     province_money/2,
                     change_province_money/3,
                     province_count/3,
                     update_province/3,
                     apply_income/4,
                     near24/3,
                     near8/3,
                     near4/3,
                     find_provinces/2,
                     find_province/3,
                     outer_border/3,
                     inner_border/3,
                     buy_and_place/6,
                     displace_unit/6,
                     merge_units/6,
                     divide_money_after_attack/5]).
:- use_module([printer, map, hex, unit, building, economy]).

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

% Caller predicate for find_provinces_
% find_provinces(+Map, -Provinces)
find_provinces(Map, Provinces) :- find_provinces_(Map,0,[],Provinces),!.
% Find all the provinces in the map
find_provinces_(_,Index,Provinces,Provinces) :-
    % If all the map has been scanned, break the recursion
    map_size(MapSize), Index is MapSize^2.
find_provinces_(Map,Index,Found,Provinces) :-
    % Calculate the coordinates from the index
    index2coord(Index, [X,Y]),
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
% buy_and_place(+Map, +Province, +ResourceName, +DestHex, -NewMap, -NewProvince)
buy_and_place(Map, Province, ResourceName, DestHex, NewMap, NewProvince) :- 
    % Check if the purchase is valid
    check_buy(Province, ResourceName, LeftMoney), % Get LeftMoney
    % Subtract the cost from the province's money
    change_province_money(Province, LeftMoney, ProvinceWithNewMoney),
    % Check if DestHex is a valid placement destination
    hex_coord(DestHex, [X, Y]), % Get
    (
        unit(ResourceName, _, _, _, _), % Check
        unit_placement(Map, Province, ResourceName, DestHex), % Check
        % Place the unit on the map
        set_unit(Map, [X, Y], ResourceName, MapWithUnit),
        % Ensure the player now owns the hex.
        province_owner(Province, Player),
        set_owner(MapWithUnit, [X, Y], Player, MapWithUnitOwned),
        % Destroy any enemy building in the destination hex.
        set_building(MapWithUnitOwned, [X, Y], none, NewMap)
        ;
        building(ResourceName, _, _, _), % Check
        building_placement(Map, Province, ResourceName, DestHex), % Check
        % Place the building on the map
        set_building(Map, [X, Y], ResourceName, NewMap)
    ),
    update_province(NewMap, ProvinceWithNewMoney, NewProvince).

% Displace a unit on a given valid hex
% displace_unit(+Map, +Province, +FromHex, +ToHex, -NewMap, -NewProvince)
displace_unit(Map, Province, FromHex, ToHex, NewMap, NewProvince) :- 
        % Check if the FromHex contains a unit
        hex_unit(FromHex, UnitName),
        UnitName \= none,
        % Check if the displacement is valid
        unit_placement(Map, Province, UnitName, ToHex), % Check

        % Place the unit on the map
        hex_coord(ToHex, [ToX, ToY]), % Get
        set_unit(Map, [ToX, ToY], UnitName, MapWithUnit),
        % Ensure the player now owns the hex.
        province_owner(Province, Player),
        set_owner(MapWithUnit, [ToX, ToY], Player, MapWithUnitOwned),
        % Destroy any enemy building in the destination hex.
        set_building(MapWithUnitOwned, [ToX, ToY], none, NewMapWithDuplicateUnit),
        % Remove the unit from its old hex location
        hex_coord(FromHex, [FromX, FromY]), % Get
        set_unit(NewMapWithDuplicateUnit, [FromX, FromY], none, NewMap),

        update_province(NewMap, Province, NewProvince).

    % merge_units(+Map, +Province, +FromHex, +DestHex, -MapMerged, -NewProvince)
    merge_units(Map, Province, FromHex, DestHex, MapMerged, NewProvince) :-
        % Check if DestHex is a valid placement destination
        hex_coord(FromHex, [X, Y]), % Get
        hex_coord(DestHex, [P, Q]), % Get
        hex_owner(FromHex,FromHexOwner),
        province_owner(Province, Player),
        FromHexOwner==Player, % Check if hex is owned by the player
        hex_unit(FromHex, UnitName1), 
        UnitName1 \= none,
        hex_unit(FromHex, UnitName2),
        UnitName2 \= none,

        unit_placement_merge(Map, Province, UnitName1, UnitName2, DestHex),
        (   UnitName1==peasant,
            UnitName2==peasant,
            set_unit(Map, [X, Y], none, Map2),
            set_unit(Map2, [P, Q], none, Map3),
            set_unit(Map3, [P, Q], spearman, MapMerged)
        
        ;
        
            UnitName1==peasant,
            UnitName2==spearman,
            set_unit(Map, [X, Y], none, Map2),
            set_unit(Map2, [P, Q], none, Map3),
            set_unit(Map3, [P, Q], baron, MapMerged)
        ;
            UnitName1==spearman,
            UnitName2==peasant,
            set_unit(Map, [X, Y], none, Map2),
            set_unit(Map2, [P, Q], none, Map3),
            set_unit(Map3, [P, Q], baron, MapMerged)
            
        
        ;

            UnitName1==peasant,
            UnitName2==baron,
            set_unit(Map, [X, Y], none, Map2),
            set_unit(Map2, [P, Q], none, Map3),
            set_unit(Map3, [P, Q], knight, MapMerged)
        ;
            UnitName1==baron,
            UnitName2==peasant,
            set_unit(Map, [X, Y], none, Map2),
            set_unit(Map2, [P, Q], none, Map3),
            set_unit(Map3, [P, Q], knight, MapMerged)
        
        
        ;
            UnitName1==spearman,
            UnitName2==spearman,
            set_unit(Map, [X, Y], none, Map2),
            set_unit(Map2, [P, Q], none, Map3),
            set_unit(Map3, [P, Q], knight, MapMerged)
            
        ),
        update_province(MapMerged, Province, NewProvince).

% This predicate takes the original province and the two new provinces resulting from the attack
% as input. It calculates the proportional share of money from the original province and updates
% the money of the new provinces accordingly.
% divide_money_after_attack(+OriginalProvince, +NewProvince1, +NewProvince2, -NewProvince1Updated, -NewProvince2Updated)
divide_money_after_attack(OriginalProvince, NewProvince1, NewProvince2, NewProvince1Updated, NewProvince2Updated) :-
    % Get the money from the original province
    province_money(OriginalProvince, OriginalMoney),
    
    % Calculate the money proportion for first province
    province_hexes(OriginalProvince, OriginalHexes),
    length(OriginalHexes, OriginalSize),
    province_hexes(NewProvince1, NewHexes1),
    length(NewHexes1, NewSize1),
    
    % Calculate the money share for first province
    MoneyRatio1 is NewSize1 / OriginalSize,
    
    Money1 is round(OriginalMoney * MoneyRatio1),
    Money2 is (OriginalMoney - Money1),
    
    % Update the money of the new provinces
    change_province_money(NewProvince1, Money1, NewProvince1Updated),
    change_province_money(NewProvince2, Money2, NewProvince2Updated).