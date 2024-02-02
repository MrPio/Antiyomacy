:- module(io, [player_input/1, purchase_input/2, displace_input/2]).
:- use_module([utils, map, hex, province, unit, building, economy, eval, minimax]).

% Check user choice
% TODO: add null move
player_input(Choice):-
    writeln('Choose a move:'),
    writeln('1) Displace'),
    writeln('2) Purchase'),
    read(Choice1),
    (Choice1 =:= 1 ->
        Choice = Choice1;
    Choice1 =:= 2 ->
        Choice = Choice1;
    writeln('Invalid choice'),
    player_input(Choice)
    ).

% Predicate to handle user input for the "Displace" move
displace_input([X1,Y1], [X2,Y2]):-
    writeln('FromCoord (format X-Y):'),
    read(FromCoord),
    (validate_coordinate(FromCoord,[X1,Y1]) ->
        true
    ; 
        writeln('Invalid coordinates. Please use the format X-Y.'),
        displace_input([X1,Y1], [X2,Y2])
    ),
    writeln('ToCoord (format X-Y):'),
    read(ToCoord),
    (validate_coordinate(ToCoord,[X2,Y2]) ->
        true
    ; 
        writeln('Invalid coordinates. Please use the format X-Y.'),
        displace_input([X1,Y1], [X2,Y2])
    ).

% Predicate to handle user input for the "Purchase" move
purchase_input(ResName,[X,Y]):-
    writeln('ResName (options: farm, tower, strong_tower, peasant, spearman, baron, knight):'),
    read(ResName),
    (validate_resource_name(ResName) ->
        true
    ; 
        writeln('Invalid resource name. Please choose from the provided options.'),
        purchase_input(ResName,[X,Y])
    ),
    writeln('ToCoord (format X-Y):'),
    read(ToCoord),
    (validate_coordinate(ToCoord,[X,Y]) ->
        true
    ; 
        writeln('Invalid coordinates. Please use the format X-Y.'),
        purchase_input(ResName,[X,Y])
    ).

% Validate the format and range of coordinates
validate_coordinate(Coord,[X,Y]):-
    % Convert the Prolog term to an atom
    term_to_atom(Coord, AtomCoord),
    % Convert the atom to a list of character codes
    atom_codes(AtomCoord, Codes),
    % Split the codes using "-" as a delimiter, obtaining XStr and YStr
    split_string(Codes, "-", "", [XStr, YStr]),
    % Convert XStr and YStr to integers X and Y
    number_codes(X, XStr),
    number_codes(Y, YStr),
    % Check that both X and Y are greater than 0
    X > 0,
    Y > 0,
    inside_map([X, Y]).

% Validate the resource name
validate_resource_name(ResName):-
    member(ResName, [farm, tower, strong_tower, peasant, spearman, baron, knight]).

% TODO: check valid purchase or displace
%building_placement(Map, Province, ResourceName, DestHex), % Check
% unit_placement(Map, Province, UnitName, Hex, NewUnitName), % Check & Get ---- Pass NuwUnitName to displace_unit