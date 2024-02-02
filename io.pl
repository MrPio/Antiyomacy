:- module(io, [player_color/1, player_move/1, purchase_input/2, displace_input/2]).
:- use_module([utils, map, hex, province, unit, building, economy, eval, minimax]).
:- use_module(library(random)).


% Predicate to check user color choice
% player_color(-Choice)
player_color(Choice):-
    writeln('Choose a color:'),
    writeln('1) Red'),
    writeln('2) Blue'),
    writeln('3) Random'),
    read(Choice1),
    (Choice1 =:= 1 ->
        writeln('You chose Red territory'),
        Choice = Choice1;
    Choice1 =:= 2 ->
        writeln('You chose Blue territory'),
        Choice = Choice1;
    Choice1 =:= 3 ->
        % Randomly choose between 1 and 2
        random_between(1, 2, RandomChoice),
        (RandomChoice =:= 1 ->
            writeln('Randomly chosen: Red territory'),
            Choice = 1
        ; 
            writeln('Randomly chosen: Blue territory'),
            Choice = 2
        );
    writeln('Invalid choice'),
    player_color(Choice)
    ).

% Predicate to check user move choice
% player_move(-Choice)
player_move(Choice):-
    writeln('Choose a move:'),
    writeln('1) Displace'),
    writeln('2) Purchase'),
    writeln('3) Skip turn'),
    read(Choice1),
    (Choice1 =:= 1 ->
        Choice = Choice1;
    Choice1 =:= 2 ->
        Choice = Choice1;
    Choice1 =:= 3 ->
        Choice = Choice1;
    writeln('Invalid choice'),
    player_move(Choice)
    ).

% Predicate to handle user input for the "Displace" move
% displace_input(-[X1,Y1], -[X2,Y2])
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
% purchase_input(-ResName, -[X,Y])
purchase_input(ResName,[X,Y]):-
    writeln('ResName (options: farm, tower, strong_tower, peasant, spearman, baron, knight):'),
    read(ResName1),
    writeln('ToCoord (format X-Y):'),
    read(ToCoord),
    (validate_resource_name(ResName1), validate_coordinate(ToCoord,[X,Y]) ->
        ResName = ResName1
    ; 
        writeln('Invalid input. Please try again.'),
        purchase_input(ResName,[X,Y])
    ).

% Validate coordinates and return the format [X,Y]
% validate_coordinate(+Coord,-[X,Y])
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
    % Check that both X and Y are greater than 0 and inside map
    X >= 0,
    Y >= 0,
    inside_map([X, Y]).

% Validate the resource name
% validate_resource_name(+ResName)
validate_resource_name(ResName):-
    member(ResName, [farm, tower, strong_tower, peasant, spearman, baron, knight]).

% TODO: check valid purchase or displace
%building_placement(Map, Province, ResourceName, DestHex), % Check
% unit_placement(Map, Province, UnitName, Hex, NewUnitName), % Check & Get ---- Pass NuwUnitName to displace_unit