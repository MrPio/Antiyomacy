:- module(io, [player_input/0]).
:- use_module([utils, map, hex, province, unit, building, economy, eval, minimax]).

% Check user choice
player_input:-
    writeln('Choose a move:'),
    writeln('1) Displace'),
    writeln('2) Purchase'),
    read(Choice),
    (Choice =:= 1 ->
        displace_input;
    Choice =:= 2 ->
        purchase_input;
    writeln('Invalid choice'),
    player_input
    ).

% Predicate to handle user input for the "Displace" move
displace_input:-
    writeln('FromCoord (format X-Y):'),
    read(FromCoord),
    (validate_coordinate(FromCoord) ->
        true
    ; 
        writeln('Invalid coordinates. Please use the format X-Y.'),
        displace_input
    ),
    writeln('ToCoord (format X-Y):'),
    read(ToCoord),
    (validate_coordinate(ToCoord) ->
        true
    ; 
        writeln('Invalid coordinates. Please use the format X-Y.'),
        displace_input
    ).

% Predicate to handle user input for the "Purchase" move
purchase_input:-
    writeln('ResName (options: farm, tower, strong_tower, peasant, spearman, baron, knight):'),
    read(ResName),
    (validate_resource_name(ResName) ->
        true
    ; 
        writeln('Invalid resource name. Please choose from the provided options.'),
        purchase_input
    ),
    writeln('ToCoord (format X-Y):'),
    read(ToCoord),
    (validate_coordinate(ToCoord) ->
        true
    ; 
        writeln('Invalid coordinates. Please use the format X-Y.'),
        purchase_input
    ).

validate_coordinate(Coord):-
    term_to_atom(Coord, AtomCoord),
    atom_codes(AtomCoord, Codes),
    split_string(Codes, "-", "", [XStr, YStr]),
    number_codes(X, XStr),
    number_codes(Y, YStr),
    X > 0,
    Y > 0.


parse_number(Number) -->
    { number_codes(Number, Codes) },
    Codes.


% Validate the resource name
validate_resource_name(ResName):-
    member(ResName, [farm, tower, strong_tower, peasant, spearman, baron, knight]).