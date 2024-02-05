:- module(io, [ask_displace/6, ask_purchase/5, ask_choice/2]).
:- use_module([map, hex, province, unit, building, economy]).
:- use_module(library(readutil)).

% HIGH LEVEL INPUTS ========================================================================================
% Ask the user to input all the detail for a displace move
% Note: this always returns a valid mode
% Note: ask_displace/2 = 2 * ask_coordinates/1
% ask_displace(+Map, +Province, -CoordFrom, -CoordTo, -NewUnitName, -Cancel)
ask_displace(Map, Province, CoordFrom, CoordTo, NewUnitName, Cancel):-
    writeln('Which unit do you want to displace? (format \'X-Y\', \'q\' to cancel): '),
    ask_displace_from_(Province, CoordFrom, UnitName, Cancel1),
    (   Cancel1 == true, Cancel = Cancel1
    ;   writeln('Where do you want to displace it? (format \'X-Y\', \'q\' to cancel): '),
        ask_displace_to_(Map, Province, UnitName, CoordFrom, CoordTo, NewUnitName, Cancel)
    ).

% ask_displace_from_(+Province, -CoordFrom, -UnitName, -Cancel)
ask_displace_from_(Province, CoordFrom, UnitName, Cancel):-
    province_hexes(Province, Hexes),
    ask_coordinates(CoordFromToCheck, Cancel1),
    (   Cancel1 == true, Cancel = Cancel1
    ;   % Find the hex in the province at the given coordinates
        member(Hex, Hexes), % Get
        hex_coord(Hex, CoordFromToCheck), % Check
        \+ hex_unit(Hex, none), !, % Check
        hex_unit(Hex, UnitName), % Get
        [CoordFrom, Cancel] = [CoordFromToCheck, false]
    ;   % The chosen hex does not belong to the province or contains no units
        writeln('Please choose a valid unit \'X-Y\': '),
        ask_displace_from_(Province, CoordFrom, UnitName, Cancel)
    ).

% ask_displace_to_(+Map, +Province, +UnitName, +CoordFrom, -CoordTo, -NewUnitName, -Cancel)
ask_displace_to_(Map, Province, UnitName, CoordFrom, CoordTo, NewUnitName, Cancel):-
    ask_coordinates(CoordToToCheck, Cancel1),
    (   Cancel1 == true, Cancel = Cancel1
    ;   CoordToToCheck \== CoordFrom,
        get_hex(Map, CoordToToCheck, DestHex),
        unit_placement(Map, Province, UnitName, DestHex, NewUnitName), !,
        [CoordTo, Cancel] = [CoordToToCheck, false]
    ;   % The chosen hex does not belong to the province or contains no units
        writeln('Please choose a valid destination \'X-Y\': '),
        ask_displace_to_(Map, Province, UnitName, CoordFrom, CoordTo, NewUnitName, Cancel)
    ).

% Ask the user to input all the detail for a purchase move
% Note: this always returns a valid mode
% Note: ask_purchase/4 = ask_resource/1 + ask_coordinates/1
% ask_purchase(+Map, +Province, -ResName, -Coord, -Cancel)
ask_purchase(Map, Province, ResName, Coord, Cancel):-
    building_cost(farm, Province, FarmCost),
    format('What resource do you want to buy? (\'q\' to cancel) \npeasant(10$), spearman(20$), baron(30$), knight(40$)\nfarm(~w$), tower(15$), strong_tower(35$)',FarmCost), nl,
    ask_purchase_res_(Province, ResName, Cancel1),
    (   Cancel1 == true, Cancel = Cancel1
    ;   writeln('Where do you want to place it? (format \'X-Y\', \'q\' to cancel): '),
        ask_purchase_to_(Map, Province, ResName, Coord, Cancel)
    ).

% ask_purchase_res_(+Province, -ResName, -Cancel)
ask_purchase_res_(Province, ResName, Cancel):-
    ask_resource(ResNameToCheck, Cancel1),
    (   Cancel1 == true, Cancel = Cancel1
    ;   check_buy(Province, ResNameToCheck, _), !, % Check
        [ResName, Cancel] = [ResNameToCheck, false]
    ;   % The chosen hex does not belong to the province or contains no units
        writeln('You don\'t have enough money. Please retry or press \'q\' to cancel: '),
        ask_purchase_res_(Province, ResName, Cancel)
    ).

% ask_purchase_to_(+Map, +Province, +ResName, -Coord, -Cancel)
ask_purchase_to_(Map, Province, ResName, Coord, Cancel):-
    ask_coordinates(CoordToCheck, Cancel1),
    (   Cancel1 == true, Cancel = Cancel1
    ;   get_hex(Map, CoordToCheck, DestHex),
        % Prevent a unit merge on purchase
        hex_unit(DestHex, none), % Check
        % Check if the placement is valid based on the resource type
        (   building(ResName, _, _, _)
        ->  building_placement(Map, Province, ResName, DestHex)
        ;   unit_placement(Map, Province, ResName, DestHex, _)
        ), !,
        [Coord, Cancel] = [CoordToCheck, false]
    ;   % The chosen hex does not belong to the province or contains no units
        writeln('Please choose a valid destination \'X-Y\' or press \'q\' to cancel: '),
        ask_purchase_to_(Map, Province, ResName, Coord, Cancel)
    ).

% LOW LEVEL INPUTS =========================================================================================
% Ask the user to make a choice from the alternatives provided
% ask_choice(+Choice, -Choice)
ask_choice(Choices, Choice):-
    length(Choices, ChoicesLength),
    read_line_to_string(user_input, ChoiceStr),
    number_codes(ChoiceToCheck, ChoiceStr),
    % Validate the input
    (   between(1, ChoicesLength, ChoiceToCheck), !,
        nth1(ChoiceToCheck, Choices, Choice)
    ;   % The input was not valid
        format('Please write a number between 1 and ~w: ', ChoicesLength), nl,
        ask_choice(Choices, Choice)
    ).
% Ask the user to input a resource name
% ask_resource(-ResName, -Cancel)
ask_resource(ResName, Cancel):-
    read_line_to_string(user_input, ResNameStr),
    string_lower(ResNameStr,ResNameStrLower),
    (   member(ResNameStrLower,["q","exit","quit","back","esc"])
    ->  Cancel = true    
    ;   % Validate the input
        atom_string(ResNameToCheck, ResNameStrLower),
        (building(ResNameToCheck, _, _, _); unit(ResNameToCheck, _, _, _, _))
    ->  [ResName, Cancel] = [ResNameToCheck, false]
    ;   writeln('Please write a valid resource name or press \'q\' to cancel: '),
        ask_resource(ResName, Cancel)
    ).

% Ask the user to input a coordinate
% ask_coordinates(-Coord, -Cancel)
ask_coordinates(Coord, Cancel) :-
    read_line_to_string(user_input, CoordStr),
    string_lower(CoordStr,CoordStrLower),
    (   member(CoordStrLower,["q","exit","quit","back","esc"])
    ->  Cancel = true
    ;   % Converts input coordinates to atoms and validates them
        split_string(CoordStr, "-", "", [XStr, YStr]),
        atom_number(XStr, X), atom_number(YStr, Y),
        inside_map([X, Y])
    ->  [Coord, Cancel] = [[X, Y], false]
    ;   writeln('Please use the format \'X-Y\' or press \'q\' to cancel: '),
        ask_coordinates(Coord, Cancel)
    ).