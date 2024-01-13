:- use_module(hex).
:- module(print_map, [print_map/1]).

% Print a map row with lateral coordinates
print_map(Map):- 
    nl,write("    "),foreach(nth0(Index,Map,_),format(" ~w  ",Index)),nl,nl,
    foreach(nth0(Index, Map, Row),(format("~w   ",Index),print_row(Row))).

% Print a map row following the pattern Owner-Building-Unit for each hex
print_row([]) :- nl,nl,!.
print_row([Hex|T]) :- 
    (    
        hex_tile(Hex, sea),
        write('     |')
        ;
        hex_owner(Hex, Owner),
        hex_building(Hex, Building),
        hex_unit(Hex, Unit),
        (Owner=none -> O="_" ; first_char(Owner,O)),
        (Building=none -> B="_" ; first_char(Building,B)),
        (Unit=none -> U="_" ; first_char(Unit,U)),
        format("~w~w~w",[O,B,U]), write('|')
    ),
    print_row(T).

% Retrieve the first char of a given string
first_char(String, FirstChar) :-
    string_chars(String, [FirstChar|_]).
