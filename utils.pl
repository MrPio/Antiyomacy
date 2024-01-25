:- module(utils, [print_map/1, 
    same_elements/2,
    op_list/3,
    filter/3]).
:- use_module([hex]).

% Print a map row with lateral coordinates
print_map(Map) :- 
    nl,write("    "),foreach(nth0(Index,Map,_),format(" ~w  ",Index)),nl,nl,
    foreach(nth0(Index, Map, Row),(format("~w  |",Index),print_row(Row))).

% Print a map row following the pattern Owner-Building-Unit for each hex
print_row([]) :- nl,nl,!.
print_row([Hex|T]) :- 
    (    
        hex_tile(Hex, sea),
        write('   |'),!
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

% Check whether two lists contain the same elements, regardless of their order
% same_elements(+L1, +L2)
same_elements(L1, L2) :-
    subset(L1, L2), 
    subset(L2, L1),
    same_length(L1, L2).

% Invoke an operation on all the element of a list
% prod_list(+List,+Num,-ResultList)
op_list([],_,[]).
op_list([H|T], Op, [H1|T1]) :- 
    (call(Op,H,H1); H1 = H),
    op_list(T, Op, T1).

% Filter a list with a condition related to the list being generated
% filter(+List, +Condition, -Sol)
filter(List, Condition, Sol):- filter_(List, Condition, [], Sol).
filter_([],_,Sol,Sol).
filter_([H|T], Condition, Temp, Sol) :-
    call(Condition, H, Temp),
    append(Temp, [H], NewTemp),
    filter_(T, Condition, NewTemp, Sol)
    ;
    filter_(T, Condition, Temp, Sol).
