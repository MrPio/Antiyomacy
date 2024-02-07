:- module(utils, [
    lap/0, lap/1,
    print_board/1,
    print_provinces/1,
    print_map/1, 
    same_elements/2,
    op_list/3,
    filter/3,
    pipe/4]).
:- use_module([hex, province, economy, minimax]).

:- dynamic(time/1).
time(0).
update_time(Time):-
    retractall(time(_)),
    assert(time(Time)).

% Take a stopwatch lap and print the elapsed time if demanded
% Note: this is useful for benchmarking purposes
% lap(+Print)
lap:-lap(0).
lap(Print):-
    time(StartTime),
    get_time(EndTime),
    update_time(EndTime),
    (   string(Print)
    ->  write(Print)
    ;   ElapsedTime is truncate((EndTime-StartTime)*1000000),
        format(': t = ~w micro', ElapsedTime), nl
    ), !.
    

% Print a game board in a nice pleasant way
% print_board(+Board)
print_board(Board):-
    board_map(Board, Map), % Get
    board_provinces(Board, Provinces), % Get
    board_conquests(Board, Conquests), % Get
    board_provinces(Board, Provinces), % Get
    print_map(Map),
    print_provinces(Provinces),
    format("Red conquests = ~w,   Blue conquests = ~w", Conquests), nl.

% Print a list of provinces in a nice pleasant way
print_provinces(Provinces):-
    include([In]>>(province_owner(In, red)), Provinces, ProvincesRed),
    exclude([In]>>(province_owner(In, red)), Provinces, ProvincesBlue),
    (   ProvincesRed == []
    ;   writeln('Red provinces: --------------------'),
        print_provinces_(ProvincesRed),nl
    ),
    (   ProvincesBlue == []
    ;   writeln('Blue provinces: -------------------'),
        print_provinces_(ProvincesBlue),nl
    ).
print_provinces_([]).
print_provinces_([Province|T]):-
    province_money(Province, Money), % Get
    province_size(Province, Size), % Get
    get_income(Province, Income), % Get
    write('['),
    format('Money = ~w | Income = ~w | Size = ~w', [Money, Income, Size]),
    write(']'),nl,
    print_provinces_(T).

% Print a map row with lateral coordinates
print_map(Map) :- 
    \+ var(Map),
    nl, write("    "),foreach(nth0(Index,Map,_), format(" ~w  ",Index)),nl,nl,
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

% Check whether two lists contain the same elements, regardless of their order (deterministic)
% same_elements(+L1, +L2)
same_elements(L1, L2) :-
    subset(L1, L2), 
    subset(L2, L1),
    same_length(L1, L2),!.

% Invoke an operation on all the element of a list (deterministic)
% prod_list(+List,:Num,-ResultList)
op_list([],_,[]):-!.
op_list([H|T], Op, [H1|T1]) :- 
    (call(Op,H,H1); H1 = H),
    op_list(T, Op, T1).

% Filter a list with a condition related to the list being generated (deterministic)
% filter(+List, :Condition, -Sol)
filter(List, Condition, Sol):- filter_(List, Condition, [], Sol).
filter_([],_,Sol,Sol):-!.
filter_([H|T], Condition, Temp, Sol) :-
    call(Condition, H, Temp),
    append(Temp, [H], NewTemp),
    filter_(T, Condition, NewTemp, Sol)
    ;
    filter_(T, Condition, Temp, Sol).

% Recursively pipe the calls of a predicate, taking the output 
% of the previous step as input at each step, and returning
% the output as the input for the next step. (⬆️higher-order⬆️)
% pipe(:Functor, +InputList, +List, -LastOutput)
pipe(_, Out, [], Out).
pipe(Op, In, [H|T], Out):-
    % Instantiate the list of output vars
    length(Out, OutLength), length(Out1, OutLength),
    % Create the univ list with the the functor as
    % the first element and its arguments as the tail
    append([Op|In], [H|Out1], UnivList),
    % Instantiate the funtor with univ predicate
    Caller =.. UnivList,
    call(Caller),
    % Perform the recursion preserving the output vars
    pipe(Op, Out1, T, Out).

