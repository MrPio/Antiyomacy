:- module(print_map, [print_map/1]).

print_map(Map):- maplist(print_row,Map).

print_row([]) :- nl,!.
print_row([H|T]):- write(H),write(' '),print_row(T).

