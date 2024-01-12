:- use_module("province").

% Unit struct ========================================
unit(Name, Strength, Protection, Cost, Income):-
    string(Name),
    integer(Strength),
    integer(Protection),
    integer(Cost),
    integer(Income).

unit(Peasant, 1, 1, 10, -2).
unit(Spearman, 2, 2, 20, -6).
unit(Baron, 3, 3, 30, -18).
unit(Knight, 4, 3, 40, -36).