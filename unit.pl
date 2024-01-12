:- use_module("province").
:- module(unit, [unit/5]).

% Unit struct ========================================
% unit(Name, Strength, Protection, Cost, Income)
unit(peasant, 1, 1, 10, -2).
unit(spearman, 2, 2, 20, -6).
unit(baron, 3, 3, 30, -18).
unit(knight, 4, 3, 40, -36).