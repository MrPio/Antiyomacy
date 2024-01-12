:- use_module("province").

% Building struct ====================================
building(Name, Protection, Cost, Income):- 
    string(Name),
    integer(Protection),
    integer(Cost),
    integer(Income).

building(farm, 0, 12, 4).
building(tower, 2, 15, -1).
building(strong_tower, 3, 35, -6).

