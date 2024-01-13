:- use_module(province).
:- use_module(building).
:- use_module(unit).
:- module(economy, [get_income/2, buy/2]).

% Calculate a province income. This will be added to the province money at the end of the turn.
get_income(province(Owner, Hexes, Money), Income):-
    % Calculate the income contribution given by the number of hexes in the province
    length(Hexes, ProvinceSize),
    % Calculate the income contribution given by the province buildings
    (findall(I, (
        member(Hex,Hexes),
        hex_building(Hex,BuildingName),
        building(BuildingName, _, _, I)
    ), BuildingIncomes) -> sumlist(BuildingIncomes, TotalBuildingIncome) ; TotalBuildingIncome = 0),
    % Calculate the income contribution given by the province units
    (findall(I, (
        member(Hex,Hexes),
        hex_unit(Hex,UnitName),
        unit(UnitName, _, _, _, I)
    ), UnitIncomes) -> sumlist(UnitIncomes, TotalUnitIncome) ; TotalUnitIncome = 0),
    Income is ProvinceSize + TotalBuildingIncome + TotalUnitIncome.

% Moves ======================================================
% Checks whether a building or unit purchase can be achieved
% This is useful to list all the possible purchase moves for a given province
buy(province(_, _, Money), Building) :-
    building(Building, Protection, Cost, Income),
    Money>=Cost.
buy(province(_, _, Money), Unit) :-
    unit(Unit, Strength, Protection, Cost, Income),
    Money>=Cost.