:- module(economy, [get_income/2, check_buy/3]).
:- use_module([hex, province, unit, building]).

% Calculate a province income. This will be added to the province money at the end of the turn.
get_income(province(_, Hexes, _), Income) :-
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
% Checks whether a building or unit purchase can be achieved and returns the returns the province's remaining money
% This is useful to list all the possible purchase moves for a given province
check_buy(Province, BuildingName, LeftMoney) :-
    building_cost(BuildingName, Province, Cost),
    province_money(Province, Money),
    writeln(Money),
    writeln(Cost),
    Money>=Cost,
    LeftMoney is Money - Cost.
check_buy(province(_, _, Money), UnitName, LeftMoney) :-
    unit(UnitName, _, _, Cost, _),
    Money>=Cost,
    LeftMoney is Money - Cost.
