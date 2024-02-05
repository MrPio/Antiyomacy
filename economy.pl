:- module(economy, [get_income/2, check_buy/3, check_buys/3, share_money/3]).
:- use_module([utils, hex, province, unit, building]).
:- use_module(library(clpfd)).

% Check/Get a province income. This will be added to the province money at the end of the turn.
% get_income(+Province, ?Income)vera
get_income(Province, Income) :-
    % Calculate the income contribution given by the number of hexes in the province
    province_hexes(Province, Hexes), % Get
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
% Checks if a list of resources can be bought and returns the remaining money of the province
% This is useful to list all the possible purchase moves for a given province
% check_buy(+Province, ?ResourcesList, -LeftMoney)
check_buys(Province, [ResourceName|Tail], _):-
    check_buy(Province, ResourceName, LeftMoney),
    change_province_money(Province, LeftMoney, NewProvince),
    check_buys(NewProvince, Tail, LeftMoney).
check_buys(_, [], _).

% Checks whether a building or unit purchase can be achieved and returns the remaining money of the province
% check_buy(+Province, ?ResourceName, -LeftMoney)
check_buy(Province, BuildingName, LeftMoney) :-
    building(BuildingName,_,_,_), % Check
    building_cost(BuildingName, Province, Cost),
    province_money(Province, Money), % Get
    Money>=Cost,
    LeftMoney is Money - Cost.
check_buy(Province, UnitName, LeftMoney) :-
    unit(UnitName, _, _, Cost, _),
    province_money(Province, Money), % Get
    Money>=Cost,
    LeftMoney is Money - Cost.

% Calculates the proportional share of split/merged provinces based on the original provinces
% Note: this is called after a merge or after a split
% share_money(+OldProvinces, +NewProvinces, -UpdatedNewProvinces)
share_money(OldProvinces, NewProvinces, UpdatedNewProvinces):-
    % Calculate the sum of old provinces money
    maplist(province_money, OldProvinces, OldProvincesMoneys),
    sum_list(OldProvincesMoneys, OldMoney),
    % Calculate the sum of new provinces sizes
    maplist(province_size, NewProvinces, NewSizes),
    sum_list(NewSizes, NewTotalSize),
    % Allocate the money to the new provinces as a weighted average
    op_list(NewSizes, [In, Out]>>(Out is (In * OldMoney) // NewTotalSize), NewMoneys),
    % TODO here: add the difference between the sum of the new money and the old money
    % Update the new provinces with the newly calculated money
    maplist(change_province_money, NewProvinces, NewMoneys, UpdatedNewProvinces).