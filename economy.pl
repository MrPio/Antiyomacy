:- use_module(province).
:- use_module(building).
:- use_module(unit).
:- module(economy, [get_income/2]).

get_income(province(Owner, Hexes, Money), Income):-
    length(Hexes, ProvinceSize),
    (findall(I, (
        member(Hex,Hexes),
        hex_building(Hex,BuildingName),
        building(BuildingName, _, _, I)
    ), BuildingIncomes) -> sumlist(BuildingIncomes, TotalBuildingIncome) ; TotalBuildingIncome = 0),
    (findall(I, (
        member(Hex,Hexes),
        hex_unit(Hex,UnitName),
        unit(UnitName, _, _, _, I)
    ), UnitIncomes) -> sumlist(UnitIncomes, TotalUnitIncome) ; TotalUnitIncome = 0),
    Income is ProvinceSize + TotalBuildingIncome + TotalUnitIncome.