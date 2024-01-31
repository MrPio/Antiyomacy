:- module(eval, [eval/2]).
:- use_module([hex, province, economy, minimax]).

% Calculates the evaluation function on a given game state
% TODO: recognize the win state and boost the score
% eval(+Board, -Score)
eval(Board, Score) :-
    board_provinces(Board, Provinces), % Get
    board_player(Board, Player), % Get
    board_state(Board, State), % Get
    MAX = blue,

    % Filter player and enemy provinces
    include([In]>>(province_owner(In, MAX)), Provinces, ProvincesOfMAX),
    exclude([In]>>(province_owner(In, MAX)), Provinces, ProvincesOfMIN),

    % Calculate the total money
    maplist(province_money, ProvincesOfMAX, MAXMoneys),
    sum_list(MAXMoneys, MAXTotalMoney),
    maplist(province_money, ProvincesOfMIN, MINMoneys),
    sum_list(MINMoneys, MINTotalMoney),

    % Calculate the total owned hexes count
    maplist(province_size, ProvincesOfMAX, MAXSizes),
    sum_list(MAXSizes, MAXTotalSize),
    maplist(province_size, ProvincesOfMIN, MINSizes),
    sum_list(MINSizes, MINTotalSize),

    % Calculate the total income
    maplist(get_income, ProvincesOfMAX, MAXIncomes),
    sum_list(MAXIncomes, MAXTotalIncome),
    maplist(get_income, ProvincesOfMIN, MINIncomes),
    sum_list(MINIncomes, MINTotalIncome),

    % Calculate the number of 'farm' buildings in the player's provinces
    province_counts(ProvincesOfMAX, [farm], MAXFarmCount),
    province_counts(ProvincesOfMIN, [farm], MINFarmCount),

    % Calculate the number of 'tower' buildings in the player's provinces
    province_counts(ProvincesOfMAX, [tower], MAXTowerCount),
    province_counts(ProvincesOfMIN, [tower], MINTowerCount),

    % Calculate the number of units in the player's provinces
    province_counts(ProvincesOfMAX, [peasant,spearman,baron,knight], MAXUnitsCount),
    province_counts(ProvincesOfMIN, [peasant,spearman,baron,knight], MINUnitsCount),

    % Calculate the evaluation score
    Score is (
        MAXTotalMoney    * 0.2 +
        MAXTotalSize     * 15 +
        MAXTotalIncome   * 8 +
        MAXFarmCount     * 1 +
        MAXTowerCount    * 0.5 +
        MAXUnitsCount    * 1) 
        - (
        MINTotalMoney    * 0.2 +
        MINTotalSize     * 15 +
        MINTotalIncome   * 8 +
        MINFarmCount     * 1 +
        MINTowerCount    * 0.5 +
        MINUnitsCount    * 1).


