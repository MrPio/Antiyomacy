:- module(eval, [eval/2]).
:- use_module([hex, province, economy, minimax]).

% Calculates the evaluation function on a given game state
% TODO: recognize the win state and boost the score
% eval(+Board, -Score)
eval(Board, Score) :-
    board_provinces(Board, Provinces), % Get
    MAX = blue,

    % Filter player and enemy provinces
    include([In]>>(province_owner(In, MAX)), Provinces, ProvincesOfPlayer),
    exclude([In]>>(province_owner(In, MAX)), Provinces, ProvincesOfEnemy),

    % Calculate the total money
    maplist(province_money, ProvincesOfPlayer, PlayerMoneys),
    sum_list(PlayerMoneys, PlayerTotalMoney),
    maplist(province_money, ProvincesOfEnemy, EnemyMoneys),
    sum_list(EnemyMoneys, EnemyTotalMoney),

    % Calculate the total owned hexes count
    maplist(province_size, ProvincesOfPlayer, PlayerSizes),
    sum_list(PlayerSizes, PlayerTotalSize),
    maplist(province_size, ProvincesOfEnemy, EnemySizes),
    sum_list(EnemySizes, EnemyTotalSize),

    % Calculate the total income
    maplist(get_income, ProvincesOfPlayer, PlayerIncomes),
    sum_list(PlayerIncomes, PlayerTotalIncome),
    maplist(get_income, ProvincesOfEnemy, EnemyIncomes),
    sum_list(EnemyIncomes, EnemyTotalIncome),

    % Calculate the number of 'farm' buildings in the player's provinces
    province_counts(ProvincesOfPlayer, [farm], PlayerFarmCount),
    province_counts(ProvincesOfEnemy, [farm], EnemyFarmCount),

    % Calculate the number of 'tower' buildings in the player's provinces
    province_counts(ProvincesOfPlayer, [tower], PlayerTowerCount),
    province_counts(ProvincesOfEnemy, [tower], EnemyTowerCount),

    % Calculate the number of units in the player's provinces
    province_counts(ProvincesOfPlayer, [peasant,spearman,baron,knight], PlayerUnitsCount),
    province_counts(ProvincesOfEnemy, [peasant,spearman,baron,knight], EnemyUnitsCount),

    % Calculate the evaluation score
    Score is (
        PlayerTotalMoney    * 0.2 +
        PlayerTotalSize     * 15 +
        PlayerTotalIncome   * 8 +
        PlayerFarmCount     * 1 +
        PlayerTowerCount    * 0.5 +
        PlayerUnitsCount    * 1) 
        - (
        EnemyTotalMoney    * 0.2 +
        EnemyTotalSize     * 15 +
        EnemyTotalIncome   * 8 +
        EnemyFarmCount     * 1 +
        EnemyTowerCount    * 0.5 +
        EnemyUnitsCount    * 1).


