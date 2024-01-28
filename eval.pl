:- module(eval, [eval/2]).
:- use_module([hex, province, economy]).

% Calculates the evaluation function on a given game state
% TODO: recognize the win state and boost the score
% eval(+Provinces, +Player, -Score)
eval(Provinces, Player, Score) :-
    % Filter player and enemy provinces
    include([In]>>(province_owner(In, Player)), Provinces, ProvincesOfPlayer),
    exclude([In]>>(province_owner(In, Player)), Provinces, ProvincesOfEnemy),

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

    % Calculate the evaluation score
    Score is (
        PlayerTotalMoney    * 1 +
        PlayerTotalSize     * 5 +
        PlayerTotalIncome   * 1 ) 
        - (
        EnemyTotalMoney    * 1 +
        EnemyTotalSize     * 5 +
        EnemyTotalIncome   * 1 ).


