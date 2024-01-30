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



    % Calculate the evaluation score
    Score is (
        PlayerTotalMoney    * 0.4 +
        PlayerTotalSize     * 5 +
        PlayerTotalIncome   * 1 ) 
        - (
        EnemyTotalMoney    * 0.4 +
        EnemyTotalSize     * 5 +
        EnemyTotalIncome   * 1 ).


