:- module(eval, [eval/2]).
:- use_module([hex, province, economy, minimax]).

% Calculates the evaluation function on a given game state
% TODO: recognize the win state and boost the score
% eval(+Board, -Score)
eval(Board, Score) :-
    board_provinces(Board, Provinces), % Get
    board_player(Board, Player), % Get
    board_state(Board, State), % Get
    board_conquests(Board, [MINConquests, MAXConquests]), % Get
    MIN = red,
    MAX = blue,

    % Filter player and enemy provinces
    include([In]>>(province_owner(In, MAX)), Provinces, ProvincesOfMAX),
    exclude([In]>>(province_owner(In, MAX)), Provinces, ProvincesOfMIN),

    % Calculate the total money
    maplist(province_money, ProvincesOfMAX, MAXMoneys),
    sum_list(MAXMoneys, MAXTotalMoney),
    (   MAXTotalMoney<0
    ->  MAXMoneyScore is -99999
    ;   MAXTotalMoney > 40
    ->  MAXMoneyScore is -1
    ;   MAXMoneyScore is 1
    ),
    maplist(province_money, ProvincesOfMIN, MINMoneys),
    sum_list(MINMoneys, MINTotalMoney),
    (   MINTotalMoney<0
        ->  MINMoneyScore is -99999
        ;   MINTotalMoney > 40
        ->  MINMoneyScore is -1
        ;   MINMoneyScore is 1
    ),

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

    % Calculate the resources
    province_counts(ProvincesOfMAX, [farm], MAXFarmCount),
    province_counts(ProvincesOfMIN, [farm], MINFarmCount),

    province_counts(ProvincesOfMAX, [tower], MAXTowerCount),
    province_counts(ProvincesOfMIN, [tower], MINTowerCount),

    province_counts(ProvincesOfMAX, [peasant], MAXPaesantCount),
    province_counts(ProvincesOfMIN, [peasant], MINPaesantCount),

    province_counts(ProvincesOfMAX, [spearman], MAXSpearmanCount),
    province_counts(ProvincesOfMIN, [spearman], MINSpearmanCount),

    province_counts(ProvincesOfMAX, [baron], MAXBaronCount),
    province_counts(ProvincesOfMIN, [baron], MINBaronCount),

    province_counts(ProvincesOfMAX, [knight], MAXKnightCount),
    province_counts(ProvincesOfMIN, [knight], MINKnightCount),
    
    % Check if MAX wins and assign a score of 99999
    (   State == win, Player == MAX 
    ->  Score = 99999, !
    ;   % Check if MIN wins and assign a score of -99999
        State == win, Player == MIN 
    ->  Score = -99999, !
    ;   % If the game is not in a win state, calculate the evaluation score
        Score is (
            MAXConquests     * 100 +
            MAXMoneyScore    * 20 +
            MAXTotalSize     * 90 +
            MAXTotalIncome   * 10 +
            MAXFarmCount     * 2 +
            MAXTowerCount    * 3 +
            MAXPaesantCount  * 1 +
            MAXSpearmanCount * 2 +
            MAXBaronCount    * 6 +
            MAXKnightCount   * 10) 
            - (
            MINConquests     * 100 +
            MINMoneyScore    * 20 +
            MINTotalSize     * 90 +
            MINTotalIncome   * 10 +
            MINFarmCount     * 2 +
            MINTowerCount    * 3 +
            MINPaesantCount  * 1 +
            MINSpearmanCount * 2 +
            MINBaronCount    * 6 +
            MINKnightCount   * 10)
    ).