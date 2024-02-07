% :- module(gui, [draw_map/1]).
:- use_module([map, hex, province, utils]).
:- use_module(library(pce)).

gui():-
    % map generation:
    generate_random_map(MapWithoutProvinces, false),
    spawn_provinces(MapWithoutProvinces, Map),
    print_map(Map),

    % resources:
    new(PeasantImage, image('resources/sprites/unit/peasant.gif')),
    new(SpearmanImage, image('resources/sprites/unit/spearman.gif')),
    new(BaronImage, image('resources/sprites/unit/baron.gif')),
    new(KnightImage, image('resources/sprites/unit/knight.gif')),
    new(CastleImage, image('resources/sprites/building/castle.gif')),
    new(FarmImage, image('resources/sprites/building/farm.gif')),
    new(TowerImage, image('resources/sprites/building/tower.gif')),
    new(StrongTowerImage, image('resources/sprites/building/strong_tower.gif')),

    % parameters:
    MapSize = 7,
    HexSize = 100,
    TotalSize = MapSize* HexSize + HexSize,

    new(Finestra, picture('Antiyomacy')),
    send(Finestra, size, size(TotalSize, TotalSize)),
    (   between(0, MapSize, X),
        between(0, MapSize, Y),
        get_hex(Map, [X, Y], hex(_, _, Tile, Owner, Building, Unit)),

        (   sea(Tile)->Color=black
        ;   nth0(PlayerIndex, [red, blue, none], Owner), nth0(PlayerIndex, [red, blue, grey], Color)
        ), 

        % Draw the hex
        GX is X * HexSize,
        GY is Y * HexSize,
        new(M, box(HexSize,HexSize)),
        send(M, fill_pattern, colour(Color)),
        send(M, recogniser, click_gesture(left, '', single, message(@prolog, on_box_select, M))),
        send(Finestra, display, M, point(GY,GX)),
        
        % Draw the unit
        % send(Finestra, display, bitmap(PeasantImage),point(GX,GY)),

        fail ; true
    ),
    send(Finestra, open),
    true.
    
on_box_select(Box) :-
    write('Hex selected!'), nl,
    send(Box, fill_pattern, colour(green)).

