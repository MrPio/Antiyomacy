% :- module(gui, [draw_map/1]).
:- use_module([map, hex, province, utils]).
:- use_module(library(pce)).

gui():-
    % map generation:
    generate_random_map(_, true),
    % Manually populating the map
    RedHexes=[[0,0],[1,0],[1,1],[1,2],[0,2]],
    BlueHexes=[[2,2],[3,2],[3,3],[3,4],[4,3]],
    Buildings=[[2,2]-farm],
    Units=[[0,0]-baron],
    foreach(member(Coord,RedHexes),set_owner(Coord,red)),
    foreach(member(Coord,BlueHexes),set_owner(Coord,blue)),
    foreach(member(Coord-Building,Buildings),set_building(Coord,Building)),
    foreach(member(Coord-Unit,Units),set_unit(Coord,Unit)),
    map(Map),
    print_map(Map),

    % resources:
    new(PeasantImage, image('resources/sprites/unit/peasant.gif')),
    new(SpearmanImage, image('resources/sprites/unit/spearman.gif')),
    new(BaronImage, image('resources/sprites/unit/baron.gif')),
    new(KnightImage, image('resources/sprites/unit/knight.gif')),
    new(CastleImage, image('resources/sprites/building/castle1.gif')),
    new(FarmImage, image('resources/sprites/building/farm1.gif')),
    new(TowerImage, image('resources/sprites/building/tower1.gif')),
    new(StrongTowerImage, image('resources/sprites/building/strong_tower1.gif')),

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
        % send(Finestra, display, bitmap(Image),point(CenterX,CenterY)),
        (   Unit \= none -> 
            (   Unit = peasant -> Image = PeasantImage
            ;   Unit = spearman -> Image = SpearmanImage
            ;   Unit = baron -> Image = BaronImage
            ;   Unit = knight -> Image = KnightImage
            ),
            get(Image, size, size(ImgWidth, ImgHeight)),
            CenterX is GX + (HexSize - ImgWidth) / 2,
            CenterY is GY + (HexSize - ImgHeight) / 2,
            send(Finestra, display, bitmap(Image), point(CenterX, CenterY))
        ;   true
        ),

        % Draw the building
        (   Building \= none ->
            (   Building = castle -> Image = CastleImage
            ;   Building = farm -> Image = FarmImage
            ;   Building = tower -> Image = TowerImage
            ;   Building = strong_tower -> Image = StrongTowerImage
            ),
            get(Image, size, size(ImgWidth, ImgHeight)),
            CenterX is GX + (HexSize - ImgWidth) / 2,
            CenterY is GY + (HexSize - ImgHeight) / 2,
            send(Finestra, display, bitmap(Image), point(CenterX, CenterY))
        ;   true
        ),

        fail ; true
    ),
    send(Finestra, open),
    true.
    
on_box_select(Box) :-
    write('Hex selected!'), nl,
    send(Box, fill_pattern, colour(green)).

