% :- module(gui, [draw_map/1]).
:- use_module([map, hex, province, utils]).
:- use_module(library(pce)).
:- dynamic selected_unit/3.

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
    new(PeasantImage, image('C:/Users/Darlene/Desktop/Magistrale/ProgettoIntelligenza/Antiyomacy/resources/sprites/unit/peasant.gif')),
    new(SpearmanImage, image('C:/Users/Darlene/Desktop/Magistrale/ProgettoIntelligenza/Antiyomacy/resources/sprites/unit/spearman.gif')),
    new(BaronImage, image('C:/Users/Darlene/Desktop/Magistrale/ProgettoIntelligenza/Antiyomacy/resources/sprites/unit/baron.gif')),
    new(KnightImage, image('C:/Users/Darlene/Desktop/Magistrale/ProgettoIntelligenza/Antiyomacy/resources/sprites/unit/knight.gif')),
    new(CastleImage, image('C:/Users/Darlene/Desktop/Magistrale/ProgettoIntelligenza/Antiyomacy/resources/sprites/building/castle1.gif')),
    new(FarmImage, image('C:/Users/Darlene/Desktop/Magistrale/ProgettoIntelligenza/Antiyomacy/resources/sprites/building/farm1.gif')),
    new(TowerImage, image('C:/Users/Darlene/Desktop/Magistrale/ProgettoIntelligenza/Antiyomacy/resources/sprites/building/tower1.gif')),
    new(StrongTowerImage, image('C:/Users/Darlene/Desktop/Magistrale/ProgettoIntelligenza/Antiyomacy/resources/sprites/building/strong_tower1.gif')),

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
    % Get the position of the box
    get(Box, position, point(X, Y)),
    HexSize = 100,
    XCoord is X // HexSize,
    YCoord is Y // HexSize,
    map(Map),
    get_hex(Map, [XCoord, YCoord], hex(_, _, _, Owner, _, Unit)),
    % Check if there is a selected unit
    (   retract(selected_unit(SX, SY, SOwner)) ->
        % If there is a selected unit
        % Check if the selected box is of the same owner
        (   Owner = SOwner ->
            % TODO (qui devo aggiungere il predicato per spostare effettivamente l'unità)
            format('Unit moved from (~w, ~w) to (~w, ~w)~n', [SX, SY, XCoord, YCoord])
        ;   % If he destination box is not of the same owner
            format('The destination box is not of the same owner.~n', []),
            % Memorize the selected unit for a new attempt
            assertz(selected_unit(SX, SY, SOwner))
        ), 
        send(SBox, fill_pattern, colour(SOwner))
    ;   % If is the first click, memorize coordinates and owner of the selected unit
        (   Unit \= none ->  % Ensure there is a unit to select
            assertz(selected_unit(XCoord, YCoord, Owner)),
            format('Unit selected at (~w, ~w) owned by ~w~n', [XCoord, YCoord, Owner])
        ;   format('No unit selected.~n', [])
        )
    ).


