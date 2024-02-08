:- module(gui, [gui/1]).
:- use_module([map, hex, province, utils]).
:- use_module(library(pce)).
:- dynamic selected_unit/4.

test_gui:-
    % map generation:
    generate_random_map(_, false),
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


    gui(Map).

gui(Map):-
    free(@window),

    % resources:
    new(PeasantImage, image('resources/sprites/unit/peasant.gif')),
    new(SpearmanImage, image('resources/sprites/unit/spearman.gif')),
    new(BaronImage, image('resources/sprites/unit/baron.gif')),
    new(KnightImage, image('resources/sprites/unit/knight.gif')),
    new(CastleImage, image('resources/sprites/building/castle1.gif')),
    new(FarmImage, image('resources/sprites/building/farm1.gif')),
    new(TowerImage, image('resources/sprites/building/tower1.gif')),
    new(StrongTowerImage, image('resources/sprites/building/strong_tower1.gif')),
    new(SeaImage, image('resources/sprites/sea/idle_dark_small_2.gif')),
    new(FreeImage, image('resources/sprites/grass/grass.gif')),
    new(RedImage, image('resources/sprites/grass/grass_red.gif')),
    new(BlueImage, image('resources/sprites/grass/grass_blue.gif')),
    new(YellowImage, image('resources/sprites/grass/grass_yellow.gif')),

    % parameters:
    MapSize = 7,
    HexSize = 100,
    TotalSize = MapSize* HexSize + HexSize,

    new(@window, picture('Antiyomacy')),
    send(@window, size, size(TotalSize, TotalSize)),
    (   between(0, MapSize, X),
        between(0, MapSize, Y),
        get_hex(Map, [X, Y], hex(_, _, Tile, Owner, Building, Unit)),
        GX is X * HexSize,
        GY is Y * HexSize,
        (   sea(Tile)
        ->  % Draw sea tile
            get(SeaImage, size, size(ImgWidth, ImgHeight)),
            CenterX is GX + (HexSize - ImgWidth) / 2,
            CenterY is GY + (HexSize - ImgHeight) / 2,
            send(@window, display, bitmap(SeaImage), point(CenterY, CenterX))
        ;   % Draw grass tile

            % Todo, use the following to draw the grass tiles, instead of box
            nth0(PlayerIndex, [red, blue, none], Owner), nth0(PlayerIndex, [RedImage, BlueImage, FreeImage], Image),
            get(Image, size, size(ImgWidth, ImgHeight)),
            CenterX is GX + (HexSize - ImgWidth) / 2,
            CenterY is GY + (HexSize - ImgHeight) / 2,
            new(GrassBitmap, bitmap(Image)),
            send(GrassBitmap, recogniser, click_gesture(left, '', single, message(@prolog, on_box_select, GrassBitmap))),
            send(@window, display, GrassBitmap, point(CenterY, CenterX))

            % nth0(PlayerIndex, [red, blue, none], Owner), nth0(PlayerIndex, [red, blue, grey], Color),
            % new(M, box(HexSize,HexSize)),
            % send(M, fill_pattern, colour(Color)),
            % send(M, recogniser, click_gesture(left, '', single, message(@prolog, on_box_select, M))),
            % send(@window, display, M, point(GY,GX))
        ), 

        % Draw the unit
        % send(@window, display, bitmap(Image),point(CenterX,CenterY)),
        (   Unit \= none -> 
            (   Unit = peasant -> Image = PeasantImage
            ;   Unit = spearman -> Image = SpearmanImage
            ;   Unit = baron -> Image = BaronImage
            ;   Unit = knight -> Image = KnightImage
            ),
            get(Image, size, size(ImgWidth, ImgHeight)),
            CenterX is GX + (HexSize - ImgWidth) / 2,
            CenterY is GY + (HexSize - ImgHeight) / 2,
            send(@window, display, bitmap(Image), point(CenterY, CenterX))
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
            send(@window, display, bitmap(Image), point(CenterY, CenterX ))
        ;   true
        ),

        fail ; true
    ),
    send(@window, open),
    true.
    

on_box_select(Box) :-
    % Get the position of the box
    get(Box, position, point(X, Y)),
    HexSize = 100,
    XCoord is Y // HexSize,
    YCoord is X // HexSize,
    map(Map),
    get_hex(Map, [XCoord, YCoord], hex(_, _, _, Owner, _, Unit)),
    % Check if there is a selected unit
    (   retract(selected_unit(SX, SY, SOwner, SBox)) ->
        % If there is a selected unit
        % Check if the selected box is of the same owner
        (   Owner = SOwner ->
            % TODO (qui devo aggiungere il predicato per spostare effettivamente l'unità)
            format('Unit moved from (~w, ~w) to (~w, ~w)~n', [SX, SY, XCoord, YCoord])
        ;   % If he destination box is not of the same owner
            format('The destination box is not of the same owner.~n', []),
            % Memorize the selected unit for a new attempt
            assertz(selected_unit(SX, SY, SOwner, SBox))
        ), 
        send(SBox, fill_pattern, colour(SOwner))
    ;   % If is the first click, memorize coordinates and owner of the selected unit
        (   Unit \= none ->  % Ensure there is a unit to select
        assertz(selected_unit(XCoord, YCoord, Owner, Box)),
            % Change the color of the selected box to yellow
            send(Box, fill_pattern, colour(yellow)),
            format('Unit selected at (~w, ~w) owned by ~w~n', [XCoord, YCoord, Owner])
        ;   format('No unit selected.~n', [])
        )
    ).


