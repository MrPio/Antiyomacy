:- module(gui, [gui/0]).
:- use_module([map, hex, province, utils]).
:- use_module(library(pce)).
:- dynamic([selected_hex/1, selected_unit/4]).


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
    init_gui,
    
    % send(@window, kind, popup),
    gui.

init_gui:-
    free(@window),
    new(@window, picture('Antiyomacy')).

gui:-
    map(Map),
    send(@window, clear),
    % resources:
    (   new(PeasantImage, image('resources/sprites/unit/peasant.gif')),
        new(SpearmanImage, image('resources/sprites/unit/spearman.gif')),
        new(BaronImage, image('resources/sprites/unit/baron.gif')),
        new(KnightImage, image('resources/sprites/unit/knight.gif')),
        new(FarmImage, image('resources/sprites/building/farm.gif')),
        new(TowerImage, image('resources/sprites/building/tower.gif')),
        new(StrongTowerImage, image('resources/sprites/building/strong_tower.gif')),
        new(SeaImage, image('resources/sprites/sea/idle.gif')),
        new(FreeImage, image('resources/sprites/grass/grass.gif')),
        new(RedImage, image('resources/sprites/grass/grass_red.gif')),
        new(BlueImage, image('resources/sprites/grass/grass_blue.gif')),
        new(YellowImage, image('resources/sprites/grass/grass_yellow.gif'))
    ),

    % parameters:
    MapSize = 7,
    HexSize = 100,
    TotalSize = MapSize* HexSize + HexSize,

    send(@window, size, size(TotalSize, TotalSize)),
    (   % Check if there is any selected hex
        (   retract(selected_hex(SelectedHex))
        ->  hex_coord(SelectedHex, SelectedCoord)
        ;   true
        ),
        between(0, MapSize, X),
        between(0, MapSize, Y),
        get_hex(Map, [X, Y], Hex),
        hex_tile(Hex, Tile), % Get
        hex_owner(Hex, Owner), % Get
        hex_unit(Hex, Unit), % Get
        hex_building(Hex, Building), % Get
        GX is X * HexSize,
        GY is Y * HexSize,
        
        % Draw the tile
        (   Tile == sea
        ->  TileImage=SeaImage
        ;   SelectedCoord == [X,Y]
        ->  TileImage=YellowImage
        ;   nth0(PlayerIndex, [red, blue, none], Owner), 
            nth0(PlayerIndex, [RedImage, BlueImage, FreeImage], TileImage)
        ), 
        get(TileImage, size, size(TileWidth, TileHeight)),
        TileX is GX + (HexSize - TileWidth) / 2,
        TileY is GY + (HexSize - TileHeight) / 2,
        new(GrassBitmap, bitmap(TileImage)),
        send(GrassBitmap, recogniser, click_gesture(left, '', single, message(@prolog, on_hex_select, X, Y))),
        send(@window, display, GrassBitmap, point(TileY, TileX)),
        % Draw the unit if present
        (   Unit == none
        ->  true
        ;   nth0(UnitIndex, [peasant, spearman, baron, knight], Unit), 
            nth0(UnitIndex, [PeasantImage, SpearmanImage, BaronImage, KnightImage], UnitImage),
            get(UnitImage, size, size(UnitWidth, UnitHeight)),
            UnitX is GX + (HexSize - UnitWidth) / 2,
            UnitY is GY + (HexSize - UnitHeight) / 2,
            send(@window, display, bitmap(UnitImage), point(UnitY, UnitX))
        ),

        % Draw the building if present
        (   Building == none
        ->  true
        ;   nth0(BuildingIndex, [farm, tower, strong_tower], Building), 
            nth0(BuildingIndex, [FarmImage, TowerImage, StrongTowerImage], BuildingImage),
            get(BuildingImage, size, size(BuildingWidth, BuildingHeight)),
            BuildingX is GX + (HexSize - BuildingWidth) / 2,
            BuildingY is GY + (HexSize - BuildingHeight) / 2,
            send(@window, display, bitmap(BuildingImage), point(BuildingY, BuildingX))
        ),

        fail ; true
    ),
    send(@window, open),
    MapHeight = 7 * 100,
    create_purchase_menu(MapHeight, 100),
    true.


% Predicate to create the purchase menu
create_purchase_menu(MapHeight, HexSize):-
    MenuHeight = 100, % Height of the menu
    TotalWidth = 7 * HexSize, % map of 7 hexagons width
    StartY is MapHeight + HexSize / 2, % Position below the map
    IconSpacing = 100, % Spacing between icons
    IconSize = 100, % Standard size for icons in the menu

    % List of units with respective actions
    UnitActions = [
        peasant-'/resources/sprites/unit/peasant.gif'-buy_peasant,
        spearman-'/resources/sprites/unit/spearman.gif'-buy_spearman,
        knight-'/resources/sprites/unit/knight.gif'-buy_knight,
        baron-'/resources/sprites/unit/baron.gif'-buy_baron
    ],

    % List of buildings with respective actions
    BuildingActions = [
        farm-'/resources/sprites/building/farm.gif'-buy_farm,
        tower-'/resources/sprites/building/tower.gif'-buy_tower,
        strongtower-'/resources/sprites/building/strong_tower.gif'-buy_strongtower,
        castle-'/resources/sprites/building/castle.gif'-buy_castle
    ],

    % Create and display unit icons
    findall(_, (
        nth1(Index, UnitActions, Action-ImagePath-Callback),
        X is (Index - 1) * IconSpacing,
        create_icon(@window, ImagePath, Callback, X, StartY, IconSize)
    ), _),

    % Create and display building icons
    length(UnitActions, UnitCount),
    findall(_, (
        nth1(Index, BuildingActions, Action-ImagePath-Callback),
        X is (UnitCount + Index - 1) * IconSpacing,
        create_icon(@window, ImagePath, Callback, X, StartY, IconSize)
    ), _).


% Predicate to create an icon
create_icon(Window, ImagePath, Action, BaseX, BaseY, IconSize):-
    new(Icon, bitmap(ImagePath)),
    get(Icon, size, size(ImgWidth, ImgHeight)),
    % Calculate center based on icon size
    CenterX is BaseX + (IconSize - ImgWidth) / 2,
    CenterY is BaseY + (IconSize - ImgHeight) / 2,
    send(Window, display, Icon, point(CenterX, CenterY)),
    send(Icon, recogniser,
         click_gesture(left, '', single,
                       message(@prolog, Action))).


skip_turn_action:-
    % Implementa l'azione desiderata per "Skip Turn"
    format('Turn Skipped...').

% Actions for buy units TODO
buy_peasant:- format('Peasant bought.~n').
buy_spearman:- format('Spearman bought.~n').
buy_knight:- format('Knight bought.~n').
buy_baron:- format('Baron bought.~n').

% Actions for buy buildings TODO
buy_farm:- format('Farm bought.~n').
buy_tower:- format('Tower bought.~n').
buy_strongtower:- format('Strong tower bought.~n').
buy_castle:- format('Castle bought.~n').

    
on_hex_select(X,Y) :-
    map(Map),
    get_hex(Map, [X,Y], Hex),
    assert(selected_hex(Hex)),
    gui.

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


