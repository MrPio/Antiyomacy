:- module(gui, [update_gui/0]).
:- use_module([map, hex, province, utils]).
:- use_module(library(pce)).
:- dynamic([selected_item/1, selected_tile/1]).

tile_size(TileSize):-
    map_size(MapSize),
    get(@display, size, size(_ScreenWidth, ScreenHeight)),
    TruncatedHeight is (ScreenHeight//100)*100,
    TileSize is min(100, TruncatedHeight/(MapSize + 1)).
window_size([WindowWidth, WindowHeight]):- 
    map_size(MapSize),
    tile_size(TileSize),
    WindowWidth is (MapSize) * TileSize,
    WindowHeight is (MapSize + 1) * TileSize.
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
    % update_gui initialization:
    init_gui,
    update_gui.

% Initialize the gui by instantiating the needed resources
init_gui:-
    % Store the unit images:
    new(@peasant, image('resources/sprites/unit/peasant.gif')),
    new(@spearman, image('resources/sprites/unit/spearman.gif')),
    new(@baron, image('resources/sprites/unit/baron.gif')),
    new(@knight, image('resources/sprites/unit/knight.gif')),
    % Store the building images:
    new(@farm, image('resources/sprites/building/farm.gif')),
    new(@tower, image('resources/sprites/building/tower.gif')),
    new(@strong_tower, image('resources/sprites/building/strong_tower.gif')),
    % Store the tile images:
    new(@tile_sea, image('resources/sprites/sea/idle.gif')),
    new(@tile_free, image('resources/sprites/grass/grass.gif')),
    new(@tile_red, image('resources/sprites/grass/grass_red.gif')),
    new(@tile_blue, image('resources/sprites/grass/grass_blue.gif')),
    new(@tile_selected, image('resources/sprites/grass/grass_yellow.gif')),
    % Store the shop menu images:
    new(@frame_peasant, image('resources/sprites/frame/peasant.gif')),
    new(@frame_spearman, image('resources/sprites/frame/spearman.gif')),
    new(@frame_baron, image('resources/sprites/frame/baron.gif')),
    new(@frame_knight, image('resources/sprites/frame/knight.gif')),
    new(@frame_farm, image('resources/sprites/frame/farm.gif')),
    new(@frame_tower, image('resources/sprites/frame/tower.gif')),
    new(@frame_strong_tower, image('resources/sprites/frame/strong_tower.gif')),
    new(@skip_turn, image('resources/sprites/skip/skipturn_white.gif')),
    % Create the window
    free(@window),
    new(@window, window('Antiyomacy')),
    send(@window, background, black)
    % send(@window, kind, popup).
    .


% Retrive the stored map and redraw the window accordingly
update_gui:-
    send(@window, clear),
    map(Map), map_size(MapSize), % Get
    window_size([WindowWidth, WindowHeight]), % Get
    send(@window, size, size(WindowWidth, WindowHeight)),
    % Draw map grid =========================================================
    (   % Check if there is any selected hex
        (   retract(selected_tile(SelectedHex))
        ->  hex_coord(SelectedHex, SelectedCoord)
        ;   true
        ),
        % Loop through the map cells  ---------------------------
        between(0, MapSize, Y), between(0, MapSize, X),
        get_hex(Map, [X, Y], hex(_Index, _Coord, Tile, Owner, Building, Unit)),
        
        % Draw the tile
        (   Tile == sea
        ->  TileImage = @tile_sea
        ;   SelectedCoord == [X,Y]
        ->  TileImage = @tile_selected
        ;   nth0(PlayerIndex, [red, blue, none], Owner), 
            nth0(PlayerIndex, [@tile_red, @tile_blue, @tile_free], TileImage)
        ),
        display_image(TileImage, [X, Y], on_tile_click(X,Y)),
        % Draw the unit if present
        (   Unit == none
        ->  true
        ;   nth0(UnitIndex, [peasant, spearman, baron, knight], Unit), 
            nth0(UnitIndex, [@peasant, @spearman, @baron, @knight], UnitImage),
            display_image(UnitImage, [X, Y], @null)
        ),
        % Draw the building if present
        (   Building == none
        ->  true
        ;   nth0(BuildingIndex, [farm, tower, strong_tower], Building), 
            nth0(BuildingIndex, [@farm, @tower, @strong_tower], BuildingImage),
            display_image(BuildingImage, [X, Y], @null)
        ),
        fail ; true
    ),
    % Draw the bottom menu ==================================================
    (    Frames = [
            peasant-(@frame_peasant),
            spearman-(@frame_spearman),
            knight-(@frame_knight),
            baron-(@frame_baron),
            farm-(@frame_farm),
            tower-(@frame_tower),
            strong_tower-(@frame_strong_tower),
            skipturn-(@skip_turn)
        ],
        % Create and display unit icons
        (   nth0(Index, Frames, _Resource-FrameImage),
            display_image(FrameImage, [Index, MapSize], @null),
            fail; true
        )
    ),
    send(@window, open).

% Display a given image on a given coordinate and assign a left click callback if requested
% display_image(+Image, +Coord, +LeftClickMessage)
display_image(Image, [X, Y], LeftClickMessage) :-
    tile_size(TileSize),
    get(Image, size, size(Width, Height)),
    PosX is (X * TileSize) + (TileSize - Width ) / 2,
    PosY is (Y * TileSize) + (TileSize - Height) / 2,
    new(Bitmap, bitmap(Image)),
    (   LeftClickMessage == @null, !
    ;   LeftClickMessage =.. [Functor| Args],
        MessageCallback =.. [message, @prolog, Functor | Args],
        send(Bitmap, recogniser, click_gesture(left, '', single, MessageCallback))
    ),
    send(@window, display, Bitmap, point(PosX, PosY)).

on_tile_click(X, Y) :-
    map(Map),
    get_hex(Map, [X, Y], Hex),
    % Check if the user has selected a unit or building to buy
    (   retract(selected_item(Type))
    ->  % If yes, try to perform a purchase move
        % TODO buy and place
        retractall(selected_item),
        format('Placed ~w at (~w, ~w).~n', [Type, X, Y])
    ;   % If not, try to perform a displace move
        format('Tile ~w selected.~n', [[X, Y]]),
        assertz(selected_tile(Hex)),
        update_gui
    ).