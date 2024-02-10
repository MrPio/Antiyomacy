:- module(gui, [init_gui/0, update_gui/0]).
:- use_module([map, hex, province, utils, building, unit, minimax]).
:- use_module(library(pce)).
:- dynamic([selected_tile/1, selected_resource/1, current_province_index/1]).

tile_size(TileSize):-
    map_size(MapSize),
    get(@display, size, size(_ScreenWidth, ScreenHeight)),
    TruncatedHeight is (ScreenHeight//100)*100,
    TileSize is min(100, TruncatedHeight/(MapSize + 1)).
window_size([WindowWidth, WindowHeight]):- 
    map_size(MapSize),
    tile_size(TileSize),
    WindowWidth is (MapSize) * TileSize,
    WindowHeight is truncate((MapSize + 1.25) * TileSize).
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
    new(@tile_current_province, image('resources/sprites/grass/grass_yellow.gif')),
    new(@tile_selected, image('resources/sprites/grass/grass_yellow_light.gif')),
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
    send(@window, background, black),
    send(@window, foreground, white)
    % send(@window, kind, popup).
    .


% Retrive the stored map and redraw the window accordingly
update_gui:-
    send(@window, clear),
    last_board(board(Map, Provinces, _Player, _, _)), map_size(MapSize), % Get
    window_size([WindowWidth, WindowHeight]), % Get
    send(@window, size, size(WindowWidth, WindowHeight)),
    % Get the current playing user province if any
    human_provinces(_HumanProvinces, HumanProvince),
    (   var(HumanProvince), CurrentCoord = [], !
    ;   province_hexes(HumanProvince, HumanProvinceHexes), % Get
        maplist(hex_coord, HumanProvinceHexes, CurrentCoord)
    )
    
    % Draw map grid =========================================================
    (   % Check if there is any selected hex
        (   retract(selected_tile(SelectedHex))
        ->  hex_coord(SelectedHex, SelectedCoord)
        ;   true
        ),
        % Loop through the map cells  ---------------------------
        between(0, MapSize, Y), between(0, MapSize, X),
        get_hex(Map, [Y, X], hex(_Index, _Coord, Tile, Owner, Building, Unit)),
        
        % Draw the tile
        (   Tile == sea
        ->  TileImage = @tile_sea,
            [OnClick, Cursor] = [@null, @null]
        ;   SelectedCoord == [X,Y]
        ->  TileImage = @tile_selected,
            [OnClick, Cursor] = [@null, @null]
        ;   member(SelectedCoord, CurrentCoord)
        ->  TileImage = @tile_current_province,
            [OnClick, Cursor] = [on_tile_click(X,Y), hand1]
        ;   nth0(PlayerIndex, [red, blue, none], Owner), 
            nth0(PlayerIndex, [@tile_red, @tile_blue, @tile_free], TileImage),
            (selected_tile(_) -> [OnClick, Cursor] = [on_tile_click(X,Y), hand1]; [OnClick, Cursor] = [@null, @null])
        ),
        display_image(TileImage, [X, Y], OnClick, Cursor),
        % Draw the unit if present
        (   Unit == none
        ->  true
        ;   nth0(UnitIndex, [peasant, spearman, baron, knight], Unit), 
            nth0(UnitIndex, [@peasant, @spearman, @baron, @knight], UnitImage),
            display_image(UnitImage, [X, Y], @null, @null)
        ),
        % Draw the building if present
        (   Building == none
        ->  true
        ;   nth0(BuildingIndex, [farm, tower, strong_tower], Building), 
            nth0(BuildingIndex, [@farm, @tower, @strong_tower], BuildingImage),
            display_image(BuildingImage, [X, Y], @null, @null)
        ),
        fail ; true
    ),
    % Draw the bottom menu ==================================================
    (   human_player(none)
    ;   Frames = [
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
        (   nth0(Index, Frames, ResourceName-FrameImage),
            display_image(FrameImage, [Index, MapSize], on_frame_click(ResourceName), hand1),
            LabelY is MapSize + 1,
            % TODO here: province needed
            % (   building_cost(ResourceName, Cost),
            %     display_label(string(' %s$', Cost), [Index, LabelY])
            % ;   unit_cost(ResourceName, Cost),
            %     display_label(string(' %s$', Cost), [Index, LabelY])
            % ; true
            % ),
            fail; true
        )
    ),
    send(@window, open).

% Display a given image on a given coordinate and assign a left click callback if requested
% display_image(+Image, +Coord, +LeftClickMessage, +Cursor)
display_image(Image, [X, Y], LeftClickMessage, Cursor) :-
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
    (   Cursor == @null, !
    ;   send(Bitmap, cursor, Cursor)
    ),
    send(@window, display, Bitmap, point(PosX, PosY)).

% Display a label at a given coordinate
% display_image(+Label, +Coord)
display_label(Text, [X, Y]) :-
    tile_size(TileSize),
    PosX is (X * TileSize),
    PosY is (Y * TileSize),
    send(@window, display, text(Text, center, @courier_bold_18), point(PosX, PosY)).

% Note: this is called only tiles that are part of the current human player playing province
on_tile_click(X, Y) :-
    last_board(board(Map, Provinces, _Player, _, _)), % Get
    human_provinces(_HumanProvinces, HumanProvince),
    get_hex(Map, [X, Y], Hex),
    % Check if the user has selected a resource to buy
    (   retract(selected_resource(Type))
    ->  % Purchase move
        format('Placed ~w at (~w, ~w).~n', [Type, X, Y])
        % TODO here, like ask_province_move
    ;   % Displace move
        format('Tile ~w selected.~n', [[X, Y]]),
        (   retract(selected_tile(HexFrom))
        ->  get_hex(Map, [X, Y], DestHex),
            % TODO here, like io.pl, call unit_placement to check for validity
            displace_unit(Map, Provinces, Province, HexFrom, DestHex, NewUnitName, NewMap, NewProvinces, _)
        ;   assert(selected_tile(Hex))
        )
    ), update_gui.

% TODO prima di chiamare il step, fai retract di current_province_index(_)


on_frame_click(ResourceName):-
    assert(selected_resource(ResourceName)),
    last_board(board(Map, Provinces, Player, State, Conquests)), % Get
    human_player(HumanPlayer),
    include([In]>>(province_owner(In, HumanPlayer)), Provinces, HumanProvinces),
    other_player(Player, Enemy),
    (   unit(ResourceName, _, _, _, _)
    ->  true
    ;   building(ResourceName, _, _, _)
    ->  true
    ;   % Skip turn
        true
    ).
% Returns the list of the human player provinces and its currently playing province, if any
human_provinces(HumanProvinces, HumanProvince):-
    last_board(Board),
    board_provinces(Board, Provinces), % Get
    human_player(HumanPlayer),
    include([In]>>(province_owner(In, HumanPlayer)), Provinces, HumanProvinces),
    (   current_province_index(HumanIndex), !
    ;   nth0(HumanIndex, HumanProvinces, HumanProvince)
    )