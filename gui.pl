:- module(gui, [init_gui/0, update_gui/0, current_province_index/1, human_provinces_to_play/1]).
:- use_module([map, hex, province, utils, building, unit, minimax, economy, game]).
:- use_module(library(pce)).
:- dynamic([selected_tile/1, selected_resource/1, current_province_index/1, human_provinces_to_play/1]).

tile_size(TileSize):-
    % map_size(MapSize),
    MapSize=8,
    get(@display, size, size(_ScreenWidth, ScreenHeight)),
    TruncatedHeight is (ScreenHeight//100)*100,
    TileSize is min(100, TruncatedHeight/(MapSize + 1)).
window_size([WindowWidth, WindowHeight]):- 
    % map_size(MapSize),
    MapSize=8,
    tile_size(TileSize),
    WindowWidth is (MapSize) * TileSize,
    ((input_mode(terminal_input); human_player(none); human_provinces_to_play([]))->HeightAdd =0; HeightAdd =1.25),
    WindowHeight is truncate((MapSize + HeightAdd) * TileSize).
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
    new(@small_peasant, image('resources/sprites/unit/small/peasant.gif')),
    new(@small_spearman, image('resources/sprites/unit/small/spearman.gif')),
    new(@small_baron, image('resources/sprites/unit/small/baron.gif')),
    new(@small_knight, image('resources/sprites/unit/small/knight.gif')),
    % Store the building images:
    new(@farm, image('resources/sprites/building/farm.gif')),
    new(@tower, image('resources/sprites/building/tower.gif')),
    new(@strong_tower, image('resources/sprites/building/strong_tower.gif')),
    new(@small_farm, image('resources/sprites/building/small/farm.gif')),
    new(@small_tower, image('resources/sprites/building/small/tower.gif')),
    new(@small_strong_tower, image('resources/sprites/building/small/strong_tower.gif')),
    % Store the tile images:
    new(@tile_sea, image('resources/sprites/sea/idle.gif')),
    new(@tile_free, image('resources/sprites/grass/grass.gif')),
    new(@tile_red, image('resources/sprites/grass/grass_red.gif')),
    new(@tile_blue, image('resources/sprites/grass/grass_blue.gif')),
    new(@tile_current_province, image('resources/sprites/grass/grass_yellow.gif')),
    new(@tile_selected, image('resources/sprites/grass/grass_yellow_light.gif')),
    % Store the shop menu images:
    new(@frame, image('resources/sprites/frame/frame.gif')),
    new(@frame_no_buy, image('resources/sprites/frame/frame_no_buy.gif')),
    new(@frame_selected, image('resources/sprites/frame/frame_selected.gif')),
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
    send(@window, foreground, white),
    % send(@window, kind, popup),
    send(@window, open).


% Retrive the stored map and redraw the window accordingly
update_gui:-
    send(@window, clear),
    send(@window, flush),
    last_board(board(Map, _Provinces, Player, _, _)), map_size(MapSize), % Get
    ScaleFactor is 8/MapSize,
    window_size([WindowWidth, WindowHeight]), % Get
    send(@window, size, size(WindowWidth, WindowHeight)),
    % Get the current playing user province if any
    human_provinces(_HumanProvinces, HumanProvince),
    (   var(HumanProvince), CurrentCoords = [], !
    ;   province_hexes(HumanProvince, HumanProvinceHexes), % Get
        maplist(hex_coord, HumanProvinceHexes, CurrentCoords)
    ),

    % Draw map grid =========================================================
    (   % Check if there is any selected hex
        (   selected_tile(SelectedHex)
        ->  hex_coord(SelectedHex, SelectedCoord)
        ;   SelectedCoord = @null
        ),
        % Loop through the map cells  ---------------------------
        between(0, MapSize, Y), between(0, MapSize, X),
        get_hex(Map, [Y, X], Hex),
        Hex = hex(_Index, _Coord, Tile, Owner, Building, Unit),
        
        % Choose the tile sprite
        (   Tile == sea
        ->  TileImage = @tile_sea,
            [OnClick, Cursor] = [@null, @null]
        ;   % A resource is selected in the purchase menu
            selected_resource(ResName), \+ var(HumanProvince)
        ->  (   (building_placement(Map, HumanProvince, ResName, Hex); unit_placement(Map, HumanProvince, ResName, Hex, _))
            ->  TileImage = @tile_selected,
                [OnClick, Cursor] = [on_tile_click(X,Y), hand1]
            ;   nth0(PlayerIndex, [red, blue, none], Owner), 
                nth0(PlayerIndex, [@tile_red, @tile_blue, @tile_free], TileImage),
                [OnClick, Cursor] = [@null, @null]
            )
        ;   % A unit is selected
            selected_tile(SelectedHex), \+ var(HumanProvince), hex_unit(SelectedHex, UnitName)
        ->  (   unit_placement(Map, HumanProvince, UnitName, Hex, _),     
                manhattan_distance(SelectedHex, Hex, ManhattanDistance),
                max_displacement_distance(MaxDisplacementDistance),
                ManhattanDistance =< MaxDisplacementDistance
            ->  TileImage = @tile_selected,
                [OnClick, Cursor] = [on_tile_click(X,Y), hand1]
            ;   nth0(PlayerIndex, [red, blue, none], Owner), 
                nth0(PlayerIndex, [@tile_red, @tile_blue, @tile_free], TileImage),
                [OnClick, Cursor] = [@null, @null]
            )
        ;   % No resource is selected in the purchase menu
            SelectedCoord == [Y, X]
        ->  TileImage = @tile_selected,
            [OnClick, Cursor] = [@null, @null]
        ;   member([Y, X], CurrentCoords), input_mode(gui_input), human_player(Player)
        ->  TileImage = @tile_current_province,
            (Owner = Player, Unit \= none -> [OnClick, Cursor] = [on_tile_click(X,Y), hand1]; [OnClick, Cursor] = [@null, @null])
        ;   nth0(PlayerIndex, [red, blue, none], Owner), 
            nth0(PlayerIndex, [@tile_red, @tile_blue, @tile_free], TileImage),
            [OnClick, Cursor] = [@null, @null]
        ),
        display_image(TileImage, [X, Y], OnClick, Cursor, ScaleFactor, 1),
        % Draw the unit if present
        (   Unit == none
        ->  true
        ;   nth0(UnitIndex, [peasant, spearman, baron, knight], Unit),
            (   ScaleFactor>0.75
            ->  nth0(UnitIndex, [@peasant, @spearman, @baron, @knight], UnitImage)
            ;   nth0(UnitIndex, [@small_peasant, @small_spearman, @small_baron, @small_knight], UnitImage)
            ),
            display_image(UnitImage, [X, Y], @null, @null, ScaleFactor, ScaleFactor)
        ),
        % Draw the building if present
        (   Building == none
        ->  true
        ;   nth0(BuildingIndex, [farm, tower, strong_tower], Building), 
            (   ScaleFactor>0.75
            ->  nth0(BuildingIndex, [@farm, @tower, @strong_tower], BuildingImage)
            ;   nth0(BuildingIndex, [@small_farm, @small_tower, @small_strong_tower], BuildingImage)
            ),
            display_image(BuildingImage, [X, Y], @null, @null, ScaleFactor, ScaleFactor)
        ),
        fail ; true
    ),
    % Draw the bottom menu ==================================================
    (   (input_mode(terminal_input); human_player(none); \+ human_player(Player); var(HumanProvince))
    ;   human_provinces(_, HumanProvince),
        province_money(HumanProvince, HumanProvinceMoney),
        Frames = [
            peasant-(@peasant),
            spearman-(@spearman),
            baron-(@baron),
            knight-(@knight),
            farm-(@farm),
            tower-(@tower),
            strong_tower-(@strong_tower)
        ],
        % Create and display unit icons
        (   nth0(Index, Frames, ResourceName-ResourceImage),
            LabelY is 8 + 1,
            (building_cost(ResourceName, HumanProvince, Cost); unit(ResourceName, _, _, Cost, _)),
            (   selected_resource(ResourceName)
            ->  [FrameImage, OnClick, Cursor] = [@frame_selected, on_frame_click(ResourceName), hand1]
            ;   HumanProvinceMoney >= Cost 
            ->  [FrameImage, OnClick, Cursor] = [@frame,on_frame_click(ResourceName), hand1]
            ;   [FrameImage, OnClick, Cursor] = [@frame_no_buy, @null, @null]
            ),
            display_image(FrameImage, [Index, 8], @null, @null, 1, 1),
            display_image(ResourceImage, [Index, 8], OnClick, Cursor, 1, 1),
            display_label(string(' %s$', Cost), [Index, LabelY]),
            fail; true
        ),
        % Display skip turn icon
        SkipTurnX is 8 - 1,
        display_image(@skip_turn, [SkipTurnX, 8], on_frame_click(skip_turn), hand1, 1, 1),
        display_label(string('[%s$]', HumanProvinceMoney), [SkipTurnX, 9])
        % TODO here: show current province money
    ),
    send(@window, flush).

% Display a given image on a given coordinate and assign a left click callback if requested
% display_image(+Image, +Coord, +LeftClickMessage, +Cursor, +Scale, +InnerScale)
display_image(Image, [X, Y], LeftClickMessage, Cursor, Scale, InnerScale) :-
    tile_size(TileSize),
    get(Image, size, size(Width, Height)),
    PosX is (X * TileSize* Scale) + (TileSize* InnerScale - Width ) / 2,
    PosY is (Y * TileSize* Scale) + (TileSize* InnerScale - Height) / 2,
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

% Note: this is called only by tiles that are part of the current human player playing province
on_tile_click(_,_):- input_mode(terminal_input), !.
on_tile_click(X, Y) :-
    last_board(board(Map, Provinces, Player, _State, [RedConq, BlueConq])), % Get
    human_provinces(_HumanProvinces, HumanProvince),
    get_hex(Map, [Y, X], Hex),
    % Check if the user has selected a resource to buy
    (   retract(selected_resource(ResName))
    ->  % Purchase move
        format('Placed ~w at (~w, ~w).~n', [ResName, X, Y]),
        check_buy(HumanProvince, ResName, _),
        % Prevent a unit merge on purchase
        \+ (hex_owner(Hex, Player), \+ hex_unit(Hex, none)),
        % Check if the placement is valid based on the resource type
        (   building(ResName, _, _, _)
        ->  building_placement(Map, HumanProvince, ResName, Hex)
        ;   unit_placement(Map, HumanProvince, ResName, Hex, _)
        ), !,
        buy_and_place(Map, Provinces, HumanProvince, ResName, Hex, NewMap, NewProvinces, _)

    ;   % Displace move
        format('Tile ~w selected.~n', [[X, Y]]),
        (   retract(selected_tile(HexFrom))
        ->  HexFrom \== Hex,
            hex_unit(HexFrom, UnitName),
            unit_placement(Map, HumanProvince, UnitName, Hex, NewUnitName), !,
            displace_unit(Map, Provinces, HumanProvince, HexFrom, Hex, NewUnitName, NewMap, NewProvinces, _)
        ;   assertz(selected_tile(Hex))
        )
    ), 
    % If a move has been successfully made, update the new board and calculate the new conquests
    (   selected_tile(_), !, update_gui
    ;   (   other_player(Player, Enemy), % Get
            hex_owner(Hex, Enemy)
        ->  (   Player == red
            ->  NewRedConq is RedConq + 1,
                NewBlueConq = BlueConq
            ;   NewRedConq = RedConq,
                NewBlueConq is BlueConq + 1
            )
        ;   [NewRedConq, NewBlueConq] = [RedConq, BlueConq]
        ),
        end_province_move(NewMap, NewProvinces, [NewRedConq, NewBlueConq])
    ), !.
on_tile_click(_, _) :- update_gui.

% Note: this is called only on purchaseable resources
on_frame_click(_):- input_mode(terminal_input), !.
on_frame_click(ResourceName):-
    retractall(selected_tile(_)),
    (   ResourceName \= skip_turn, !, 
        % Deselect the resource if it was already selected
        (selected_resource(ResourceName), retractall(selected_resource(_)); retractall(selected_resource(_)), assertz(selected_resource(ResourceName))),
        update_gui
    ;   % Skip turn
        last_board(board(Map, Provinces, _Player, _State, Conquests)),
        end_province_move(Map, Provinces, Conquests)
    ).
% Returns the list of the human player provinces and its currently playing province, if any
human_provinces(_, _):- human_player(none), !.
human_provinces(HumanProvinces, HumanProvince):-
    (human_provinces_to_play(HumanProvinces),!; HumanProvinces=[]),
    (   current_province_index(HumanIndex), HumanProvinces\==[]
    ->  nth0(HumanIndex, HumanProvinces, HumanProvince)
    ;   true
    ).

% Move the input to the next human province if any, otherwise make a game step
end_province_move(NewMap, NewProvinces, NewConquests):-
    last_board(board(_Map, _Provinces, Player, State, _Conquests)),
    human_provinces(HumanProvinces, _),
    length(HumanProvinces, HumanProvncesCount),
    current_province_index(CurrentProvinceIndex),
    retractall(current_province_index(_)),
    retractall(last_board(_)),
    (   CurrentProvinceIndex is HumanProvncesCount - 1
    ->  % That was the last human province to move, now it is the enemy's turn
        other_player(Player, NewPlayer),
        % Check if the human player has won
        (   has_won(NewMap, NewProvinces, Player)
        ->  NewState = win
        ;   NewState = play
        ),
        assertz(last_board(board(NewMap, NewProvinces, NewPlayer, NewState, NewConquests))),
        update_gui,
        game_step
    ;   % There are other human provinces that need to move
        NewIndex is CurrentProvinceIndex + 1,
        assertz(current_province_index(NewIndex)),
        assertz(last_board(board(NewMap, NewProvinces, Player, State, NewConquests))),
        update_gui
    ).