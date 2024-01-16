# **Antiyomacy**
An implementation of a strategy game heavily inspired by Antiyoy and with some elements of Diplomacy. The project is focused on developing AI software that manages to play the game using the minimax algorithm.


<a name="index"></a>
## 📘 Index

* [Code structure](#code_structure)
* [Screenshots](#screenshots)


<a name="code_structure"></a>
## 📐 Code structure
The code is structured as follows, listing the main predicates for each file:

### Game core files

- `hex.pl`
    - **tile/1** : The list of tiles type
    - **owner/1** : The list of players
    - **hex/6**: The struct of an hex in the map

- `map.pl`
    - **generate_random_map/1** : Generates a random map using the Random Walkers algorithm
    - **inside_map/1** : Checks if a coordinate lies within the map boundaries
    - **get_hex/3** : Retrieves the hex at the given coordinate
    - **set_owner/2** : Change an hex owner
    - **set_building/2** : Spawns a building on a given coordinate
    - **set_unit/** : Spawns a unit on a given coordinate

- `province.pl`
    - **province/3** : The struct of a player's province
    - **boundary/3** : Search for adjacent hexes around the given one
    - **find_provinces/2** : Find all the provinces in the map
    - **outer_border/3** : Find all hexagons that border the given province externally
    - **inner_border/3** : Find all hexagons that border the given province internally
    - **units_location/3** : Check/Get a unit possible location on the given province (non-deterministic)
    - **buildings_location/3** : Check/Get a building possible location on the given province (non-deterministic)
    - **buy_and_place/6** : Purchase a building or a unit and place it on the map at the given location
    - **displace_unit** : Displace a unit on a given valid hex

- `unit.pl`
    - **unit/5** : The list of the units that can be bought
    - **unit_placement/4** : Checks/Get a unit valid location on the given province. This is useful to list all the possible placement moves for a given unit

- `building.pl`
    - **building/4** : The list of the buildings that can be built
    - **tower_nearby/3** : Checks if there is an enemy tower nearby that prevents a unit move
    - **building_placement/3** : Checks/Get a building valid location on the given province. This is useful to list all the possible placement moves for a given building

- `economy.pl`
    - **get_income/2** : Calculate a province income. This will be added to the province money at the end of the turn
    - **check_buy/3** : Checks whether a building or unit purchase can be achieved and returns the returns the province's remaining money
    - **check_buy_comb**
---
### Other files

- `printer.pl`
    - **print_map/1** : Print a map row with lateral coordinates

- `game.pl`
    - **test/0** : Test various game predicates

<a name="screenshots"></a>
## 🖼 Screenshots

<img src="https://github.com/MrPio/Antiyomacy/assets/22773005/158ca404-f159-4d62-9689-b9e68fd537fa" width="250rem">
<img src="img/screen1.png" width="250rem">