# **Antiyomacy**
An implementation of a strategy game heavily inspired by Antiyoy and with some elements of Diplomacy. The project is focused on developing AI software that manages to play the game using the minimax algorithm.


<a name="index"></a>
## 📘 Index

* [Code structure](#code_structure)
* [Screenshots](#screenshots)

<a name="game_moves"></a>
## 🎲 Moves in the game
The two players, Red and Blue, take turns. In each player's turn, they can choose from the following three types of move:

| Move  | How many times it can be made |  
|---|---:|
|***Buy a unit*** | for each province, until its money runs out  |
|***Buy a building***   |  for each province, until its money runs out |
|***Displace a unit***   |  for each unit in each province | 

---
### Predicates used

**Buy a unit** : 
`province:buy_and_place/6` ---> (`economy:check_buy/3`, `unit:unit_placement/4`)

**Buy a building** : 
`province:buy_and_place/6` ---> (`economy:check_buy/3`, `building:building_placement/4`)

**Displace a unit** : 
`province:displace_unit/6` ---> `unit:unit_placement/3`

<a name="code_structure"></a>
## 📐 Code structure
The code is structured as follows, listing the main predicates for each file.
Note: The term "resource" refers to both units and buildings.

### Game core files

- `hex.pl`
    - **tile/1** : The list of tiles type
    - **owner/1** : The list of players
    - **hex/6**: The struct of an hex in the map

- `map.pl`
    - **generate_random_map/2** : Generates a random map using the Random Walkers algorithm
    - **inside_map/1** : Checks if a coordinate lies within the map boundaries
    - **get_hex/3** : Retrieves the hex at the given coordinate
    - **set_owner/4** : Change an hex owner
    - **set_building/4** : Spawns a building on a given coordinate
    - **set_unit/4** : Spawns a unit on a given coordinate
    - **destroy_units/3** : Destroy all units located on the specified hexes on the map. This is useful for handling bankruptcy cases
    - **spawn_provinces/2** : Randomly spawns a red and a blue province. This is used at the start of a game
- `province.pl`
    - **province/3** : The struct of a player's province
    - **province_count/3** : Checks or calculates the number of buildings or units owned by the province
    - **update_province/3** : Reload all the hexes in the old province hex list, remove conquered hexes from the enemy and add the hexes conquered by the player
    - **apply_income/4** : Add the income to the province money and go bankrupt if necessary
    - **near/3** : Search for adjacent hexes around the given one
    - **find_provinces/2** : Find all the provinces in the map
    - **outer_border/3** : Find all hexagons that border the given province externally
    - **inner_border/3** : Find all hexagons that border the given province internally
    - **buy_and_place/6** : Purchase a building or a unit and place it on the map at the given location
    - **displace_unit** : Displace a unit on a given valid hex

- `unit.pl`
    - **unit/5** : The list of the units that can be bought
    - **unit_placement/4** : Checks/Get a unit valid location on the given province. This is useful to list all the possible placement moves for a given unit (non-deterministic)

- `building.pl`
    - **building/4** : The list of the buildings that can be built
    - **building_cost/3** : Calculates the construction cost of a building based on its type
    - **tower_nearby/3** : Checks if there is an enemy tower nearby that prevents a unit move
    - **strong_tower_nearby/3** : Checks if there is an enemy strong tower nearby that prevents a unit move
    - **farm_nearby/4** : Checks if there is a farm nearby, useful to check where a farm can be placed
    - **building_placement/4** : Checks/Get a building valid location on the given province. This is useful to list all the possible placement moves for a given building (non-deterministic)

- `economy.pl`
    - **get_income/2** : Calculate a province income. This will be added to the province money at the end of the turn
    - **check_buys/3** : Checks if a list of resources can be bought and returns the remaining money of the province. This is useful to list all the possible purchase moves for a given province (non-deterministic)
    - **check_buy/3** : Checks whether a building or unit purchase can be achieved and returns the remaining money of the province. This is useful generate one possible purchase move for a given province (non-deterministic)
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