:- module(hex, [tile/1,
                sea/1,
                terrain/1,
                owner/1,
                hex/4,
                hex_tile/2,
                change_hex_tile/3,
                hex_owner/2,
                change_hex_owner/3,
               hex_coord/2]).

% Tiles
tile(X):-sea(X);terrain(X).
sea(ocean).
terrain(X):- member(X,[desert,wood,rock,sheep,clay,hay]).

% Owners
owner(X):-member(X,[none,red,blue]).

% Hex struct ====================================================================
hex(Index,[X,Y], Tile, Owner):- number(Index),X>=0,Y>=0, tile(Tile), owner(Owner).

% Check/Get hex tile type
hex_tile(hex(_,_,Tile,_),Checker):-call(Checker,Tile).

% Change an hex tile type
change_hex_tile(hex(Index,Coord,_,Owner),NewTile,NewHex):-NewHex=hex(Index,Coord,NewTile,Owner).

% Check/Get hex owner
hex_owner(hex(_,_, _, Owner), Owner).

% Change an hex owner
change_hex_owner(hex(Index,Coord,Tile,_),NewOwner,NewHex):-NewHex=hex(Index,Coord,Tile,NewOwner).

% Check/Get hex coordinates
hex_coord(hex(_,Coord, _, _), Coord).
