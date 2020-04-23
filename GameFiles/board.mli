(********************************************************************
   Representation of a static board game. 

   This module represents the data stored in the game files, including
   the tiles, dice, snakes and ladders.  It handles loading of that data 
   from JSON as well as querying the data.
 ********************************************************************)

(** The abstract type of values representing a game board. *)
type t

(** The type of tile identifier. *)
type tile_id = string

(** Raised when an unknown tile is encountered. *)
exception UnknownTile of tile_id

(** The type of die identifier. *)
type dice_id = string

(** Raised when an unknown die is encountered. *)
exception UnknownDice of dice_id

(** The type of snake identifier. *)
type snake_id = string

(** Raised when an unknown die is encountered. *)
exception UnknownSnake of snake_id

(** The type of ladder identifier. *)
type ladder_id = string

(** Raised when an unknown ladder is encountered. *)
exception UnknownLadder of ladder_id

(** The type of face value of a die. *)
type face = int

(** The type of probability value of a face of a die. Values of this type
    must be between 0 and 1 (both inclusive). *)
type prob = float

(** [from_json json] is the adventure that [json] represents.
    Requires: [json] is a valid JSON adventure representation. *)
val from_json : Yojson.Basic.t -> t

(** [tile_ids brd] is the list of tile identifiers in [brd]. The identifiers 
    of the tiles are arranged in the increasing order of the integer 
    representation of the identifiers in [brd].
    Requires: [brd] is a a tree representing a valid game board. *)
val tile_ids : t -> tile_id list

(** [start_tile brd] is the starting tile of [brd]. 
    Requires: [brd] is a a tree representing a valid game board. *)
val start_tile : t -> tile_id

(** [win_tile brd] is the winning tile of [brd] at the end of the game. 
    Requires: [brd] is a a tree representing a valid game board. *)
val win_tile : t -> tile_id

(** [dice_ids brd] is the list of dice identifiers in [brd]. 
    Requires: [brd] is a a tree representing a valid game board. *)
val dice_ids : t -> dice_id list

(** [die_vals brd d] is an association list where the faces of the die are
    the keys in the list while the corresponding probabilities of the face being 
    rolled are values associated with the keys. 
    Requires: [brd] is a a tree representing a valid game board. 
    Raises: [UnknownDice d] if [d] is not the identifier of a die in [brd].
    Raises: [Failure "Invalid die"] if the length of the list of faces and the
    list of probabilities are not equal. *)
val die_vals : t -> dice_id -> (face * prob) list

(** [find_locate brd d] is the identifier of the current tile location of the 
    dice with indentifier [d] in [brd]. 
    Requires: [brd] is a a tree representing a valid game board.
    Raises: [UnknownDice d] if [d] is not the identifier of a die in [brd]. *)
val find_locate : t -> dice_id -> tile_id

(** [start_die brd] is the identifier of the die the board game starts out
    with. 
    Requires: [brd] is a a tree representing a valid game board. *)
val start_die : t -> dice_id

(** [snake_move brd t] is the tile to which the player must move if there is
    a snake's head on tile [t]. If there is a snake's head on tile [t], 
    [snake_move brd t] is the tile at the snake's tail. Otherwise, 
    [snake_move brd t] is [t]. 
    Requires: [brd] is a a tree representing a valid game board.
    Raises: [UnknownTile t] if [t] is not the identifier of a tile in [brd]. *)
val snake_move : t -> tile_id -> tile_id

(** [ladder_move brd t] is the tile to which the player must move if there is
    a ladder's bottom on tile [t]. If there is a ladder's bottom on tile [t], 
    [ladder_move brd t] is the tile at the ladder's top. Otherwise, 
    [ladder_move brd t] is [t]. 
    Requires: [brd] is a a tree representing a valid game board.
    Raises: [UnknownTile t] if [t] is not the identifier of a tile in [brd]. *)
val ladder_move : t -> tile_id -> tile_id

