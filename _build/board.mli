(********************************************************************
   Representation of a static board game. 

   This module represents the data stored in the game files, including
   the tiles, dice, snakes and ladders.  It handles loading of that data 
   from JSON as well as querying the data.
 ********************************************************************)

(** The abstract type of values representing a game board. *)
type t

(** The type of tile identifier. *)
type tile_id = int

(** Raised when an unknown tile is encountered. *)
exception TileOutOfRange of tile_id

(** The type of die identifier. *)
type dice_id = string

(** Raised when an unknown die is encountered. *)
exception UnknownDie of dice_id

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

(** [from_json json] is the adventure that [json] represents.
    Requires: [json] is a valid JSON adventure representation. *)
val from_json : Yojson.Basic.t -> t

(** [dice_ids brd] is the list of dice identifiers in [brd]. 
    Requires: [brd] is a a record representing a valid game board. *)
val dice_ids : t -> dice_id list

(** [find_locate brd d] is the identifier of the current tile location of the 
    dice with indentifier [d] in [brd]. 
    Requires: [brd] is a a record representing a valid game board.
    Raises: [UnknownDice d] if [d] is not the identifier of a die in [brd]. *)
val find_locate : t -> dice_id -> tile_id

(** [additional_move brd pos] is the resulting position of interacting with
    either a ladder or a snake at tile pos if either of them exists. 
    Requires: [pos] is a tile within {0 ... n-1}
    Raises: [TileOutOfRange pos] if [pos] is out of the range of tiles on
    the board*)
val additional_move: t -> tile_id -> tile_id

(* [dice_roll die] is the result of rolling die given its probabilities and faces *)
val dice_roll: t -> dice_id -> face

(** [start_die brd] is the identifier of the die the board game starts out
    with. 
    Requires: [brd] is a a record representing a valid game board. *)
val start_die : t -> dice_id

(* [get_size brd] is the number of tiles on the board *)
val get_size: t -> int

val get_die_at_tile: t -> tile_id -> dice_id option

