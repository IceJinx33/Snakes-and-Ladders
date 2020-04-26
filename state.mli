open Board
(********************************************************************
   Representation of a snakes and ladder's game state, a snapshot of
   a game in progress.

   This module represents the data stored in the game files, including
   the tiles, dice, snakes and ladders.  It handles loading of that data 
   from JSON as well as querying the data.
 ********************************************************************)

(** The abstract type of values representing a game state. *)
type t

type result = 
  | Changed_Die of t
  | Normal_Roll of t 
  | Slid_Down_Snake of t
  | Went_Up_Ladder of t
  | Found_New_Die of t
  | Roll_Not_Valid of t
  | Invalid_Die of t

val init_state : Board.t -> int -> t
val roll : Board.t -> t -> result 
val use_die: t -> Board.dice_id -> result  
val curr_pos: t -> Board.tile_id
val last_roll: t -> string
val check_won: t -> bool 
val get_curr_player: t -> int 