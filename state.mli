(********************************************************************
   Representation of a snakes and ladder's game state, a snapshot of
   a game in progress.

   This module represents the data stored in the game files, including
   the tiles, dice, snakes and ladders.  It handles loading of that data 
   from JSON as well as querying the data.
 ********************************************************************)

(** The abstract type of values representing a game state. *)
type t

val init_state : Board.t -> t