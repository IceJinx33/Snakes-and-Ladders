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

(** The abstract type of results possible based on the terminal input 
    and the available actions of the board. Every result type contains a state. *)
type result = 
  | Changed_Die of t
  | Normal_Roll of t 
  | Slid_Down_Snake of t
  | Went_Up_Ladder of t
  | Found_New_Die of t
  | Roll_Not_Valid of t
  | Invalid_Die of t

(** [init_state brd ] takes in the board [brd] and creates the game's 
    starting state. *)
val init_state : Board.t -> int -> t

(** [roll brd st] updates the state [st] after each player roll and 
    checks for [Normal_Roll] [Slid_Down_Snake] [Went_Up_Ladder] [Found_New_Die]. 
    It is [Roll_Not_Valid] if player rolls a number greater than are left in the 
    board. *)
val roll : Board.t -> t -> result 

(** [use_die st d] updates the game state after player selects a new die. 
    [Invalid_Die] if the die is has not been picked up by player. *)
val use_die: t -> Board.dice_id -> result  

(** [check_won st] checks if any player has reached the winning tile. *)
val check_won: t -> bool 

val check_bot: t -> bool list

(** [n_players st] is the number of players in the game state [st]. *)
val n_players: t -> int

(** [get_curr_player st] is the current player in the game state [st]. *)
val get_curr_player: t -> int 

(** [dice_list st] is the list of the lists of dice of each player in the game 
    state [st]. *)
val dice_list: t -> Board.dice_id list list

(** [selected_die st] is the currently selected dies for the players in [st]. *)
val selected_die: t -> Board.dice_id list

(** [player_pos st] is the list of player positions in [st]. *)
val players_pos: t -> Board.tile_id list

(** [last_roll st] gives the last roll of the state [st]. *)
val last_roll: t -> string

(** [curr_pos st] returns the current position of the current player in [st]. *)
val curr_pos: t -> Board.tile_id

(** [curr_dice st] returns the list of dice for the current player in [st]. *)
val curr_dice: t -> Board.dice_id list

(** [curr_dice st] returns the list of dice for the current player in [st]. *)
val curr_die: t -> Board.dice_id

val prev_players_position: t -> string

