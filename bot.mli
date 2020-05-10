(** This module contains the functionality for a game bot. *)

(** [bot_die brd st] is the identifier of the die with the most beneficial roll 
    given board [brd] and state [st]. *)
val bot_die : Board.t -> State.t -> Board.dice_id