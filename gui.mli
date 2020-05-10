(**
   Graphical visualization of the board game. 
   This module contains drawing functions that help create a 
   graphical interface for the board game.
*)

(** [draw_game brd st] draws the game board [brd] and the current positions of 
    the players in state [st] of the game. 
    Requires: [brd] is a valid representation of a game board and 
              [st] is a valid game state. *)
val draw_game : Board.t -> State.t -> unit 

(** [draw_game_init brd st] draws the game board [brd] and the initial
    positions of the players in the initial state [st] of the game. 
    Requires: [brd] is a valid representation of a game board and 
              [st] is a valid game state. *)
val draw_game_init : Board.t -> State.t -> unit 

(** [draw_win message brd] draws a win message [message] on the Ocaml 
    Graphics window. 
    Requires: There must be an existing Ocaml Graphics window open and 
    [brd] is a valid representation of a game board. *)
val draw_win : string -> Board.t -> unit