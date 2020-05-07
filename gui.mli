(********************************************************************
   Graphical visualization of the board game. 

   This module contains drawing functions that help create a 
   graphical interface for the board game.
 ********************************************************************)

(** [draw_board brd t_size x y] draws the board [brd] on an Ocaml Graphics 
    window with tiles having side of length [t_size] pixels. The lower 
    left-most corner of the board has x-coordinates [x] pixels and 
    y-coordinates [y] pixels with respect to the origin of the Graphics window.
    Requires: The number of tiles in [brd] must be a positive square number. 
*)
val draw_board : Board.t -> int -> int -> int -> unit 