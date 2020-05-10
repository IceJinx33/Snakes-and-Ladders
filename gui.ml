open Graphics
open Board 
open State

(** Colors representing the different players. *)
let purple = rgb 128 0 128
let orange = rgb 255 165 0
let dark_green = rgb 0 100 0
let teal = rgb 0 204 204
let brown = rgb 153 76 0

(** [tile_coord t length t_size x_init y_init] is a tuple
    containing the x and y coordinates in pixels of a tile with tile identifier
    [t] where the side length of the tile is [t_size] pixels, the length of the 
    square board is [length] tiles and the x and y coordinates in pixels of 
    the lower left-most corner of the board are [x_init] and [y_init] 
    respectively with respect to the origin of the Graphics window. *)
let tile_coord t length t_size x_init y_init = 
  let x_coord = x_init + (t mod length)*t_size + (t_size/2) in
  let y_coord = y_init + (t/length)*t_size + (t_size/2) in
  (x_coord, y_coord)

(** [draw_tiles length t_size x y side] draws tiles of side length [t_size] 
    pixels on the Ocaml Graphics window of a square board that has a side of 
    [length] tiles where the x and y coordinates in pixels of the lower 
    left-most corner of the board are [x] and [y] respectively with respect to 
    the origin of the Graphics window. *)
let draw_tiles length t_size x y = 
  for i = 0 to (length-1) do 
    for j = 0 to (length-1) do 
      ( if i = length-1 && j = length-1 then set_color yellow else 
          begin
            if (i+j) mod 2 == 0 then set_color red else set_color white;
          end;
        fill_rect (x + t_size*j) (y + t_size*i) t_size t_size;  )
    done 
  done

(** [draw_tile_nos length t_size x y] draws the tile numbers starting from 0
    of tiles of side length [t_size] pixels on the Ocaml Graphics window of a 
    square board that has a side of [length] tiles where the x and y 
    coordinates in pixels of the lower left-most corner of the board are [x] 
    and [y] respectively with respect to the origin of the Graphics window. *)
let draw_tile_nos length t_size x y = 
  set_color black;
  set_font "-*-fixed-medium-r-semicondensed--40-*-*-*-*-*-iso8859-1";
  let count = ref 0 in
  for i = 0 to (length-1) do 
    for j = 0 to (length-1) do 
      ( if !count < 10 then moveto (x + t_size*j + 17) (y + t_size*i + 5) else
          moveto (x + t_size*j + 7) (y + t_size*i + 5);
        draw_string (string_of_int !count);
        if(!count <> length*length-1) then count := !count+1 else count := 0;
      )
    done 
  done

(** [draw_ladders brd length t_size x_init y_init] draws the ladders in board 
    [brd] where the side length of the board tile is [t_size] pixels, the 
    length of the square board is [length] tiles and the x and y coordinates 
    in pixels of the lower left-most corner of the board are [x_init] and 
    [y_init] respectively with respect to the origin of the Graphics window. *)
let draw_ladders brd length t_size x_init y_init = 
  let rec draw_ladders_helper lst = 
    match lst with 
    | [] -> ()
    | h::t -> begin 
        let (x,y) = tile_coord (fst h) length t_size x_init y_init in
        let (x',y') = tile_coord (snd h) length t_size x_init y_init in
        let x_diff = min (abs(x'-x)/10) 15 in let y_diff = abs(y'-y)/4 in
        let ladder_seg = (x'-x)/4 in let x1 = x+ladder_seg in 
        let x2 = x+(2*ladder_seg) in let x3 = x+(3*ladder_seg) in
        let points = [|(x-x_diff, y, x'-x_diff, y'); 
                       (x+x_diff, y, x'+x_diff, y'); 
                       (x1-x_diff, y+y_diff, x1+x_diff, y+y_diff);
                       (x2-x_diff, y+(2*y_diff), x2+x_diff, y+(2*y_diff));
                       (x3-x_diff, y'-y_diff, x3+x_diff, y'-y_diff)|] in
        draw_segments points;
        draw_ladders_helper t
      end
  in draw_ladders_helper (Board.get_ladders_tiles brd)

(** [draw_snakes brd length t_size x_init y_init] draws the snakes in board 
    [brd] where the side length of the board tile is [t_size] pixels, the 
    length of the square board is [length] tiles and the x and y coordinates 
    in pixels of the lower left-most corner of the board are [x_init] and 
    [y_init] respectively with respect to the origin of the Graphics window. *)
let draw_snakes brd length t_size x_init y_init = 
  let rec draw_snakes_helper lst = 
    match lst with 
    | [] -> ()
    | h::t -> begin 
        let (x,y) = tile_coord (fst h) length t_size x_init y_init in
        let (x',y') = tile_coord (snd h) length t_size x_init y_init in
        let y_control = y + abs(y'-y)/2 in let x_diff = abs(x'-x)/4 in
        let x1_control = (min x x') + x_diff in 
        let x2_control = (max x x') - x_diff in
        moveto x y;
        curveto (x1_control, y_control) (x2_control, y_control) (x',y');
        draw_snakes_helper t
      end
  in draw_snakes_helper (Board.get_snakes_tiles brd)

(** [draw_board brd length t_size x y] draws the board square [brd] with a side 
    length of [length] tiles on an Ocaml Graphics window with tiles having side 
    of length [t_size] pixels. The lower left-most corner of the board has 
    x-coordinates [x] pixels and y-coordinates [y] pixels with respect to the 
    origin of the Graphics window.
    Requires: The number of tiles in [brd] must be a positive square integer. *)
let draw_board brd length t_size x y = 
  draw_tiles length t_size x y ;
  draw_tile_nos length t_size x y ;
  set_line_width 5;
  set_color black;
  draw_rect x y (t_size*length) (t_size*length);
  set_line_width 5;
  set_color blue;
  draw_snakes brd length t_size x y;
  set_color green; 
  draw_ladders brd length t_size x y; ()

(** [players_in_tile lst] is a list of tuples where the first element is the 
    player number and the second element is the tile identifier associated 
    with the player. 
    Requires: [lst] is a list of identifiers of tile positions of the players 
    starting in order from player 1. *)
let players_in_tile lst = 
  match lst with 
  | [] -> []
  | _::_ -> begin
      let rec player_helper lst acc count = 
        match lst with 
        | [] -> acc
        | h::t -> player_helper t ((count,h)::acc) (count+1)
      in player_helper lst [] 1
    end

(** [player_settings n t length t_size x_init y_init] sets the color and 
    position of player number [n] within a tile with identifier [t] and tile 
    size [t_size] pixels on a square board with side of length [length] tiles 
    and the x and y coordinates in pixels of the lower left-most corner of the 
    board are [x_init] and [y_init] respectively with respect to the origin of 
    the Graphics window. 
    Requires: [n] must be a positive integer less than or equal to 5.
    Raises: Failure ["No more than 5 players allowed"] if [n] > 5. *)
let player_settings n t length t_size x_init y_init = 
  let (x,y) = tile_coord t length t_size x_init y_init in
  let diff = 3*t_size/10 in
  match n with 
  | 1 -> set_color purple; (x+diff,y+diff)
  | 2 -> set_color orange; (x+diff,y-diff)
  | 3 -> set_color dark_green; (x-diff,y+diff)
  | 4 -> set_color teal; (x-diff,y-diff)
  | 5 -> set_color brown; (x,y)
  | _ -> failwith "No more than 5 players allowed"

(** [draw_players st length t_size x y p_size] draws the current positions of 
    the players in state [st] using circles of radius [p_size] on a square 
    board with side of length [length] tiles and the x and y coordinates in 
    pixels of the lower left-most corner of the board are [x] and [y] 
    respectively with respect to the origin of the Graphics window. *)
let draw_players st length t_size x y p_size = 
  set_font "-*-fixed-medium-r-semicondensed--20-*-*-*-*-*-iso8859-1";
  let p_tile_lst = players_in_tile (State.players_pos st) in 
  List.iter (fun elt -> let (x', y') = 
                          player_settings (fst elt) (snd elt) length t_size x y 
              in fill_circle x' y' p_size; moveto ((fst elt)*x) (y/2);
              draw_string ("Player "^(string_of_int (fst elt)));
            ) p_tile_lst 

let draw_game brd st = 
  clear_graph();
  set_window_title "Snakes and Ladders";
  let size = Board.get_size brd in
  let length = int_of_float(sqrt(float_of_int(size))) in 
  draw_board brd length 50 100 100;
  draw_players st length 50 100 100 10; ()

let draw_game_init brd st = 
  open_graph " 700x700";
  set_window_title "Snakes and Ladders";
  let size = Board.get_size brd in
  let length = int_of_float(sqrt(float_of_int(size))) in 
  draw_board brd length 50 100 100;
  draw_players st length 50 100 100 10; ()

let draw_win message brd = 
  let size = Board.get_size brd in
  let length = int_of_float(sqrt(float_of_int(size))) in 
  set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
  set_color black;
  moveto 10 (120+length*50); draw_string message; ()
