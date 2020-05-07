open Graphics
open Board 

(** [tile_coord t t_size length x_init y_init] is a tuple
    containing the x and y coordinates in pixels of a tile with tile id [t] 
    where the side length of the tile is [t_size] pixels, the length of the 
    square board is [length] tiles and the x and y coordinates in pixels of 
    the lower left-most corner of the board are [x_init] and [y_init] 
    respectively with respect to the origin of the Graphics window. *)
let tile_coord t t_size length x_init y_init = 
  let x_coord = x_init + (t mod length)*t_size + (t_size/2) in
  let y_coord = y_init + (t/length)*t_size + (t_size/2) in
  (x_coord, y_coord)

(** [draw_tiles r c x y side] draws tiles of side length [size] pixels on the 
    Ocaml Graphics window of a board that has [r] rows and [c] columns with the 
    x and y coordinates of the lower left-most corner of the board are [x] and 
    [y] respectively with respect to the origin of the Graphics window. *)
let draw_tiles r c x y side = 
  for i = 0 to (r-1) do 
    for j = 0 to (c-1) do 
      ( if i = r-1 && j = c-1 then set_color yellow else 
          begin
            if (i+j) mod 2 == 0 then set_color red else set_color white;
          end;
        fill_rect (x + side*j) (y + side*i) side side;  
      )
    done 
  done

(** [draw_ladders brd t_size length x_init y_init] draws the ladders in board 
    [brd] where the side length of the board tile is [t_size] pixels, the 
    length of the square board is [length] tiles and the x and y coordinates 
    in pixels of the lower left-most corner of the board are [x_init] and 
    [y_init] respectively with respect to the origin of the Graphics window. *)
let draw_ladders brd t_size length x_init y_init = 
  let rec draw_ladders_helper lst = 
    match lst with 
    | [] -> ()
    | h::t -> begin 
        let (x,y) = 
          tile_coord (fst h) t_size length x_init y_init in
        let (x',y') = 
          tile_coord (snd h) t_size length x_init y_init in
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

(** [draw_snakes brd t_size length x_init y_init] draws the snakes in board 
    [brd] where the side length of the board tile is [t_size] pixels, the 
    length of the square board is [length] tiles and the x and y coordinates 
    in pixels of the lower left-most corner of the board are [x_init] and 
    [y_init] respectively with respect to the origin of the Graphics window. *)
let draw_snakes brd t_size length x_init y_init = 
  let rec draw_snakes_helper lst = 
    match lst with 
    | [] -> ()
    | h::t -> begin 
        let (x,y) = 
          tile_coord (fst h) t_size length x_init y_init in
        let (x',y') = 
          tile_coord (snd h) t_size length x_init y_init in
        let y_control = y + abs(y'-y)/2 in let x_diff = abs(x'-x)/4 in
        let x1_control = (min x x') + x_diff in 
        let x2_control = (max x x') - x_diff in
        moveto x y;
        curveto (x1_control, y_control) (x2_control, y_control) (x',y');
        draw_snakes_helper t
      end
  in draw_snakes_helper (Board.get_snakes_tiles brd)

let draw_board brd t_size x y = 
  open_graph " 700x700";
  set_window_title "Snakes and Ladders";
  set_color red;
  let size = Board.get_size brd in
  let rows = int_of_float(sqrt(float_of_int(size))) in 
  let cols = rows in
  draw_tiles rows cols x y t_size;
  set_color black;
  draw_rect x y (t_size*rows) (t_size*cols);
  set_line_width 5;
  set_color blue;
  draw_snakes brd t_size rows x y;
  set_color green; 
  draw_ladders brd t_size rows x y; ()