open Yojson.Basic.Util

type dice_id = string
exception UnknownDie of dice_id

type snake_id = string
exception UnknownSnake of snake_id

type ladder_id = string
exception UnknownLadder of ladder_id

type face = int
type prob = float

(** A tile of the game board. *)
type tile_id = int
exception TileOutOfRange of tile_id


(** A die in the game. 
    [d_id] is the identifier of the dice.
    [faces] is a list of the faces of a die. 
    [probs] is the corresponding probability with which the 
      face of the die is rolled. 
      Requires: The sum of all the probabilities in this
      list must be 1. 
    [d_loc] is the location of the dice. *)
type die = {
  d_id: dice_id;
  faces: face list;
  probs: prob list;
  d_loc: tile_id;
}

(** A snake on the board. If a player lands on a tile with [id] = 
    [head] then the player is moved to a tile with [id] = [tail]. 
    [s_id] is the identifier of the snake.
    [head] is the head of the snake.
    [tail] is the tail of the snake. *)
type snake = {
  s_id: snake_id;
  head: tile_id;
  tail: tile_id;
}

(** A ladder on the board. If a player lands on a tile with [id] = 
    [bottom] then the player is moved to a tile with [id] = [top].
    [l_id] is the identifier of the ladder
    [top] is the top of the ladder.
    [bottom] is the bottom of the ladder. *)
type ladder = {
  l_id: ladder_id;
  top: tile_id;
  bottom: tile_id;
}

(** A record representing a game board.
    [dice] is the list of dice in the game.
    [snakes] is the list of snakes on the board.
    [ladders] is the list of ladders on the board.
    [board_size] is size of the board. It is a square number. 
    [start_die] is the starting die in the game.*)
type t = {
  dice: die list;
  snakes: snake list;
  ladders: ladder list;
  board_size: int;
  start_die: die;
}

(** [die_of_json json] parses part of json that corresponds to a die on 
    the board game. *)
let die_of_json json = {
  d_id  = json |> member "die id" |> to_string;
  faces = json |> member "faces" |> to_list |> List.map to_int;
  probs = json |> member "probs" |> to_list |> List.map to_float;
  d_loc = json |> member "die location" |> to_int;
}

(** [snake_of_json json] parses part of json that corresponds to a snake on 
    the board game. *)
let snake_of_json json = {
  s_id = json |> member "snake id" |> to_string;
  head = json |> member "head" |> to_int;
  tail = json |> member "tail" |> to_int;
}

(** [ladder_of_json json] parses part of json that corresponds to a ladder on 
    the board game. *)
let ladder_of_json json = {
  l_id = json |> member "ladder id" |> to_string;
  top = json |> member "top" |> to_int;
  bottom = json |> member "bottom" |> to_int;
}

(** [board_of_json json] is a record representing a game board. 
    Requires: [json] is a valid JSON adventure representation. *)
let board_of_json json = 
  let dice' = json |> member "dice" |> to_list |> List.map die_of_json in
  let start_die_id = json |> member "start die" |> to_string in
  let start_die' = List.find (fun d-> d.d_id = start_die_id) dice' in
  {
    board_size = json |> member "tiles" |> to_int;
    dice = dice';
    start_die = start_die';
    snakes = json |> member "snakes" |> to_list |> List.map snake_of_json;
    ladders = json |> member "ladders" |> to_list |> List.map ladder_of_json;
  }

let from_json json = 
  try board_of_json json
  with Type_error (s, _) -> failwith ("Parsing error: " ^ s)

let dice_ids brd = 
  List.map (fun t -> t.d_id) brd.dice

let find_locate brd d = 
  match List.find (fun di -> di.d_id = d) brd.dice with
  | exception Not_found -> raise (UnknownDie d)
  | di_req -> di_req.d_loc

let start_die brd = 
  brd.start_die.d_id

let additional_move brd pos = 
  if pos < 0 || pos >= brd.board_size 
  then raise (TileOutOfRange pos)
  else
    let snake_at_tile = List.filter (fun s -> s.head = pos) brd.snakes in
    let ladder_at_tile = List.filter (fun l -> l.bottom = pos) brd.ladders in
    match snake_at_tile,ladder_at_tile with
    | [s],_ -> s.tail
    | _,[l] -> l.top
    | _ -> pos

(** [get_die] checks the list of dice in [brd] and returns die given 
    the die_id. *)
let get_die brd d_id = 
  let rec get_die_helper d_lst d_id = 
    match d_lst with
    | hd::tl -> if hd.d_id = d_id then hd else get_die_helper tl d_id
    | _ -> raise (UnknownDie d_id) 
  in
  get_die_helper brd.dice d_id

let dice_roll brd d_id = 
  let die = get_die brd d_id in
  let rec rollhelper i (x:float) lb ub faces probs  =
    if x > lb && x <= ub then 
      List.nth faces i
    else
      let ub' = ub +. List.nth probs (i+1) in
      rollhelper (i+1) x ub ub' faces probs
  in
  let () = Random.self_init () in
  let tgt = Random.float 1.0 in
  rollhelper 0 tgt 0.0 (List.nth die.probs 0) die.faces die.probs

let get_size brd = brd.board_size

let get_die_at_tile brd pos = 
  let dice_at_tile = List.filter (fun d -> d.d_loc = pos) brd.dice in
  match dice_at_tile with
  | [x] -> Some x.d_id
  | [] -> None
  | _ -> failwith "Multiple Dice occupying one tile!"

let get_faces brd d_id =
  let d = get_die brd d_id in
  d.faces

let get_probs brd d_id =
  let d = get_die brd d_id in
  d.probs

let get_ladders_tiles brd = 
  let rec get_ladder_helper lst acc = 
    match lst with 
    | [] -> acc 
    | hd::tl -> get_ladder_helper tl ((hd.bottom, hd.top)::acc) in 
  get_ladder_helper brd.ladders []

let get_snakes_tiles brd = 
  let rec get_snake_helper lst acc = 
    match lst with 
    | [] -> acc 
    | hd::tl -> get_snake_helper tl ((hd.tail, hd.head)::acc) in 
  get_snake_helper brd.snakes []