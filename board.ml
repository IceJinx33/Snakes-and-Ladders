open Common
open Yojson.Basic.Util

(* type tile_id = string *)
(* exception UnknownTile of tile_id *)

type dice_id = string
exception UnknownDie of dice_id

type snake_id = string
exception UnknownSnake of snake_id

type ladder_id = string
exception UnknownLadder of ladder_id

type face = int
type prob = float


type tile_id = int
(** A tile of the game board. *)
(* type tile = { *)
  (** [t_id] is the identifier of the tile. 
      Requires: [t_id] must be an integer represented in a string. *)
  (* t_id: tile_id; *)
(* } *)

(** A die in the game. *)
type die = {
  (** [d_id] is the identifier of the dice. *)
  d_id: dice_id;
  (** [faces] is a list of the faces of a die. *)
  faces: face list;
  (** [probs] is the corresponding probability with which the 
      face of the die is rolled. 
      Requires: The sum of all the probabilities in this
      list must be 1. *)
  probs: prob list;
  (** [d_loc] is the location of the dice. *)
  d_loc: tile_id;
}

(** A snake on the board. If a player lands on a tile with [id] = 
    [head] then the player is moved to a tile with [id] = [tail]. *)
type snake = {
  (** [s_id] is the identifier of the snake. *)
  s_id: snake_id;
  (** [head] is the head of the snake. *)
  head: tile_id;
  (** [tail] is the tail of the snake. *)
  tail: tile_id;
}

(** A ladder on the board. If a player lands on a tile with [id] = 
    [bottom] then the player is moved to a tile with [id] = [top]. *)
type ladder = {
  (** [l_id] is the identifier of the ladder. *)
  l_id: ladder_id;
  (** [top] is the top of the ladder. *)
  top: tile_id;
  (** [bottom] is the bottom of the ladder. *)
  bottom: tile_id;
}

(** A record representing a game board. *)
type t = {
  (** [tiles] is the list of tiles on the board. *)
  (* tiles: tile list; *)
  (** [start_tile] is the identifier of the tile where the game begins. *)
  (* start_tile: tile_id; *)
  (** [win_tile] is the identifier of the winning tile where the game ends. *)
  (* win_tile: tile_id; *)
  (** [dice] is the list of dice in the game. *)
  dice: die list;
  (** [start_dice] is the starting dice in the game. *)
  start_die: dice_id;
  (** [snakes] is the list of snakes on the board. *)
  snakes: snake list;
  (** [ladders] is the list of ladders on the board. *)
  ladders: ladder list;
  board_size: int;
}

(** [tile_of_json json] parses part of [json] that corresponds to a tile on 
    the board game. *)
(* let tile_of_json json = {
  t_id = json |> member "tile id" |> to_string;
} *)

(** [die_of_json json] parses part of json that corresponds to a die on 
    the board game. *)
let die_of_json json = {
  d_id = json |> member "die id" |> to_string;
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
let board_of_json json = {
  (* tiles = json |> member "tiles" |> to_list |> List.map tile_of_json; *)
  board_size = json |> member "tiles" |> to_int;
  (* win_tile = json |> member "win tile" |> to_string; *)
  dice = json |> member "dice" |> to_list |> List.map die_of_json;
  start_die = json |> member "start die" |> to_string;
  snakes = json |> member "snakes" |> to_list |> List.map snake_of_json;
  ladders = json |> member "ladders" |> to_list |> List.map ladder_of_json;
}

let from_json json = 
  try board_of_json json
  with Type_error (s, _) -> failwith ("Parsing error: " ^ s)


(* let tile_ids brd = 
  brd.tiles |> List.map (fun t -> t.t_id) |> List.map int_of_string |> 
  List.sort_uniq compare |> List.map string_of_int *)

(* let start_tile brd = 
  brd.start_tile *)

(* let win_tile brd = 
  brd.win_tile *)

let dice_ids brd = 
  List.map (fun t -> t.d_id) brd.dice

(* let die_vals brd d = 
  match List.find (fun di -> di.d_id = d) brd.dice with
  | exception Not_found -> raise (UnknownDie d)
  | di_req -> begin
      let face_prob f p = (f,p) in
      match List.map2 face_prob di_req.faces di_req.probs with
      | exception Invalid_argument s -> raise (Failure "Invalid die")
      | lst -> lst
    end *)

let find_locate brd d = 
  match List.find (fun di -> di.d_id = d) brd.dice with
  | exception Not_found -> raise (UnknownDie d)
  | di_req -> di_req.d_loc

let start_die brd = 
  brd.start_die

let rec get_snake_tail brd s_id = 
  let snake = List.filter (fun s -> s.s_id = s_id) brd.snakes in
  match snake with
  | [x] -> x.tail 
  | _ -> raise (UnknownSnake s_id)
  
(* let snake_move brd s_id =  *)

  (* match List.mem t (tile_ids brd) with 
  | true -> begin 
      match List.find (fun s -> s.head = t) brd.snakes with
      | exception Not_found -> t
      | snk -> snk.tail
    end
  | false -> raise (UnknownTile t) *)

let ladder_move brd t = 
  match List.mem t (tile_ids brd) with 
  | true -> begin 
      match List.find (fun l -> l.bottom = t) brd.ladders with
      | exception Not_found -> t
      | lad -> lad.top
    end
  | false -> raise (UnknownTile t)

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
    select_elem i faces  
    else
    let ub' = ub +. select_elem (i+1) probs in
    rollhelper (i+1) x ub ub' faces probs
  in
  let tgt = Random.float 1.0 in
  rollhelper 0 tgt 0.0 (select_elem 0 die.probs) die.faces die.probs