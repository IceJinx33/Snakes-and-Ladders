open Common
open Board

type t = {
  won: bool;
  num_players: int;
  current_player: int;
  dice: dice_id list list;
  selected_die: dice_id list;
  player_positions: tile_id list;
  last_roll: int option;
}

let init_state board nplayers =
  {
    won = false;
    num_players = nplayers;
    current_player = 0;
    dice = List.init nplayers (fun x-> [Board.start_die board]);
    selected_die = List.init nplayers (fun x-> Board.start_die board);
    player_positions = List.init nplayers (fun x -> 0);
    last_roll = None
  }

let selected_die st = st.selected_die

let curr_die st: dice_id = List.nth st.selected_die st.current_player 

type result = 
  | Changed_Die of t
  | Normal_Roll of t 
  | Slid_Down_Snake of t
  | Went_Up_Ladder of t
  | Found_New_Die of t
  | Roll_Not_Valid of t
  | Invalid_Die of t

let rec pp_list_list lst acc = 
match lst with 
| [] -> acc 
| [h] -> acc^" "^(pp_list pp_string h)
| h1::(h2::t as t') -> pp_list_list t' (acc^" "^(pp_list pp_string h1)) 

(** [pp_state st] is the string representation of the state [st]. *)
let pp_state st = 
"{ won : " ^ string_of_bool (st.won) ^ " , no. of players : " ^ 
 string_of_int (st.num_players) ^ " , current player : " ^ 
 string_of_int (st.current_player) ^ " , list of dice : " ^ 
 pp_list_list (st.dice) "" ^ " , selected die : " ^ 
 (pp_list pp_string st.selected_die) ^ " , player pos : " ^ 
 (pp_list string_of_int st.player_positions) ^ " , last roll : " ^ 
 extract_opt (st.last_roll)

let roll brd st : result = 
  let next_player =  (st.current_player + 1) mod st.num_players in
  let curr_pos = List.nth st.player_positions st.current_player in
  let die_to_roll = curr_die st in
  let roll_val = Board.dice_roll brd die_to_roll in
  let roll_landed = curr_pos + roll_val in
  if roll_landed > Board.get_size brd then
  Roll_Not_Valid {
    st with
    current_player = next_player;
    last_roll = Some roll_val;
  } 
  else
  let tile_landed = Board.additional_move brd roll_landed in 
  let new_die = Board.get_die_at_tile brd tile_landed in
  let positions' = swap_elem tile_landed st.current_player st.player_positions in
  if tile_landed = roll_landed then
    match new_die with
    (* Rolled, No events, No new dice *)
    | None -> Normal_Roll {
      st with 
      current_player = next_player;
      player_positions = positions';
      last_roll = Some roll_val;
      won = tile_landed = Board.get_size brd
    }
    (* Rolled, No events, Found new dice *)
    | Some d -> Found_New_Die {
      st with 
      current_player = next_player;
      player_positions = positions';
      last_roll = Some roll_val;
      won = tile_landed = Board.get_size brd
    }
  else if tile_landed > roll_landed then
  (* Went up ladder *)
  Went_Up_Ladder {
      st with 
      current_player = next_player;
      player_positions = positions';
      last_roll = Some roll_val;
      won = tile_landed = Board.get_size brd
    }
  else
  (* Went down *)
  Slid_Down_Snake {
    st with 
    current_player = next_player;
    player_positions = positions';
    last_roll = Some roll_val;
  }
  
  


let use_die st d_id : result = 
  let player_diceids = List.nth st.dice st.current_player in
  let dice_match = List.mem d_id player_diceids in
  if dice_match
  then 
  let selected_die'=  swap_elem st.current_player d_id st.selected_die in
  Changed_Die {st with 
  selected_die = selected_die'; 
   }
  else Invalid_Die st  
