open Common
open Board

type t = {
  won: bool;
  bot: bool list;
  num_players: int;
  current_player: int;
  dice: dice_id list list;
  selected_die: dice_id list;
  player_positions: tile_id list;
  last_roll: int option;
  prev_player_pos: int option; 
}

let bot_lst_init nbots nplayers = 
  let playerlst = List.init nplayers (fun x -> false) in 
  let botlst = List.init nbots (fun x -> true) in 
  List.append playerlst botlst

let init_state board nplayers nbots =
  {
    bot= bot_lst_init nbots nplayers;
    won = false;
    num_players = nplayers+nbots;
    current_player = 0;
    dice = List.init (nplayers+nbots) (fun x-> [Board.start_die board]);
    selected_die = List.init (nplayers+nbots)(fun x-> Board.start_die board);
    player_positions = List.init (nplayers+nbots) (fun x -> 0);
    last_roll = None;
    prev_player_pos = None;
  }

let check_won st = st.won

let bot_list st = st.bot 

let n_players st = st.num_players

let get_curr_player st = st.current_player

let dice_list st = st.dice

let selected_die st = st.selected_die

let players_pos st = st.player_positions

let last_roll st = 
  match st.last_roll with
  | Some x -> string_of_int x
  | _ -> "None"

let prev_player_pos st = 
  match st.prev_player_pos with
  | Some x -> string_of_int x
  | _ -> "None"

let curr_pos st = List.nth st.player_positions st.current_player

let curr_die st: dice_id = List.nth st.selected_die st.current_player 

let curr_dice st: dice_id list = List.nth st.dice st.current_player 

type result = 
  | Changed_Die of t
  | Normal_Roll of t 
  | Slid_Down_Snake of t
  | Went_Up_Ladder of t
  | Found_New_Die of t
  | Roll_Not_Valid of t
  | Invalid_Die of t

(** [invalid_roll st next_player roll_val curr_pos] is the state representing 
    an invalid roll. *)
let invalid_roll st next_player roll_val curr_pos = 
  Roll_Not_Valid { st with
                   current_player = next_player;
                   last_roll = Some roll_val;
                   prev_player_pos = Some curr_pos;
                 } 

(** [normal_roll st brd next_player positions' roll_val tile_landed] is the 
    state represnting a normal die roll with normal progression and no new die. 
*)
let normal_roll st brd next_player positions' roll_val tile_landed = 
  Normal_Roll { st with 
                current_player = next_player;
                player_positions = positions';
                last_roll = Some roll_val;
                won = (tile_landed = Board.get_size brd -1);
                prev_player_pos = Some tile_landed;
              }

(** [found_die st brd next_player dice' positions' roll_val tile_landed] is the 
    state represnting a normal die roll with normal progression and a new die.
*)
let found_die st brd next_player dice' positions' roll_val tile_landed = 
  Found_New_Die { st with 
                  current_player = next_player;
                  dice = dice';
                  player_positions = positions';
                  last_roll = Some roll_val;
                  won = tile_landed = Board.get_size brd -1;
                  prev_player_pos = Some tile_landed
                }

(** [ladder_m st brd next_player positions' roll_val tile_landed] is the state 
    representing the movement of a player up a ladder. *)
let ladder_m st brd next_player positions' roll_val tile_landed = 
  Went_Up_Ladder { st with 
                   current_player = next_player;
                   player_positions = positions';
                   last_roll = Some roll_val;
                   won = tile_landed = Board.get_size brd -1;
                   prev_player_pos = Some tile_landed
                 }

(** [snake_m st brd next_player positions' roll_val tile_landed] is the state 
    representing the movement of a player dwon a snake. *)
let snake_m st brd next_player positions' roll_val tile_landed = 
  Slid_Down_Snake { st with 
                    current_player = next_player;
                    player_positions = positions';
                    last_roll = Some roll_val;
                    prev_player_pos = Some tile_landed;
                  }

(** [new_die_helper new_die st brd next_player positions' roll_val tile_landed] 
    is the resulting state on checking if the current player has landed on a 
    tile containing a die. *)
let new_die_helper new_die st brd next_player positions' roll_val tile_landed = 
  match new_die with
  | None -> normal_roll st brd next_player positions' roll_val tile_landed
  | Some d -> begin 
      let player_dice = List.nth st.dice st.current_player in
      let player_dice' = d::player_dice in
      let dice' = swap_elem st.current_player player_dice' st.dice in
      found_die st brd next_player dice' positions' roll_val tile_landed 
    end

let roll brd st : result = 
  let next_player =  (st.current_player + 1) mod st.num_players in
  let curr_pos = List.nth st.player_positions st.current_player in
  let die_to_roll = curr_die st in
  let roll_val = Board.dice_roll brd die_to_roll in
  let roll_landed = curr_pos + roll_val in
  if roll_landed < 0 || roll_landed >= (Board.get_size brd) then
    invalid_roll st next_player roll_val curr_pos 
  else 
    let tile_landed = Board.additional_move brd roll_landed in 
    let positions' = swap_elem st.current_player tile_landed st.player_positions
    in let new_die = Board.get_die_at_tile brd tile_landed in
    if (tile_landed = roll_landed) then
      new_die_helper new_die st brd next_player positions' roll_val tile_landed 
    else begin
      if tile_landed > roll_landed then 
        ladder_m st brd next_player positions' roll_val tile_landed
      else snake_m st brd next_player positions' roll_val tile_landed 
    end

let use_die st d_id : result = 
  let player_diceids = List.nth st.dice st.current_player in
  let dice_match = List.mem d_id player_diceids in
  if dice_match
  then 
    let selected_die'=  swap_elem st.current_player d_id st.selected_die in
    Changed_Die { st with 
                  selected_die = selected_die'; 
                }
  else Invalid_Die st  
