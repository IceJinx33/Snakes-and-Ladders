open Board
open Main

type t = {
  won: bool;
  num_players: int;
  current_player: int;
  dice: dice_id list list;
  selected_die: dice_id list;
  player_positions: tile_id list;
}

let init_state board nplayers =
  {
    won = false;
    num_players = nplayers;
    current_player = 0;
    dice = List.init nplayers (fun x-> [Board.start_die board]);
    selected_die = List.init nplayers (fun x-> Board.start_die board);
    player_positions = List.init nplayers (fun x-> Board.start_tile board);
  }

let selected_die st = st.selected_die

let curr_die st: dice_id =
  match select_elem st.current_player st.selected_die with
  | Some x -> x
  | _ -> failwith "current_player index messed up"

let roll st : t = 
  let die_to_roll = curr_die st in
  let roll_val = Board.dice_roll die_to_roll in
  let 



