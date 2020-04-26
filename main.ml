open Board
open State
open Command
open Yojson.Basic.Util

let indent = "    "

let check_win_cond st = 
  if check_won st then
  let curr_player_str = string_of_int (State.get_curr_player st) in
  ANSITerminal.(print_string [green] ("\nCongratulations! " ^curr_player_str^ " won!\n"));
  exit 0;
  else ()

let handle_pick_die board st dice_id = 
    match State.use_die st dice_id  with 
    | Invalid_Die st' -> ANSITerminal.(print_string [red]
     ("\nThat die is unavaiable, try again. \n"));
       print_string  ">"; st'
    | Changed_Die st' -> ANSITerminal.(print_string [red]
     ("\n New dice chosen \n"));
       print_string  ">"; st'
    | _ -> st

let handle_roll board st = 
  let roll_res = State.roll board st in
  let print_roll_res st = 
    let roll_val = State.last_roll st in
      ANSITerminal.(print_string [red] ("\nYou rolled a "^roll_val^"\n"));
  in
  match roll_res with 
    | Normal_Roll st' -> 
      print_roll_res st';
      st'
    | Slid_Down_Snake st' -> 
      print_roll_res st';
      ANSITerminal.(print_string [red] ("\nYou've slide down a snake \n")) ;  st'
    | Went_Up_Ladder st' -> 
      print_roll_res st';
      ANSITerminal.(print_string [red] ("\nYou've gone up a ladder\n")) ;
      st'
    | Found_New_Die  st' -> 
      print_roll_res st';
      ANSITerminal.(print_string [red] ("\nYou found a new die \n")) ;
      st'
    | Roll_Not_Valid st' -> 
      print_roll_res st';
      ANSITerminal.(print_string [red] ("\n That roll is not valid, your roll must be the exact spaces until the end \n")) ;
      st'
    |_ -> st


let rec make_move st brd: unit = 
  check_win_cond st;
  let curr_pos_str = string_of_int (State.curr_pos st) in 
  ANSITerminal.(print_string [white] ("\n You're on tile "^curr_pos_str));
  ANSITerminal.(print_string [red] ("\n Please choose an action\n"));
 match read_line () with
  | exception End_of_file -> ()
  | input -> 
    begin
    match Command.parse input with
    | Quit ->  exit 0
    | Pick_Die d_id -> 
      let st' = handle_pick_die brd st d_id in
      make_move st' brd
    | Roll ->  
      let () = print_endline "parsed correctly" in
      let st' = handle_roll brd st in
      let () = print_endline "Finished roll" in
      make_move st' brd;
    | exception Empty ->  
      print_string ">";
      make_move st brd
    | exception Malformed ->   ANSITerminal.(print_string [red] ("\nCommand was malformed, try again \n")); make_move st brd
    end
    

(** [play_game f] starts the adventure in file [f]. *)
let play_game (f:string): unit =
  (* Try wrap this, file may not exist *)
  (* Take the name of the json *)
  (* import it based off the file path *)
  try
    let () = print_string indent in
    ANSITerminal.(print_string [yellow] ("Loading "^f^"...\n"));
    let j = Yojson.Basic.from_file f in
    let () = print_string indent in
    ANSITerminal.(print_string [green] ("Successully loaded "^f^"!\n\n"));
    let board = Board.from_json j in
    let st = State.init_state board 1 in
    make_move st board

  (* with e -> failwith ("Lets try a valid file name instead of: " ^ f) *)
  with e -> 
  match e with
  | Type_error (s, _) ->
    ANSITerminal.(print_string [red] (
        "\n\ Uh oh, "^f^" is in uncharted territory.\n\ 
        Let's not try to venture into "^f^" next time. \n\n"))
  | _ -> ANSITerminal.(print_string [red] (
      "\n\ Some error occured and you should probably panic. \n\n" );)


(* Make function for getting stdin --> doing  *)

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to Snakes and Ladders.\n");
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name

(* Execute the game engine. *)
(*  *)
let () = main ()