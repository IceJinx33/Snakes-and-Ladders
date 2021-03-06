open Board
open State
open Common
open Command
open Yojson.Basic.Util
open Gui
open Bot

(** [check_win_cond brd st] checks if any player has fulfilled the winning 
    condition. *)
let check_win_cond brd st = 
  if check_won st then
    let cur_p = if State.get_curr_player st = 0 then State.n_players st else
        State.get_curr_player st in
    let curr_player_str = string_of_int (cur_p) in
    let message = "Player "^curr_player_str^ " won!" in
    draw_game brd st;
    draw_win message brd;
    ANSITerminal.(print_string [green] ("\n"^message^"\n"));
    ANSITerminal.(print_string [red] ("Quitting!!\n"));
    for i=1 to 1000000 do for j=1 to 100 do () done done;
    exit 0;
  else ()

let is_bot st = 
  List.nth (bot_list st) (State.get_curr_player st)

(** [handle_pick_die board st dice_id] takes the parsed command and prints in 
    the terminal if the channge was made or invalid.  *)
let handle_pick_die brd st dice_id = 
  match State.use_die st dice_id  with 
  | Invalid_Die st' -> ANSITerminal.(print_string [red] (String.concat ""
                                                           ["That die is ";
                                                            "unavailable, ";
                                                            "try again.";
                                                            "\n"]));                                                    
    print_string  "> ";
    st'
  | Changed_Die st' -> ANSITerminal.(print_string [green]
                                       ("\nNew dice chosen. \n"));
    print_string  "> ";
    st'
  | _ -> st

(** [result_roll brd st print_roll_res roll_res new_tile] is the new state that
    is reached in rolling a die in state [st]. *)
let result_roll brd st print_roll_res roll_res new_tile = 
  match roll_res with 
  | Normal_Roll st' -> print_roll_res st'; st'
  | Slid_Down_Snake st' -> print_roll_res st';
    ANSITerminal.(print_string [red] ("You've slide down a snake to "^
                                      new_tile st'^".\n")) ; st'
  | Went_Up_Ladder st' -> print_roll_res st';
    ANSITerminal.(print_string [blue] ("You've gone up a ladder to "^ 
                                       new_tile st'^".\n")) ; st'
  | Found_New_Die  st' -> print_roll_res st';
    ANSITerminal.(print_string [red] ("A new die was found. \n")) ; st'
  | Roll_Not_Valid st' -> print_roll_res st';
    ANSITerminal.(print_string [red]
                    (String.concat "" ["\n"; "That roll is not valid,";
                                       "you will exceed the number of tiles ";
                                       "on the board and ";"\n go into the ";
                                       "unknown. \n"])) ;
    print_string  "> "; st'
  |_ -> st

(** [handle_roll brd st] takes the parsed command and calls the state to update
    according to what the rolled tile contains (ie. snake, ladder, die, 
    nothing). *)
let handle_roll brd st = 
  let roll_res = State.roll brd st in
  let print_roll_res st = 
    let roll_val = State.last_roll st in
    ANSITerminal.(print_string [blue] ("\nRolled a "^roll_val^".\n"));
  in
  let new_tile st= State.prev_player_pos st in 
  result_roll brd st print_roll_res roll_res new_tile

(** [handle_show_dice brd st] prints out the list of the current player's       
    available dice. *)
let handle_show_dice brd st = 
  let dice = State.curr_dice st in
  (* Print here *)
  List.iter (fun x -> ANSITerminal.(
      print_string [blue] ("Die: ");
      print_string [green] (pp_string x ^"\n");
      print_string [blue] ("Faces: ");
      print_string [green] (pp_list pp_int (get_faces brd x) ^"\n");
      print_string [blue] ("Probabilities: ");
      print_string [green] (pp_list pp_float (get_probs brd x) ^"\n");
    )) dice;
  print_string  "> ";
  (* Return same state *)
  st

(** [print_move_pompt brd st] prints out the current tile location and the 
    faces of the selected die to the current player. *)
let print_move_prompt brd st = 
  let curr_player_str = string_of_int ((State.get_curr_player st)+1) in
  let curr_pos_str = string_of_int (State.curr_pos st) in 
  let curr_die_did = State.curr_die st in 
  let curr_die_faces = get_faces brd curr_die_did in
  let curr_die_probs = get_probs brd curr_die_did in
  let pp_faces = pp_list pp_int curr_die_faces in
  let pp_probs = pp_list pp_float curr_die_probs in
  ANSITerminal.(
    print_string [green] ("\nPlayer "^ curr_player_str ^" \n");
    print_string [blue] ("You're on tile "^curr_pos_str^".\n");
    print_string [blue] ("Currently in your hand is die "^curr_die_did^".\n");
    print_string [blue] ("It has faces: "^pp_faces^" with respective "^
                         "probabilities: "^pp_probs^"\n");
    print_string [red] ("Please choose an action\n");
  );
  print_string  "> ";
  ()

(** [make_move st brd] checks winning condition then takes the players terminal 
    command and handles the parsed commands. *)
let rec make_move brd st print_prompt: unit = 
  draw_game brd st;
  check_win_cond brd st;
  if ( is_bot st ) then (  
    let curr_player_str = string_of_int ((State.get_curr_player st)+1) in
    let curr_pos_str = string_of_int (State.curr_pos st) in 
    ANSITerminal.(print_string [green] ("\nBOT : Player "^curr_player_str^
                                        " is rolling. \n"));
    ANSITerminal.(print_string [blue] ("It is currently on tile "^
                                       curr_pos_str^"."));
    let st' brd st  = handle_pick_die brd st (bot_die brd st) in
    if (bot_die brd st = curr_die st) 
    then make_move brd (handle_roll brd st) true 
    else make_move brd (handle_roll brd (st' brd st)) true );
  if print_prompt then 
    print_move_prompt brd st;
  match read_line () with
  | exception End_of_file -> ()
  | input -> input_parser brd st input

and 
  (** [input_parser brd st input] advances to a new state on parsing [input] 
      into a game command. *)
  input_parser brd st input = 
  match Command.parse input with
  | Quit ->  exit 0
  | Pick_Die d_id -> 
    let st' = handle_pick_die brd st d_id in
    make_move brd st' false
  | Roll ->  
    let st' = handle_roll brd st in
    make_move brd st' true;
  | Show_Dice -> 
    let st' = handle_show_dice brd st in
    make_move brd st' false;
  | exception Empty ->  
    print_string ">";
    make_move brd st true;
  | exception Malformed -> 
    ANSITerminal.(print_string [red] 
                    ("Command was malformed, try again \n"));
    print_string  "> ";
    make_move brd st false 

(** [print_commands ()] prints the game commands into the terminal. *)
let print_commands () = 
  ANSITerminal.(print_string [red] ("Alongside the dice is a list of "^
                                    "incantations you can"^
                                    " say to bid you success"^
                                    "\nin your journey:\n"^
                                    "   * \"roll\"\n"^
                                    "   * \"use [id]\"\n"^
                                    "   * \"quit\"\n"^
                                    "   * \"show\"\n");)

(** [print_introduction brd fname nplayers] prints the game introduction into
    the terminal. *)
let print_introduction brd fname nplayers : unit = 
  let nplayers_str = string_of_int nplayers in
  let nsquares_str = string_of_int (Board.get_size brd - 1) in
  let start_did = start_die brd in
  let start_faces = get_faces brd start_did in
  let start_probs = get_probs brd start_did in
  ANSITerminal.( 
    print_string [yellow] 
      (String.concat "" ["Your flight to Brazil has taken an unfortunate turn.";
                         "\nYou crash landed in the amazon at square 0 and ";
                         "must progress "^nsquares_str^" squares \nto make ";
                         "it to rescue convoy that is "^
                         "about to depart!\n"]);
    print_string [blue] ("You and your fellow passengers, "^nplayers_str^
                         " of you in total - find a pouch of dice\n"^
                         "entitled: "^start_did^" that all have the"^ 
                         " same properties:\n");
    print_string [yellow] ("It has faces:\n");
    print_string [green] (pp_list pp_int start_faces ^"\n");
    print_string [yellow] ("Each respectively with probabilities:\n");
    print_string [green] (pp_list pp_float start_probs ^"\n"); )

(** [game_init f nplayers bot_num brd] initializes the game in file [f] with 
    board [brd], [n_players] normal players and [bot_num] bot players. *)
let game_init f nplayers bot_num brd = 
  if (nplayers <= 0 || bot_num < 0 || nplayers + bot_num <= 0 || 
      nplayers + bot_num > 5) then 
    begin 
      ANSITerminal.(print_string [red] "\nIllegal number of players!");
      failwith "Error" 
    end
  else begin
    let st = State.init_state brd nplayers bot_num  in
    print_introduction brd f (nplayers+bot_num);
    print_commands();
    draw_game_init brd st;
    make_move brd st true
  end

(** [game_inputs f] takes in the inputs for the game in file [f]. *)
let game_inputs (f: string) = 
  ANSITerminal.(print_string [yellow] ("Loading "^f^"...\n"));
  let j = Yojson.Basic.from_file f in
  ANSITerminal.(print_string [green] ("Successully loaded "^f^"!\n\n"));
  ANSITerminal.(print_string [red] (String.concat "" ["The total number of ";
                                                      "players and bots must";
                                                      " not exceed 5.\n"]));
  let board = Board.from_json j in
  let nplayers = 
    print_endline "Please enter the number of players";
    print_string  "> ";
    int_of_string (read_line ()) in 
  let bot_num  = 
    print_endline "Please enter the number of bots";
    print_string  "> ";
    int_of_string (read_line ()) in  
  game_init f nplayers bot_num board

(** [play_game f] starts the adventure in file [f]. *)
let play_game (f : string) : unit =
  try game_inputs f
  with e -> 
  match e with
  | Type_error (s, _) ->
    ANSITerminal.(print_string [red] (
        "\n\ Uh oh, "^f^" is in uncharted territory.\n\ 
        Let's not try to venture into "^f^" next time. \n\n"));
  | _ -> ANSITerminal.(print_string [red] (String.concat "" 
                                             ["\nSome error occured and you";
                                              " should probably panic. \n\n"] ))

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.
    (print_string [red] "\n\nWelcome to Snakes and Ladders.\n");
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name

(* Execute the game engine. *)
let () = main ()