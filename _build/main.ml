open Board
open State
open Command
open Yojson.Basic.Util


let indent = "    "

(*     
let handle_die_choice board st dice_id = 
    match State.use_die st dice_id  with 
    |Invalid_Die st -> ANSITerminal.(print_string [red]
     ("\nThat die is unavaiable, try again. \n"));
       print_string  ">"; make_move st;
    |Changed_Die st -> ANSITerminal.(print_string [red]
     ("\n New dice chosen \n"));
       print_string  ">";
      make_move st ;
    | _ -> () *)

(* let execute cmd st board =    
  let handle_roll board st = 
  match State.roll board st with 
    |Normal_Roll st -> make_move st;
    |Slid_Down_Snake st -> ANSITerminal.(print_string [red] 
    ("\nYou've slide down a snake \n")) ;  print_string  ">"; make_move st;
    |Went_Up_Ladder st -> ANSITerminal.(print_string [red] 
    ("\nYou've gone up a ladder\n")) ;  print_string  ">"; make_move st;
    |Found_New_Die  st -> ANSITerminal.(print_string [red] 
    ("\nYou found a new die \n")) ;  print_string  ">"; make_move st;
    |Roll_Not_Valid st -> ANSITerminal.(print_string [red] 
    ("\n That roll is not valid, 
    your roll must be the exact spaces until the end \n")) ;
    print_string  ">"; make_move st;
    |_ -> ()
  in 
  match cmd with 
    | Quit -> exit 0
    | Roll -> handle_roll board st  
    | Pick_Die d_id ->  handle_die_choice board st d_id ; *)

let parse_input str = 
    match parse str with
    | Quit -> Quit
    | Roll -> Roll
    | Pick_Die d_id -> Pick_Die d_id
    | exception Malformed -> raise Malformed 
    | exception Empty -> raise Empty
    | _ -> failwith "Command is Unimplemented"

let print_malformed:()= 
  ANSITerminal.(print_string [red] ("\nCommand was malformed, try again \n"));
  
let print_empty:() =
  ANSITerminal.(print_string [red] ("\n Did not choose a die please try again \n"));

let rec make_move st board: unit = 
 match read_line () with
  | exception End_of_file -> ()
  | input -> 
    let cmd = parse_input input in
    match cmd with
    | Quit ->  exit 0;
    | Malformed -> print_malformed; make_move st board;
    | Empty ->  print_empty; make_move st board;
    | Pick_Die -> 
    | Roll ->  
    | move -> begin 
      match execute cmd st board with
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
    let st = State.init_state board in
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