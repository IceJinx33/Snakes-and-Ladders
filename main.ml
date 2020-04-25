open Board
open State
open Yojson.Basic.Util


let indent = "    "

let execute cmd st =    
let di_choice adv st ob_phrase = 
  in    
let roll board st = 
  in 

  match cmd  with 
    | Quit -> exit 0
    | Roll -> roll board st  
    | Di ->  di_choice adv sta (String.concat " " op);

let parse_input str = 
    match Command.parse (read_line ()) with
    | exception Command.Malformed -> 
      ANSITerminal.(print_string [red] ("\nCommand was malformed, try again \n"));  make_move st;
    | exception Command.Empty -> 
      ANSITerminal.(print_string [red] ("\n Did not choose a di please try againn \n"));  make_move st;
    | command -> execute command st

| _ -> failwith "Command is Unimplemented"

let rec make_move st: unit = 
 match read_line () with
  | exception End_of_file -> ()
  | input -> 
    let cmd = parse_input input in
    execute cmd st

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
    make_move st

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