open Board

type command = 
  | Quit
  | Roll 
  | Show_Dice 
  | Pick_Die of dice_id

exception Empty

exception Malformed

let rec filter_spaces stringlist = 
  match stringlist with 
  | [] -> []
  | h::t -> if h = "" then filter_spaces t else h::filter_spaces t 

(** [parse_helper cmd] converts a single input phrase [cmd] and converts it to 
    a game command. 
    Raises: [Malformed] if the [cmd] represents an invalid command. *)
let parse_helper cmd = 
  match cmd with
  |"quit" -> Quit
  |"roll" -> Roll
  |"show" -> Show_Dice
  | _ -> raise Malformed

let parse str : command =
  let stringlist = String.split_on_char ' ' str in 
  let nospaces= filter_spaces stringlist in 
  match nospaces with
  | [] -> raise Empty
  | [cmd] -> parse_helper cmd
  | cmd::die_phrase -> 
    begin
      match cmd with
      |"use" -> 
        if List.length die_phrase = 1 then  
          Pick_Die (List.hd die_phrase) else raise Malformed
      |_-> raise Malformed
    end
