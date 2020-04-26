open Board

(** The type [command] represents a player command that is decomposed
    into a verb and possibly an object phrase. *)
type command = 
  | Quit
  | Pick_Die of dice_id
  | Roll 

  (* | Inventory  *)

  (** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed
(** takes phrase as a list of lists and removes spaces from object phrases
so we are only left with list list of words *)
let rec filter_spaces stringlist= 
  match stringlist with 
  |[] -> []
  | h::t -> if h = "" then filter_spaces t else h::filter_spaces t 


(** takes input phrase as list and forms commands, rasises malformed if its not
written in the correct commannd format or di command is empty  *)
let parse str : command =
  let stringlist = String.split_on_char ' ' str in 
  let nospaces= filter_spaces stringlist in 

  match nospaces with
  | [] -> raise Empty
  | [cmd] -> 
    begin
    match cmd with
      |"quit" -> Quit
      |"roll" -> Roll
      (* | "inventory" -> Inventory *)
      | _ -> raise Malformed
    end
  | cmd::die_phrase -> 
    begin
    match cmd with
      |"die" -> if List.length die_phrase == 0 then raise Empty else Pick_Die (List.hd die_phrase)
      |_-> raise Malformed
    end
