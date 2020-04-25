(**  Parsing of player commands.
*)


(** The type [object_phrase] represents the object phrase that can be part of a 
    player command.  Each element of the list represents a word of the object 
    phrase, where a {i word} is defined as a consecutive sequence of non-space 
    characters. No element of the list should contain any leading,
    internal, or trailing spaces.  The list is in the same order as the words 
    in the original player command.  For example:
    - If the player command is ["di 2"], then the object phrase is 
      ["1"].
    - If the player command is ["di      1"], then the object phrase is
      again [["1"]]. 

    An [object_phrase] is not permitted to be the empty list. *)


(** The type [command] represents a player command that is decomposed
    into a verb and possibly an object phrase. *)
type command = 
  | Roll 
  | Quit
  | Pick_Die of dice_id
  (* | Inventory  *)

  (** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

val parse : string -> command

val filter_spaces: string list -> string list 
(* You are free to add more code here. *)
