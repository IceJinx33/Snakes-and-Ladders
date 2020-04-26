open Board
(********************************************************************
  This module parses player commands.
********************************************************************)

(** The type [command] represents a player command that is decomposed
    into a verb and possibly an object phrase. Possible commands are
    [Quit] [Show_Dice] [Pick_Die d] and [Roll]. *)
type command = 
  | Quit
  | Roll 
  | Show_Dice 
  | Pick_Die of dice_id
  
(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] takes input phrase [str] matches it to the correct 
command.
Raises: [Empty] if [str] is the empty string or only contains spaces.
Raises: [Malformed] if [str] not written in the correct command format. 
        A command is malformed if it does not satisfy at least one if the 
        following: 
        1. On removing leading and trailing spaces, [str] is "quit" or "roll"
           or "show".
        2. On removing leading and trailing spaces, [str] has an action "use" 
        followed by a non-empty die phrase that may be the identifier of the 
        die the player wants to pick.
*)
val parse : string -> command

(** [filter_spaces strlist] is the list of strings in [strlist] that are 
not empty. *)
val filter_spaces: string list -> string list 

