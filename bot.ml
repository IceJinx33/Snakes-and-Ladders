(********************************************************************
   The Bot.

   This module contains the functionality for a game bot.
 ********************************************************************)

open Board
open State
open Common
open Command
open Yojson.Basic.Util
open Gui

(** [prob_of_die brd current_pos die] is the probibility of rolling die with 
    identifier [d]. *)
let prob_of_die brd current_pos die : float = 
  let curr_die_faces = Board.get_faces brd die in
  let curr_die_probs = Board.get_probs brd die in
  let die_face_distance =  List.map (fun x -> Board.additional_move brd 
                                        (x +current_pos)) curr_die_faces in 
  let p_of_die_face = List.map2 (fun x y -> x *. float_of_int(y)) 
      curr_die_probs die_face_distance in
  let totalprob = List.fold_left (fun x acc -> x +. acc ) 0.0  p_of_die_face in 
  totalprob

(** [problst brd current_pos bot_dice] is a list of the probablilities of 
    using each die in [bot_dice] given the bot's current position 
    [current_pos]. *)
let rec problst brd current_pos bot_dice : float list = 
  match bot_dice with 
  | [] -> []
  | hd::tl -> [prob_of_die brd current_pos hd ] @ (problst brd current_pos tl)

(** [max_index problst maxprob index] is the index of the greatest probability 
    in the [problst]. *)
let rec max_index problst maxprob index : int= 
  match problst with 
  | [] -> index
  | hd :: tl -> if hd = maxprob then index else max_index tl maxprob (index+1)

(** [bot_die brd st is the identifier of the die with the most beneficial roll 
    given board [brd] and state [st]. *)
let rec bot_die brd st = 
  print_endline "entered bot die";
  let current_pos = State.curr_pos st in 
  print_endline "git curr pos";
  let bot_dice= List.nth (State.dice_list st) (State.get_curr_player st) in
  print_endline "entered bot_dice";
  let maxprob = List.fold_left(fun x acc -> max x acc) 0.0 
      (problst brd current_pos bot_dice) in 
  print_endline "got max prob";
  (* print_string  "bots dice "; pp_list print_string bot_die; *)
  (* print_string  "/n best choice "; print_string (List.nth bot_dice (max_index (problst brd bot_dice) maxprob 0));
     print_string  "/n current choice "; print_string (curr_die st); *)
  List.nth bot_dice (max_index (problst brd current_pos bot_dice) maxprob 0)


