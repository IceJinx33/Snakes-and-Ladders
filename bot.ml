open Board
open State

(** [prob_of_die brd current_pos die] is the probability of rolling die with 
    identifier [d]. *)
let prob_of_die brd current_pos die : float = 
  let curr_die_faces = Board.get_faces brd die in
  let curr_die_probs = Board.get_probs brd die in
  let roll_landed x = x + current_pos in 
  let die_f_dist = List.map (fun x -> Board.additional_move brd 
                                (if(roll_landed x >= (Board.get_size brd)) 
                                 then 0 else roll_landed x)) curr_die_faces in 
  let p_of_die_face = List.map2 (fun x y -> x *. float_of_int(y)) 
      curr_die_probs die_f_dist in
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
    in [problst]. *)
let rec max_index problst maxprob index : int= 
  match problst with 
  | [] -> index
  | hd :: tl -> if hd = maxprob then index else max_index tl maxprob (index+1)

let rec bot_die brd st = 
  let current_pos = State.curr_pos st in 
  let bot_dice= List.nth (State.dice_list st) (State.get_curr_player st) in
  let maxprob = List.fold_left(fun x acc -> max x acc) 0.0 
      (problst brd current_pos bot_dice) in 
  List.nth bot_dice (max_index (problst brd current_pos bot_dice) maxprob 0)


