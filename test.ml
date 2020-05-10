open OUnit2
open Yojson.Basic
open Board
open Common
open Command
open State

(********************************************************************
   Helper functions for testing the Board module. 
 ********************************************************************)

(** [dice_ids_test name brd expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [dice_ids brd]. *)
let dice_ids_test 
    (name : string) 
    (brd: Board.t) 
    (expected_output : dice_id list) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal ~printer: (pp_list pp_string) expected_output (dice_ids brd))

(** [find_locate_test name brd d expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [find_locate brd d]. *)
let find_locate_test 
    (name : string) 
    (brd: Board.t) 
    (d: dice_id)
    (expected_output : tile_id) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal ~printer: (string_of_int) expected_output (find_locate brd d))

(** [start_die_test name brd expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [start_die brd]. *)
let start_die_test 
    (name : string) 
    (brd: Board.t) 
    (expected_output : dice_id) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output (start_die brd) ~printer: pp_string)

(** [add_m_test name brd pos expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [additional_move brd pos]. *)
let add_m_test 
    (name : string) 
    (brd: Board.t) 
    (pos: tile_id)
    (expected_output : tile_id) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal ~printer: (string_of_int) expected_output 
        (additional_move brd pos))

(** [b_size_test name brd expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [additional_move brd pos]. *)
let b_size_test 
    (name : string) 
    (brd: Board.t) 
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal ~printer: (string_of_int) expected_output 
        (get_size brd))

(** [get_die_t_test name brd expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [get_die_at_tile brd pos]. *)
let get_die_t_test 
    (name : string) 
    (brd: Board.t) 
    (pos: Board.tile_id) 
    (expected_output : string option) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal ~printer: extract_opt_str expected_output 
        (get_die_at_tile brd pos))

(** [die_faces_test name brd d expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [get_faces brd d]. *)
let die_faces_test 
    (name : string) 
    (brd: Board.t) 
    (d : dice_id)
    (expected_output : face list) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal ~printer: (pp_list pp_int) expected_output (get_faces brd d))

(** [die_probs_test name brd d expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [get_probs brd d]. *)
let die_probs_test 
    (name : string) 
    (brd: Board.t) 
    (d : dice_id)
    (expected_output : prob list) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal ~printer: (pp_list pp_float) expected_output 
        (get_probs brd d))

(** [ladder_tile_test name brd d expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [get_ladders_tiles brd d]. *)
let ladder_tile_test 
    (name : string) 
    (brd: Board.t) 
    (expected_output : (tile_id*tile_id) list) : test = 
  (* the [cmp] tells OUnit to check that [get_ladders_tiles brd] and 
       [expected output] are equivalent set-like lists *)
  name >:: (fun _ -> assert_equal ~cmp: cmp_set_like_lists 
               expected_output (get_ladders_tiles brd))

(** [snake_tile_test name brd d expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [get_ladders_tiles brd d]. *)
let snake_tile_test 
    (name : string) 
    (brd: Board.t) 
    (expected_output : (tile_id*tile_id) list) : test = 
  (* the [cmp] tells OUnit to check that [get_snakes_tiles brd] and 
       [expected output] are equivalent set-like lists *)
  name >:: (fun _ -> assert_equal ~cmp: cmp_set_like_lists 
               expected_output (get_snakes_tiles brd))

(********************************************************************
   End helper functions for Board module. 
 ********************************************************************)

(** Tests for Board module. *)

let json = Yojson.Basic.from_file "game3.json"
(** [brd] is the tree representing the above game board. *)
let brd = Board.from_json json

let boardTests = [
  dice_ids_test "The list of dice is " brd ["1";"2";"3";"4";"5"];
  find_locate_test "The location of dice 1 is " brd "1" 0;
  "find_locate_test exception UnknownDie " >:: 
  (fun _ -> assert_raises (UnknownDie "9") 
      (fun () -> find_locate brd "9"));
  "find_locate_test exception TileOutOfRange " >:: 
  (fun _ -> assert_raises (TileOutOfRange 64) 
      (fun () -> find_locate brd "4"));
  "find_locate_test exception TileOutOfRange " >:: 
  (fun _ -> assert_raises (TileOutOfRange (-1)) 
      (fun () -> find_locate brd "5"));
  start_die_test "The starting die is " brd "1";
  "add_m_test exception TileOutOfRange " >:: 
  (fun _ -> assert_raises (TileOutOfRange 64) 
      (fun () -> additional_move brd 64));
  "add_m_test exception TileOutOfRange " >:: 
  (fun _ -> assert_raises (TileOutOfRange (-1)) 
      (fun () -> additional_move brd (-1)));
  add_m_test "Snake moves player from tile 9 to 2 " brd 9 2;
  add_m_test "Snake doesn't move player from tile 2 to 9 " brd 2 2;
  add_m_test "No snake - player stays at same tile " brd 7 7;
  add_m_test "Ladder moves player from tile 3 to 4 " brd 3 4;
  add_m_test "Ladder doesn't move player from tile 4 to 3 " brd 4 4;
  add_m_test "No ladder - player stays at same tile " brd 8 8;
  b_size_test "Size of the board is " brd 64;
  get_die_t_test "Die at tile 0 is " brd 0 (Some "1");
  get_die_t_test "Die at tile 5 is " brd 5 None;
  {|get_die_t_test Failure "Multiple Dice occupying one tile!" |} >:: 
  (fun _ -> assert_raises (Failure "Multiple Dice occupying one tile!") 
      (fun () -> get_die_at_tile brd 6));
  die_faces_test "Test faces of die with id 2" brd "2" [1;3;1;3;1;3];
  "die_faces_test exception UnknownDie " >:: 
  (fun _ -> assert_raises (UnknownDie "9") 
      (fun () -> get_faces brd "9"));
  die_probs_test "Test probabilities of die with id 2" brd "2" 
    [0.18;0.17;0.17;0.16;0.16;0.16];
  "die_probs_test exception UnknownDie " >:: 
  (fun _ -> assert_raises (UnknownDie "6") 
      (fun () -> get_probs brd "6"));
  ladder_tile_test "Ladder tiles of board (bottom,top) " brd [(3,4);(36,50)];
  snake_tile_test "Snake tiles of board (tail,head) " brd [(2,9);(35,51)]
]

(** End tests for Board module. *)

(********************************************************************
   Helper functions for testing the Command module. 
 ********************************************************************)

(** [pp_command c] is the string representation of command [c]. *)
let pp_command c = 
  match c with 
  | Quit -> "Quit"
  | Roll -> "Roll"
  | Show_Dice -> "Show dice"
  | Pick_Die d -> "Pick die "^d

(** [filter_test name str expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [filter_spaces str]. *)
let filter_test 
    (name : string)
    (str: string list)
    (expected_output : string list) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal ~printer: (pp_list pp_string) 
        expected_output (filter_spaces str))

(** [parse_test name str expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [parse str]. *)
let parse_test 
    (name : string)
    (str: string)
    (expected_output : command) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output (parse str) ~printer: pp_command)

(********************************************************************
   End helper functions for the Command module. 
 ********************************************************************)

(** Tests for Command module. *)

let commandTests = [
  filter_test "No empty strings" ["a";"b";"c"] ["a";"b";"c"];
  filter_test "One empty string" ["a";"";"c"] ["a";"c"];
  filter_test "All empty strings" ["";"";"";""] [];
  parse_test "Quit command " "   quit     " Quit;
  parse_test "Roll command " " roll     " Roll;
  parse_test "Show_Dice command " "    show " Show_Dice;
  parse_test "Pick_Die d command " "    use  1" (Pick_Die "1");
  "parse_test Empty Exception - empty string " >:: 
  (fun _ -> assert_raises (Empty) 
      (fun () -> parse ""));
  "parse_test Empty Exception - string with only spaces " >:: 
  (fun _ -> assert_raises (Empty) 
      (fun () -> parse "               "));
  "parse_test Malformed Exception - action not in approved list of actions" 
  >:: (fun _ -> assert_raises (Malformed) 
          (fun () -> parse "hello"));
  "parse_test Malformed Exception - action not in approved list of actions" 
  >:: (fun _ -> assert_raises (Malformed) 
          (fun () -> parse "hold 1 2"));
  {|parse_test Malformed Exception - "quit home"|} >:: 
  (fun _ -> assert_raises (Malformed) 
      (fun () -> parse "quit home"));
  {|parse_test Malformed Exception - "roll     die"|} >:: 
  (fun _ -> assert_raises (Malformed) 
      (fun () -> parse "roll     die"));
  {|parse_test Malformed Exception - Pick multiple die|} >:: 
  (fun _ -> assert_raises (Malformed) 
      (fun () -> parse "use 1 2"));
]

(** End tests for Command module. *)

(********************************************************************
   Helper functions for testing the State module. 
 ********************************************************************)

(** [won_test name st expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [check_won st]. *)
let won_test 
    (name : string) 
    (st: State.t) 
    (expected_output : bool) : test = 
  (* the [printer] tells OUnit how to convert the output to a string *)
  name >:: (fun _ -> assert_equal ~printer: string_of_bool 
               expected_output (check_won st))

(** [won_test name st expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [check_won st]. *)
let b_list_test 
    (name : string) 
    (st: State.t) 
    (expected_output : bool list) : test = 
  name >:: (fun _ ->
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal ~printer: (pp_list string_of_bool) 
        expected_output (bot_list st))

(** [num_players_test name st expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [n_players st]. *)
let num_players_test 
    (name : string) 
    (st: State.t) 
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal ~printer: (string_of_int) 
        expected_output (n_players st))

(** [cur_p_test name st expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [get_curr_player st]. *)
let cur_p_test 
    (name : string) 
    (st: State.t) 
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal ~printer: (string_of_int) 
        expected_output (get_curr_player st))

(** [players_die_test name st expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [dice_list st]. *)
let players_die_test 
    (name : string) 
    (st: State.t) 
    (expected_output : dice_id list list) : test = 
  name >:: (fun _ -> assert_equal expected_output (dice_list st))

(** [sel_die_test name st expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [selected_die st]. *)
let sel_die_test 
    (name : string) 
    (st: State.t) 
    (expected_output : dice_id list) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal ~printer: (pp_list pp_string)
        expected_output (selected_die st))

(** [pl_pos_test name st expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [players_pos st]. *)
let pl_pos_test 
    (name : string) 
    (st: State.t) 
    (expected_output : tile_id list) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal ~printer: (pp_list string_of_int)
        expected_output (players_pos st))

(** [last_r_test name st expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [last_roll st]. *)
let last_r_test 
    (name : string) 
    (st: State.t) 
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal ~printer: (pp_string)
        expected_output (last_roll st))

(** [c_pos_test name st expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [curr_pos st]. *)
let c_pos_test 
    (name : string) 
    (st: State.t) 
    (expected_output : tile_id) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal ~printer: (string_of_int)
        expected_output (curr_pos st))

(** [c_dice_test name st expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [curr_dice st]. *)
let c_dice_test 
    (name : string) 
    (st: State.t) 
    (expected_output : dice_id list) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal ~printer: (pp_list pp_string)
        expected_output (curr_dice st))

(** [c_die_test name st expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [curr_die st]. *)
let c_die_test 
    (name : string) 
    (st: State.t) 
    (expected_output : dice_id) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal ~printer: (pp_string)
        expected_output (curr_die st))

(** [pr_pos_test name st expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [prev_player_pos st]. *)
let pr_pos_test 
    (name : string) 
    (st: State.t) 
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal ~printer: pp_string
        expected_output (prev_player_pos st))

(********************************************************************
   End helper functions for State module. 
 ********************************************************************)

let json2 = Yojson.Basic.from_file "game2.json"
(** [brd2] is the tree representing the above game board. *)
let brd2 = Board.from_json json2
(** [st1] is the initial state of the game. *)
let st1 = State.init_state brd2 2 1

let stateTests = [
  won_test "Check init state for won " st1 false;
  b_list_test "Check if botlist is correctly initialized " st1 
    [false;false;true];
  num_players_test "Initialize no. of players correctly " st1 3;
  cur_p_test "Index of current player " st1 0;
  players_die_test "Initial list of each player's dice list " st1 
    [["1"];["1"];["1"]];
  sel_die_test "Currently selected die of all players " st1 ["1";"1";"1"];
  pl_pos_test "Initial positions of players " st1 [0;0;0];
  last_r_test "No rolls before init " st1 "None";
  c_pos_test "Current position of current player " st1 0;
  c_dice_test "Current player's dice list " st1 ["1"];
  c_die_test "Current player's selected die " st1 "1";
  pr_pos_test "Previous position of current player " st1 "None"
]

let tests = [boardTests; commandTests; stateTests]

let suite =
  "test suite for Snakes and Ladders"  >::: List.flatten tests

let _ = run_test_tt_main suite