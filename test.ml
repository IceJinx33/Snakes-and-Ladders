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
      (* the [cmp] tells OUnit to check that [dice_ids brd] and 
         [expected output] are equivalent set-like lists *)
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal ~cmp: cmp_set_like_lists ~printer: (pp_list pp_string)
        expected_output (dice_ids brd))

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

(********************************************************************
   End helper functions for Board module. 
 ********************************************************************)

(** Tests for Board module. *)

let json = Yojson.Basic.from_file "game3.json"
(** [brd] is the tree representing the game board. *)
let brd = Board.from_json json

let boardTests = [
  dice_ids_test "The list of dice is " brd ["1";"2";"3"];
  find_locate_test "The location of dice 1 is " brd "1" 0;
  "find_locate_test exception UnknownDie " >:: 
  (fun _ -> assert_raises (UnknownDie "5") 
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

let tests = [boardTests; commandTests]

let suite =
  "test suite for Snakes and Ladders"  >::: List.flatten tests

let _ = run_test_tt_main suite