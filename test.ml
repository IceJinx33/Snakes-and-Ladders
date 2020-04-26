open OUnit2
open Board
open Yojson.Basic
open Common

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

(********************************************************************
   End helper functions for Board module. 
 ********************************************************************)

let json = Yojson.Basic.from_file "game2.json"
(** [brd] is the tree representing the game board. *)
let brd = Board.from_json json

let boardTests = [
  find_locate_test "The location of dice 1 is " brd "1" 1;
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
  add_m_test "Ladder moves player from tile 2 to 4 " brd 2 4;
  add_m_test "Ladder doesn't move player from tile 4 to 2 " brd 4 4;
  add_m_test "No ladder - player stays at same tile " brd 3 3;
  b_size_test "Size of the board is " brd 64;
]

let tests = [boardTests]

let suite =
  "test suite for Snakes and Ladders"  >::: List.flatten tests

let _ = run_test_tt_main suite