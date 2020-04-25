open OUnit2
open Board
open Yojson.Basic

(********************************************************************
   General helper functions. 
 ********************************************************************)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""
(** [pp_kv elt] pretty-prints tuple [elt]. *)
let pp_kv elt = "("^(string_of_int(fst elt))^","^(string_of_float(snd elt))

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1::(h2::t as t') ->
        if n=100 then acc ^ "..."  (* stop printing long list *)
        else loop (n+1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(********************************************************************
   End general helper functions. 
 ********************************************************************)

(********************************************************************
   Helper functions for testing the Board module. 
 ********************************************************************)

(** [tile_ids_test name brd expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [tile_ids brd]. *)
let tile_ids_test 
    (name : string) 
    (brd: Board.t) 
    (expected_output : tile_id list) : test = 
  name >:: (fun _ -> 
      (* the [cmp] tells OUnit to check that [tile_ids brd] and 
         [expected output] are equivalent set-like lists *)
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal ~printer: (pp_list pp_string) expected_output (tile_ids brd))

(** [start_tile_test name brd expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [start_tile brd]. *)
let start_tile_test 
    (name : string) 
    (brd: Board.t) 
    (expected_output : tile_id) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output (start_tile brd) ~printer: pp_string)

(** [win_tile_test name brd expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [win_tile brd]. *)
let win_tile_test 
    (name : string) 
    (brd: Board.t) 
    (expected_output : tile_id) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output (win_tile brd) ~printer: pp_string)

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

(** [die_vals_test name brd d expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [die_vals brd d]. *)
let die_vals_test 
    (name : string) 
    (brd: Board.t) 
    (d: dice_id)
    (expected_output : (int*float) list) : test = 
  name >:: (fun _ -> 
      (* the [cmp] tells OUnit to check that [die_vals brd d] and 
         [expected output] are equivalent set-like lists *)
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal ~cmp: cmp_set_like_lists ~printer: (pp_list pp_kv)
        expected_output (die_vals brd d))

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
      assert_equal ~printer: (pp_string) expected_output (find_locate brd d))

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

(** [snake_m_test name brd t expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [find_locate brd d]. *)
let snake_m_test 
    (name : string) 
    (brd: Board.t) 
    (t: tile_id)
    (expected_output : tile_id) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal ~printer: (pp_string) expected_output (snake_move brd t))

(** [ladder_m_test name brd t expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [ladder_move brd d]. *)
let ladder_m_test 
    (name : string) 
    (brd: Board.t) 
    (t: tile_id)
    (expected_output : tile_id) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal ~printer: (pp_string) expected_output (ladder_move brd t))

(********************************************************************
   End helper functions for Board module. 
 ********************************************************************)

let json = from_file "game1.json"
(** [brd] is the tree representing the game board. *)
let brd = from_json json

let boardTests = [
  tile_ids_test "The tile identifiers are " brd 
    ["1";"2";"3";"4";"5";"6";"7";"8";"9";"10"];
  start_tile_test "The start tile is " brd "1";
  win_tile_test "The win tile is " brd "10";
  dice_ids_test "The dice identifiers are " brd ["1"];
  die_vals_test "The die face and probabilities are " brd "1" 
    [(1,0.18);(3,0.17);(2,0.17);(4,0.16);(5,0.16);(6,0.16)];
  "die_vals_test exception UnknownDie " >:: 
  (fun _ -> assert_raises (UnknownDie "5") 
      (fun () -> die_vals brd "5"));
  find_locate_test "The location of dice 1 is " brd "1" "1";
  "find_locate_test exception UnknownDie " >:: 
  (fun _ -> assert_raises (UnknownDie "5") 
      (fun () -> find_locate brd "5"));
  start_die_test "The start die is " brd "1";
  snake_m_test "Snake moves player from tile 9 to 2 " brd "9" "2";
  snake_m_test "Snake doesn't move player from tile 2 to 9 " brd "2" "2";
  snake_m_test "No snake - player stays at same tile " brd "7" "7";
  "snake_m_test exception UnknownTile " >:: 
  (fun _ -> assert_raises (UnknownTile "12") 
      (fun () -> snake_move brd "12"));
  ladder_m_test "Ladder moves player from tile 2 to 4 " brd "2" "4";
  ladder_m_test "Ladder doesn't move player from tile 4 to 2 " brd "4" "4";
  ladder_m_test "No ladder - player stays at same tile " brd "3" "3";
  "ladder_m_test exception UnknownTile " >:: 
  (fun _ -> assert_raises (UnknownTile "12") 
      (fun () -> ladder_move brd "12"));
]

let tests = [boardTests]

let suite =
  "test suite for Snakes and Ladders"  >::: List.flatten tests

let _ = run_test_tt_main suite