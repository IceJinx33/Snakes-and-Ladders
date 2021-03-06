let rec select_elem x lst = 
  match lst with
  | hd::tl -> if x = 0 then hd else select_elem (x-1) tl
  | _ -> failwith "index out of bounds"

let swap_elem ind elm lst = 
  let rec swap_helper (ind:int) (elm:'a) (acc:'a list) (lst:'a list) =
    match lst with
    | [] -> failwith "index out of bounds"
    | hd::tl -> if ind = 0 then List.concat [List.rev (elm::acc);tl]
      else swap_helper (ind-1) elm (hd::acc) tl
  in
  swap_helper ind elm [] lst

let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

let pp_string s = s

let pp_int i = string_of_int i

let pp_float f = string_of_float f

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

let extract_opt x = 
  match x with 
  | None -> "None"
  | Some v -> "Some "^(string_of_int v)

let extract_opt_str x = 
  match x with 
  | None -> "None"
  | Some v -> "Some "^v

let rec pp_list_list acc lst = 
  match lst with 
  | [] -> acc 
  | [h] -> acc^" "^(pp_list pp_string h)
  | h1::(h2::t as t') -> pp_list_list (acc^" "^(pp_list pp_string h1)) t'



