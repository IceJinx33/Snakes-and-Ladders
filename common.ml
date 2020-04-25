let rec select_elem x lst = 
  match lst with
  | hd::tl -> if x = 0 then hd else select_elem (x-1) tl
  | _ -> failwith "index out of bounds"

let swap_elem ind elm lst = 
  let rec swap_helper (ind:int) (elm:'a) (acc:'a list) (lst:'a list) =
    match lst with
    | [] -> []
    | hd::tl -> if ind = 0 then List.concat [List.rev (elm::acc);tl]
    else swap_helper (ind-1) elm (hd::acc) tl
  in
  swap_helper ind elm [] lst