(** This module holds commonly used functions. *)

(** [select_elem x l] takes an index [x] and a list [l] and returns the 
    element at that index. *)
val select_elem : int -> 'a list -> 'a

(** [swap_elem ind elm lst] takes an index [ind], element [elm] and list [lst]
    then swaps out the element at the index [ind] in [lst] with [elm]. *)
val swap_elem : int -> 'a -> 'a list -> 'a list

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
val cmp_set_like_lists : 'a list -> 'a list -> bool

(** [pp_string s] pretty-prints string [s]. *)
val pp_string : 'a -> 'a

(** [pp_int i] pretty-prints int [i]. *)
val pp_int : int -> string

(** [pp_float f] pretty-prints float [f]. *)
val pp_float : float -> string

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
val pp_list : ('a -> string) -> 'a list -> string 

(** [extract_opt x] is the string representation of an integer option. *)
val extract_opt : int option -> string

(** [extract_opt_str] is the string representation of an string option. *)
val extract_opt_str : string option -> string

(** [pp_list_list ] concatanates elements of a list with spaces after each 
    element. *) 
val pp_list_list : string -> string list list -> string