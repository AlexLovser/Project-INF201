include Types
include Case
include StringUtils


(**
  Affiche une case [c] dans la sortie standart.
*)
let print_case (c:case): unit =
  print_string (string_of_case c)
;;


(**
  Affiche la liste [l] de type char dans la sortie standart.
*)
let print_char_list (l:char list): unit =
  let s = "[" in
  let rec f (l:char list): string =
    match l with
    | [] -> ""
    | t::q -> let s = string_of_char t in "\'" ^ s ^ "\'; " ^ f q in
  let s = s ^ f l in 
  let s = sub_end s 2 ^ "]" in print_endline s
;;

print_char_list ['a'; 'b'; 'c'; 'd'; 'e']

let rec ( ** ) (a:int) (n:int) : int = 
  match n with
  | 0 -> 1
  | n -> a * a ** (n - 1)
;;

let rec ( **. ) (a:float) (n:int) : float  = 
  match n with
  | 0 -> 1.
  | n -> a *. ( **. ) a (n - 1)
;;