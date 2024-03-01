include Types
include StringUtils

(**
  Renvoie une chaîne de caractères qui représente la case [c].
*)
let string_of_case (c:case): string =
  let i, j, k = c in "(" ^ (string_of_int i) ^ " " ^ (string_of_int j) ^ " " ^ (string_of_int k) ^ ")"


(**
  Renvoie une liste de type char avec les caractères de [s].
*)
let rec char_list_of_string (s:string): char list = 
  match s with
  | "" -> []
  | s ->  let ch = (char_of_string s 0)
          and s = sub_beg s 1 in 
            ch::char_list_of_string s
;;