(****************************************************************************)
(*                                                                          *)
(****************************************************************************)


include Types


(**
  Renvoie une chaine de caracteres qui represente la case [c].
*)
let string_of_case (c:case): string =
  let i, j, k = c in 
    "(" ^ (string_of_int i) ^ 
    " " ^ (string_of_int j) ^ 
    " " ^ (string_of_int k) ^ ")"


(**
  Renvoie une chaine de caracteres qui represente la couleur [col].
*)
let string_of_couleur (col:couleur):string =
  match col with
  | Libre  -> " . "
  | Vert   -> " V "
  | Jaune  -> " J "
  | Rouge  -> " R "
  | Noir   -> " N "
  | Bleu   -> " B "
  | Marron -> " M "
  | Code s -> s  ;;


(**
  Verifie si le caractere [ch] est un nombre.
*)
let char_is_number (ch:char): bool =
  let ascii_code = int_of_char ch in 
    48 <= ascii_code && ascii_code <= 57 
;;


(**
  Renvoie le premier element de la chaine de caracteres [s] en type char.
*)
let char_of_string (s:string) (pos:int): char = 
  String.get s pos ;;


(**
  Renvoie une chaine de caracteres [string] qui est le representation d'une
  caractere [char].
*)
let string_of_char (ch:char): string =
  String.make 1 ch
;;
  

(**
  Renvoie le premier element de la chaine de caracteres [s] en type string.
*)
let first_element (s:string): string = 
  String.make 1 (char_of_string s 0) ;;


(**
  Renvoie la sub de chaine de caracteres de [pos] jusqu'a a la fin.
*)
let sub_beg (s:string) (pos:int): string =
  let len = String.length s in
  let len = len - pos in 
    String.sub s pos len 
;;


(**
  Renvoie la sub de chaine de caracteres a partir de la fin par [pos].
*)
let sub_end (s:string) (pos:int): string =
  let len = String.length s in
  let len = len - pos in 
    String.sub s 0 len  (* "0123456789" 0 -> "0123456789" *)
;;


(**
  Renvoie une liste de type char avec les caracteres de [s].
*)
let rec char_list_of_string (s:string): char list = 
  match s with
  | "" -> []
  | s ->  let ch = (char_of_string s 0)
          and s = sub_beg s 1 in 
            ch::char_list_of_string s
;;