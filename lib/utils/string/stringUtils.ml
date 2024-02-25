(**
  Renvoie le premier élement de la chaîne de caractères [s] en type char.
*)
let char_of_string (s:string) (pos:int): char = 
  String.get s pos ;;


(**
  Renvoie une chaîne de caractères [string] qui est le représentation d'une
  caractère [char].
*)
let string_of_char (ch:char): string =
  String.make 1 ch
;;
  

(**
  Renvoie le premier élement de la chaîne de caractères [s] en type string.
*)
let first_element (s:string): string = 
  String.make 1 (char_of_string s 0) ;;


(**
  Renvoie la sub de chaîne de caractères de [pos] jusqu'a à la fin.
*)
let sub_beg (s:string) (pos:int): string =
  let len = String.length s in
  let len = len - pos in 
    String.sub s pos len 
;;


(**
  Renvoie la sub de chaîne de caractères a partir de la fin par [pos].
*)
let sub_end (s:string) (pos:int): string =
  let len = String.length s in
  let len = len - pos in 
    String.sub s 0 len  (* "0123456789" 0 -> "0123456789" *)
;;