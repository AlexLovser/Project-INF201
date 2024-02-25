(** 
  Renvoie le maximum entre trois entiers [a], [b] et [c]
*)
let max3 (a:int) (b:int) (c:int): int =
  max a (max b c)
;;


(** 
  Renvoie un entier aleatoire entre les bornes [a] et [b] inclus
*)
let random_int (a:int) (b:int): int = (* a >= 0 and b >= 0 *)
  a + Random.int (b - a + 1)
;;


(**
  Vérifie si le caractère [ch] est un nombre.
*)
let char_is_number (ch:char): bool =
  let ascii_code = int_of_char ch in 
    48 <= ascii_code && ascii_code <= 57 
;;