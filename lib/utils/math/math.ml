(**
  Vérifie si le caractère [ch] est un nombre.
*)
let char_is_number (ch:char): bool =
  let ascii_code = int_of_char ch in 
    48 <= ascii_code && ascii_code <= 57 
;;