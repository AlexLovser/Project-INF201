#use "rendu_etd.ml" ;;
Sys.command "clear" ;;

let dim : dimension = 3 ;;

(**
  Vérifie si les coordonnées des cases [c1] et [c2] sont égals.
*)
let case_identique (c1:case) (c2:case): bool =
  let i1, j1, k1 = c1
  and i2, j2, k2 = c2 in
    i1 == i2 && j1 == j2 && k1 == k2 
;;

let mult_case_entier ((i,j,k):case) (n:int): case =
  i * n, j * n, k * n
;;

(* i < -dim *)
assert (est_case (-2 * dim, +1 * dim, +1 * dim)) ;; 

(* i > dim *)
assert (est_case (+2 * dim, -1 * dim, -1 * dim)) ;; 

(* j < -dim*)
assert (est_case (+1 * dim, -2 * dim, +1 * dim)) ;; 

(* (i,j,k) = (2dim, -dim, -dim) *)
assert (est_case (+2 * dim, -1 * dim, -1 * dim)) ;; 

(* (i,j,k) = (-dim - 1, 1, dim) *)
assert (est_case (-dim - 1, 1, dim)) ;;  

(* i >= -dim && j >= -dim && k >= -dim *)
assert (est_case (-dim, -dim, 2*dim)) ;; 


(* Testing "tourner_case" *)
let test_case_0 : case = ( 2, -1, -1) ;;
let test_case_1 : case = ( 1, -2,  1) ;;
let test_case_2 : case = (-1, -1,  2) ;;
let test_case_3 : case = (-2,  1,  1) ;;
let test_case_4 : case = (-1,  2, -1) ;;
let test_case_5 : case = ( 1,  1, -2) ;;

assert (case_identique (tourner_case 1 test_case_0) test_case_1) ;;
assert (case_identique (tourner_case 2 test_case_0) test_case_2) ;;
assert (case_identique (tourner_case 3 test_case_0) test_case_3) ;;
assert (case_identique (tourner_case 4 test_case_0) test_case_4) ;;
assert (case_identique (tourner_case 5 test_case_0) test_case_5) ;;
assert (case_identique (tourner_case 6 test_case_0) test_case_0) ;;


(* Testing "calcul_pivot" *)
calcul_pivot ( 0,-1, 1) ( 0, 1,-1) ;;
calcul_pivot (-2, 0, 2) ( 0, 0, 0) ;;
calcul_pivot (-2, 2, 0) ( 0, 0, 0) ;;

(* Testing "vec_et_dist" *)
let v, d = vec_et_dist ( 0, 2,-2) ( 0, 0, 0) ;;
translate (mult_case_entier v d) (0, 2,-2) ;;

let v, d = vec_et_dist ( 0, 6,-6) ( 0,-6, 6) ;;
translate (mult_case_entier v d) (0, 6,-6) ;;