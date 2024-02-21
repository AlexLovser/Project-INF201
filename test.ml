#use "rendu_etd.ml" ;;
Sys.command "clear" ;;

(* Functions for testing  *)
let case_identique ((c11,c12,c13):case) ((c21,c22,c23):case): bool =
  c11 == c21 && c12 == c22 && c13 == c23 
;;

let case_different (c1:case) (c2:case): bool =
  not (case_identique c1 c2)
;;

let mult_case_entier ((i,j,k):case) (n:int): case =
  i * n, j * n, k * n
;;

Random.self_init () ;;
let random_int (a:int) (b:int): int = (* a >= 0 and b >= 0 *)
  a + Random.int (b - a + 1)
;;


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