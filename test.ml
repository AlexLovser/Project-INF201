#use "rendu_etd.ml" ;;
Sys.command "clear" ;;

(* Func for testing  *)
let case_identique ((c11,c12,c13):case) ((c21,c22,c23):case): bool =
  c11 == c21 && c12 == c22 && c13 == c23 
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