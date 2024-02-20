#use "rendu_etd.ml" ;;
Sys.command "clear" ;;

print_endline "Test tourner_case" ;;

let tour_0 : case = (2, -1, -1) ;;
let tour_1 = tourner_case 1 tour_0 ;;

let tour_2 = tourner_case 1 tour_1 ;;

let tour_3 = tourner_case 1 tour_2 ;;

let tour_4 = tourner_case 1 tour_3 ;;

let tour_5 = tourner_case 1 tour_4 ;;
let tour_6 = tourner_case 12 tour_0 ;;