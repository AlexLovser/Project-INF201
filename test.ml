#use "inf201_ElKortbi_Tabolskii_Bendouha_Caille_Crelerot_Q1-Q9.ml" ;;
Sys.command "clear" ;;


(* Variables définie pour les tests *)

(* dimenstion du plateau *)
let dim : dimension = 3 ;; 

(* le centre/origine du plateau *)
let origine : case = (0, 0, 0) ;; 


print_endline "Testing: 'indice_valide'" ;;
(* -2 <= x <= 2 est valide pour dim = 1 *)
assert (indice_valide (-2) 1 = true) ;;
assert (indice_valide 2 1 = true) ;;

(* -4 <= x <= 4 est valide pour dim = 2 *)
assert (indice_valide (-4) 2 = true) ;;
assert (indice_valide 4 2 = true) ;;

(* -6 <= x <= 6 est valide pour dim = 2 *)
assert (indice_valide (-6) 3 = true) ;;
assert (indice_valide 6 3 = true) ;;

(* -3 < x ou x > 3 n'est pas valide pour dim = 1 *)
assert (indice_valide (-3) 1 = false) ;;
assert (indice_valide 3 1 = false) ;;

(* -5 < x ou x > 3 n'est pas valide pour dim = 2 *)
assert (indice_valide (-5) 2 = false) ;;
assert (indice_valide 5 2 = false) ;;

(* -7 < x ou x > 7 n'est pas valide pour dim = 3 *)
assert (indice_valide (-7) 3 = false) ;;
assert (indice_valide 7 3 = false) ;;

(* Donc, -dim * 2 <= x <= dim * 2 est valide pour dim *)
assert (indice_valide (-dim * 2) dim = true) ;;
assert (indice_valide ( dim * 2) dim = true) ;;

(* Donc, -dim * 2 < x ou x > dim * 2 n'est pas valide pour dim *)
assert (indice_valide (-dim * 2 - 1) dim = false) ;;
assert (indice_valide ( dim * 2 + 1) dim = false) ;;


print_endline "Testing: 'est_case'" ;;

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


print_endline "Testing: 'est_dans_losange'" ;;

(* coin supérieur *)
assert (est_dans_losange (-6, 3, 3) dim = true) ;; 

(* coin gauche *)
assert (est_dans_losange ( 0,-3, 3) dim = true) ;; 

(* origine/centre du plateau *)
assert (est_dans_losange origine dim = true) ;; 

(* coin droite *)
assert (est_dans_losange ( 0, 3,-3) dim = true) ;;

(* coin inférieur *)
assert (est_dans_losange ( 6,-3,-3) dim = true) ;;


print_endline "Testing: 'est_dans_losange_2'" ;;

(* coin supérieur gauche *)
assert (est_dans_losange_2 ( 3,-6, 3) dim = true) ;; 

(* coin supérieur droite *)
assert (est_dans_losange_2 ( 3, 0,-3) dim = true) ;; 

(* origine/centre du plateau *)
assert (est_dans_losange_2 origine dim = true) ;; 

(* coin inférieur gauche *)
assert (est_dans_losange_2 (-3, 0, 3) dim = true) ;; 

(* coin inférieur droite *)
assert (est_dans_losange_2 (-3, 6,-3) dim = true) ;; 


print_endline "Testing: 'est_dans_losange_3'" ;;

(* coin supérieur gauche *)
assert (est_dans_losange_3 ( 3,-3, 0) dim = true) ;; 

(* coin supérieur droite *)
assert (est_dans_losange_3 ( 3, 3,-6) dim = true) ;; 

(* origine/centre du plateau *)
assert (est_dans_losange_3 origine dim = true) ;; 

(* coin inférieur gauche *)
assert (est_dans_losange_3 (-3, 3, 0) dim = true) ;; 

(* coin inférieur droite *)
assert (est_dans_losange_3 (-3,-3, 6) dim = true) ;; 


print_endline "Testing: 'est_dans_etoile'" ;;

assert (est_dans_etoile (0, 0, 0) dim = true) ;;

(* origine/centre du plateau *)
assert (est_dans_etoile origine dim = true) ;;


print_endline "Testing: 'tourner_case'" ;;


print_endline "Testing: 'translate'" ;;


print_endline "Testing: 'diff_case'" ;;


print_endline "Testing: 'dist_entre_cases'" ;;


print_endline "Testing: 'sont_cases_voisines'" ;;


print_endline "Testing: 'calcul_pivot'" ;;


print_endline "Testing: 'mod_case'" ;;


print_endline "Testing: 'vec_et_dist'" ;;


print_endline "Testing: END" ;;