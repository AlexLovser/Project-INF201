#use "inf201_ElKortbi_Tabolskii_Bendouha_Caille_Crelerot_Q1-Q9.ml" ;;
Sys.command "clear" ;;


(* 
  Fonctions pour les tests 
*)

(**
  Renvoie un nombre entier alèatoire dans l'intervalle [[a, b]].
*)
let randint (a:int) (b:int): int =
  Random.int (b - a + 1) + a
;;


(**
  Renvoie un triplet entier aléatoire dans l'intervalle [[a, b] * [a, b] * [a, b]]. 
*)
let randtriplet (a:int) (b:int): int * int * int =
  let x = randint a b
  and y = randint a b
  and z = randint a b in x, y, z
;;


(**
  Renvoie une case alèatoire non nuls dans l'étoile de dimension [dim].
*)
let rec randcase (dim:dimension): case =
  let c = randtriplet (-dim * 2) (dim * 2) in
  if est_case c && est_dans_etoile c dim && c != (0,0,0) then c else randcase dim
;;

(**
  Renvoie un vecteur alèatoire non nuls de la taille 1.
*)
let rec randvec (): vecteur =
  let v = randtriplet (-1) 1 in
  if est_case v && v != (0,0,0) then v else randvec ()
;;


(* 
  Variables pour les tests 
*)

(* dimenstion du plateau *)
let dim : dimension = 3 ;; 

(* le centre/origine du plateau et vecteur nuls *)
let origine : case = (0, 0, 0) ;; 
let v_nuls : vecteur = (0, 0, 0) ;;

(* une case alèatoire de taille dim *)
let r_case : case = randcase dim ;;

(* un vecteur avec une combinaison de -1, 0 et 1 non nuls *)
let r_vec : vecteur = randvec () ;;


(* Testing de fonctions *)

print_endline "Testing: 'indice_valide'" ;;
(* -2 <= x <= 2 est valide pour dim = 1 *)
assert (indice_valide (-2) 1 = true) ;;
assert (indice_valide 2 1 = true) ;;

(* -4 <= x <= 4 est valide pour dim = 2 *)
assert (indice_valide (-4) 2 = true) ;;
assert (indice_valide 4 2 = true) ;;

(* -6 <= x <= 6 est valide pour dim = 3 *)
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

(* tour supérieur *)
assert (est_dans_etoile ( 6,-3,-3) dim = true) ;;

(* tour supérieur droite *)
assert (est_dans_etoile ( 3, 3,-6) dim = true) ;;

(* tour supérieur gauche *)
assert (est_dans_etoile ( 3,-6, 3) dim = true) ;;

(* origine/centre du plateau *)
assert (est_dans_etoile origine dim = true) ;;

(* tour inférieur *)
assert (est_dans_etoile (-6, 3, 3) dim = true) ;;

(* tour inférieur droite *)
assert (est_dans_etoile (-3, 6,-3) dim = true) ;;

(* tour inférieur gauche *)
assert (est_dans_etoile (-3, 6, 3) dim = true) ;;


print_endline "Testing: 'tourner_case'" ;;

(* la case ne tourne pas si m = 0 *)
assert (tourner_case 0 ( 4, -2, -2) = ( 4,-2,-2)) ;;

(* la case du tour supérieur tourne vers le tour supérieur gauche *)
assert (tourner_case 1 ( 4, -2, -2) = ( 2,-4, 2)) ;;

(* la case du tour supérieur tourne vers le tour inférieur gauche *)
assert (tourner_case 2 ( 4, -2, -2) = (-2,-2, 4)) ;;

(* la case du tour supérieur tourne vers le tour inférieur *)
assert (tourner_case 3 ( 4, -2, -2) = (-4, 2, 2)) ;;

(* la case du tour supérieur tourne vers le tour inférieur droite *)
assert (tourner_case 4 ( 4, -2, -2) = (-2, 4,-2)) ;;

(* la case du tour supérieur tourne vers le tour supérieur droite *)
assert (tourner_case 5 ( 4, -2, -2) = ( 2, 2,-4)) ;;

(* la case du tour supérieur tourne vers luis même *)
assert (tourner_case 6 ( 4, -2, -2) = ( 4,-2,-2)) ;;

(* la case du origine/centre tourne vers lui même dans tous les cas *)
assert (tourner_case (randint 0 1000) origine = origine) ;;


print_endline "Testing: 'translate'" ;;

(* translation d'un vecteur nuls par l'origine/centre est une case 
   origine/centre *)
assert (translate v_nuls origine = origine) ;;

(* translation d'un vecteur nuls vers une case renvoie la case *)
assert (translate v_nuls r_case = r_case) ;;

(* translation d'un n'importe quel vecteur vers origine/centre renvoie une case 
  avec les coordonnées du vecteur. *)
assert (translate r_vec origine = r_vec) ;;

(* Quelque tests *)
assert (translate (0, -1, 1) (-3, 2, 1) = (-3, 1, 2)) ;;
assert (translate (0, -1, 1) ( 0, 4,-4) = ( 0, 3,-3)) ;;
assert (translate (0, -1, 1) ( 0,-4, 4) = ( 0,-5, 5)) ;;


print_endline "Testing: 'diff_case'" ;;


print_endline "Testing: 'dist_entre_cases'" ;;


print_endline "Testing: 'sont_cases_voisines'" ;;


print_endline "Testing: 'calcul_pivot'" ;;


print_endline "Testing: 'mod_case'" ;;


print_endline "Testing: 'vec_et_dist'" ;;


print_endline "Testing: END" ;;