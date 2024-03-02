include Types
include Case
include OUnit2

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
  Renvoie un triplet entier aléatoire dans l'intervalle 
  [[a, b] * [a, b] * [a, b]]. 
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
  if c != (0,0,0) && est_case c && est_dans_etoile c dim then c 
  else randcase dim
;;

(**
  Renvoie un vecteur alèatoire non nuls de la taille 1.
*)
let rec randvec (): vecteur =
  let v = randtriplet (-1) 1 in
  if est_case v && v != (0,0,0) then v 
  else randvec ()
;;


(**
  Creation d'une [TestLabel] pour une [TestCase].
*)
let test_bool (t_name_1:string) (t_name_2:string) (b:bool) =
  t_name_1 >:: (fun _ -> assert_bool t_name_2 b)
;;


(**
  Creation d'une liste de [TestCase].
*)
let rec test_bools (num:int) (raise:string) (bl:bool list):OUnitTest.test list =
  match bl with
  | [] -> []
  | t::q -> (test_bool (string_of_int num) raise t)::(test_bools (num + 1) raise q)
;;


(**
  ...
*)
let run_test (test_name:string) (raison:string) (bl:bool list): unit =
  run_test_tt_main (test_name >::: test_bools 0 raison bl)
;;



(* 
  Variables pour les tests 
*)

(* dimenstion du plateau *)
let dim : dimension = 3 ;; 

(* l'centre du plateau et vecteur nuls *)
let centre : case = (0, 0, 0) ;; 
let v_nuls : vecteur = (0, 0, 0) ;;

(* une case alèatoire de taille dim *)
let r_case : case = randcase dim ;;

(* un vecteur avec une combinaison de -1, 0 et 1 non nuls *)
let r_vec : vecteur = randvec () ;;


run_test "indice_valide" "indice n'est pas valise" [
  (* -2 <= x <= 2 est valide pour dim = 1 *)
  indice_valide (-2) 1 = true;
  indice_valide 2 1 = true;

  (* -4 <= x <= 4 est valide pour dim = 2 *)
  indice_valide (-4) 2 = true;
  indice_valide 4 2 = true;

  (* -6 <= x <= 6 est valide pour dim = 3 *)
  indice_valide (-6) 3 = true;
  indice_valide 6 3 = true;

  (* -3 < x ou x > 3 n'est pas valide pour dim = 1 *)
  indice_valide (-3) 1 = false;
  indice_valide 3 1 = false;

  (* -5 < x ou x > 3 n'est pas valide pour dim = 2 *)
  indice_valide (-5) 2 = false;
  indice_valide 5 2 = false;

  (* -7 < x ou x > 7 n'est pas valide pour dim = 3 *)
  indice_valide (-7) 3 = false;
  indice_valide 7 3 = false;

  (* Donc, -dim * 2 <= x <= dim * 2 est valide pour dim *)
  indice_valide (-dim * 2) dim = true;
  indice_valide ( dim * 2) dim = true;

  (* Donc, -dim * 2 < x ou x > dim * 2 n'est pas valide pour dim *)
  indice_valide (-dim * 2 - 1) dim = false;
  indice_valide ( dim * 2 + 1) dim = false;
] ;;

run_test "est_case" "n'est pas un case" [
  (* i < -dim *)
  est_case (-2 * dim, +1 * dim, +1 * dim);

  (* i > dim *)
  est_case (+2 * dim, -1 * dim, -1 * dim);

  (* j < -dim*)
  est_case (+1 * dim, -2 * dim, +1 * dim);

  (* (i,j,k) = (2dim, -dim, -dim) *)
  est_case (+2 * dim, -1 * dim, -1 * dim);

  (* (i,j,k) = (-dim - 1, 1, dim) *)
  est_case (-dim - 1, 1, dim);

  (* i >= -dim && j >= -dim && k >= -dim *)
  est_case (-dim, -dim, 2*dim);
] ;;


run_test "est_dans_losange" "n'est pas dans losange North-South" [
  (* coin supérieur *)
  est_dans_losange (-6, 3, 3) dim = true;
  
  (* coin gauche *)
  est_dans_losange ( 0,-3, 3) dim = true; 

  (* centre du plateau *)
  est_dans_losange centre dim = true;

  (* coin droite *)
  est_dans_losange ( 0, 3,-3) dim = true;

  (* coin inférieur *)
  est_dans_losange ( 6,-3,-3) dim = true;
];;


run_test "est_dans_losange_2" "n'est pas losange Northwest-Southeast" [
  (* coin supérieur gauche *)
  est_dans_losange_2 ( 3,-6, 3) dim = true; 

  (* coin supérieur droite *)
  est_dans_losange_2 ( 3, 0,-3) dim = true; 

  (* centre du plateau *)
  est_dans_losange_2 centre dim = true; 

  (* coin inférieur gauche *)
  est_dans_losange_2 (-3, 0, 3) dim = true; 

  (* coin inférieur droite *)
  est_dans_losange_2 (-3, 6,-3) dim = true; 
] ;;


run_test "est_dans_losange_3" "n'est pas dans losange Northeast-Southwest" [
  (* coin supérieur gauche *)
  est_dans_losange_3 ( 3,-3, 0) dim = true; 

  (* coin supérieur droite *)
  est_dans_losange_3 ( 3, 3,-6) dim = true; 

  (* centre du plateau *)
  est_dans_losange_3 centre dim = true; 

  (* coin inférieur gauche *)
  est_dans_losange_3 (-3, 3, 0) dim = true; 

  (* coin inférieur droite *)
  est_dans_losange_3 (-3,-3, 6) dim = true; 

] ;;


run_test "est_dans_etoile" "n'est pas dans etoile" [
  (* tour supérieur *)
  est_dans_etoile ( 6,-3,-3) dim = true;

  (* tour supérieur droite *)
  est_dans_etoile ( 3, 3,-6) dim = true;

  (* tour supérieur gauche *)
  est_dans_etoile ( 3,-6, 3) dim = true;

  (* centre du plateau *)
  est_dans_etoile centre dim = true;

  (* tour inférieur *)
  est_dans_etoile (-6, 3, 3) dim = true;

  (* tour inférieur droite *)
  est_dans_etoile (-3, 6,-3) dim = true;

  (* tour inférieur gauche *)
  est_dans_etoile (-3, 6, 3) dim = true;

] ;;


run_test "tourner_case" "la valeur invalide" [

  (* la case ne tourne pas si m = 0 *)
  tourner_case 0 ( 4, -2, -2) = ( 4,-2,-2);

  (* la case du tour supérieur tourne vers le tour supérieur gauche *)
  tourner_case 1 ( 4, -2, -2) = ( 2,-4, 2);

  (* la case du tour supérieur tourne vers le tour inférieur gauche *)
  tourner_case 2 ( 4, -2, -2) = (-2,-2, 4);

  (* la case du tour supérieur tourne vers le tour inférieur *)
  tourner_case 3 ( 4, -2, -2) = (-4, 2, 2);

  (* la case du tour supérieur tourne vers le tour inférieur droite *)
  tourner_case 4 ( 4, -2, -2) = (-2, 4,-2);

  (* la case du tour supérieur tourne vers le tour supérieur droite *)
  tourner_case 5 ( 4, -2, -2) = ( 2, 2,-4);

  (* la case du tour supérieur tourne vers luis même *)
  tourner_case 6 ( 4, -2, -2) = ( 4,-2,-2);

  (* la case du centre tourne vers lui même dans tous les cas *)
  tourner_case (randint 0 1000) centre = centre ;

] ;;


run_test "translate" "translation incorrete" [
  (* translation d'un vecteur nuls par l'centre est une case 
    centre *)
  translate v_nuls centre = centre ;

  (* translation d'un vecteur nuls vers une case renvoie la case *)
  translate v_nuls r_case = r_case ;

  (* translation d'un n'importe quel vecteur vers centre renvoie une case 
    avec les coordonnées du vecteur. *)
  translate r_vec centre = r_vec ;

  (* Quelque tests *)
  translate (0, -1, 1) (-3, 2, 1) = (-3, 1, 2) ;
  translate (0, -1, 1) ( 0, 4,-4) = ( 0, 3,-3) ;
  translate (0, -1, 1) ( 0,-4, 4) = ( 0,-5, 5) ;
] ;;


run_test "diff_case" "vecteur du translation incorrecte" [
  (* difference entre deux cases identique renvoie un vecteur nuls *)
  diff_case r_case r_case = v_nuls ;

  (* difference entre c1 non nuls et l'centre (c2) renvoie le vecteur de
    translation qui est égal à c1 *)
  diff_case r_case centre = r_case ;

  (* difference entre c2 non nuls et l'centre (c1) renvoie le vecteur de
    translation qui est le symetrique de c2 *)
  diff_case centre (-1, 1, 0) = ( 1,-1, 0) ;
  diff_case centre (-6, 3, 3) = ( 6,-3,-3) ;
  diff_case centre (-3, 6,-3) = ( 3,-6, 3) ;
  diff_case centre ( 3, 3,-6) = (-3,-3, 6) ;
] ;;


run_test "sont_cases_alignee" "les cases ne sont pas alignées" [
  (* les cases alignées de coordonnées i *)
  sont_cases_alignee ( 0,-1, 1) ( 0, 2,-2) = true;
  sont_cases_alignee ( 0, 2,-2) ( 0,-1, 1) = true;
  sont_cases_alignee ( 0,-3, 3) ( 0, 3,-3) = true;

  (* les cases alignées de coordonnées j *)
  sont_cases_alignee (-1, 0, 1) ( 2, 0,-2) = true;
  sont_cases_alignee ( 2, 0,-2) (-1, 0, 1) = true;
  sont_cases_alignee (-3, 0, 3) ( 3, 0,-3) = true;

  (* les cases alignées de coordonnées k *)
  sont_cases_alignee ( 1,-1, 0) (-2, 2, 0) = true;
  sont_cases_alignee (-2, 2, 0) ( 1,-1, 0) = true;
  sont_cases_alignee ( 3,-3, 0) (-3, 3, 0) = true;
] ;;


run_test "dist_coords" "la distance entre les coordoonnées incorecte" [
  (* calcul de distances entres les coordonnées de cases *)
  dist_coords ( 1, 1,-2) ( 3, 3,-6) = (2,2, 4);
  dist_coords ( 0, 0, 0) ( 2, 2,-4) = (2,2, 4);
  dist_coords (-3,-3, 6) ( 3, 3,-6) = (6,6,12);
  dist_coords ( 0, 0, 0) ( 0, 0, 0) = (0,0, 0);
] ;;


run_test "max_dist_cases" "la distance maximale entre les cases incorrecte" [
  max_dist_cases ( 1, 1,-2) ( 3, 3,-6) =  4;
  max_dist_cases ( 0, 0, 0) ( 2, 2,-4) =  4;
  max_dist_cases (-3,-3, 6) ( 3, 3,-6) = 12;
  max_dist_cases ( 0, 0, 0) ( 0, 0, 0) =  0;
] ;;


run_test "min_dist_cases" "la distance minumale entre les cases incorrecte" [
  min_dist_cases ( 1, 1,-2) ( 3, 3,-6) = 2 ;
  min_dist_cases ( 0, 0, 0) ( 2, 2,-4) = 2 ;
  min_dist_cases (-3,-3, 6) ( 3, 3,-6) = 6 ;
  min_dist_cases ( 0, 0, 0) ( 0, 0, 0) = 0 ;
] ;;


run_test "compte_cases" "nombre de cases entre les cases incorrecte" [
  (* entre c1 et c2: 0 cases *)
  compte_cases r_case r_case = 0 ;
  compte_cases centre centre = 0 ;

  (* entre c1 et c2: 1 cases *)
  compte_cases ( 0,-1, 1) ( 0, 1,-1) = 1 ;
  compte_cases ( 1, 0,-1) (-1, 0, 1) = 1 ;
  compte_cases (-1, 1, 0) ( 1,-1, 0) = 1 ;
  compte_cases ( 0,-2, 2) ( 2, 0,-2) = 1 ;
  compte_cases ( 2, 0,-2) (-2, 2, 0) = 1 ;
  compte_cases (-2, 2, 0) ( 0,-2, 2) = 1 ;

  (* entre c1 et c2: 2 cases *)
  compte_cases ( 2,-3, 1) ( 2, 0,-2) = 2 ;
  compte_cases ( 2,-2, 0) ( 2, 1,-1) = 2 ;

  (*   c1 et c2: 3 cases *)
  compte_cases ( 0,-2, 2) ( 0, 2,-2) = 3 ;
  compte_cases ( 2, 0,-2) (-2, 0, 2) = 3 ;
  compte_cases (-2, 2, 0) ( 2,-2, 0) = 3 ;
  compte_cases ( 4,-2,-2) (-4, 2, 2) = 3 ;
  compte_cases (-2, 4,-2) ( 2,-4, 2) = 3 ;
  compte_cases (-2,-2, 4) ( 2, 2,-4) = 3 ;

  (*   c1 et c2: 4 cases *)
  compte_cases ( 3,-3, 0) (-2, 2, 0) = 4 ;
  compte_cases ( 3,-2,-1) (-2, 1,-1) = 4 ;

  (*   c1 et c2: 5 cases *)
  compte_cases ( 0,-3, 3) ( 0, 3,-3) = 5 ;
  compte_cases ( 3, 0,-3) (-3, 0, 3) = 5 ;
  compte_cases (-3, 3, 0) ( 3,-3, 0) = 5 ;
  compte_cases ( 6,-3,-3) (-6, 3, 3) = 5 ;
  compte_cases (-3, 6,-3) ( 3,-6, 3) = 5 ;
  compte_cases (-3,-3, 6) ( 3, 3,-6) = 5 ;
] ;;


run_test "sont_cases_voisines" "les cases ne sont pas voisines" [
  sont_cases_voisines (centre) ( 0,-1, 1) = true;
  sont_cases_voisines (centre) ( 0, 1,-1) = true;
  sont_cases_voisines (centre) (-1, 0, 1) = true;
  sont_cases_voisines (centre) ( 1, 0,-1) = true;
  sont_cases_voisines (centre) (-1, 1, 0) = true;
  sont_cases_voisines (centre) ( 1,-1, 0) = true;
] ;;


run_test "calcul_pivot t1" "le pivot existe" [
(* entre c1 et c2: 1 cases, alignées *)
calcul_pivot ( 0,-1, 1) ( 0, 1,-1) = Some (centre) ;
calcul_pivot ( 1, 0,-1) (-1, 0, 1) = Some (centre) ;
calcul_pivot (-1, 1, 0) ( 1,-1, 0) = Some (centre) ;

(* entre c1 et c2: 3 cases, alignées *)
calcul_pivot ( 0,-2, 2) ( 0, 2,-2) = Some (centre) ;
calcul_pivot ( 2, 0,-2) (-2, 0, 2) = Some (centre) ;
calcul_pivot (-2, 2, 0) ( 2,-2, 0) = Some (centre) ;

(* entre c1 et c2: 5 cases, alignées *)
calcul_pivot ( 0,-3, 3) ( 0, 3,-3) = Some (centre) ;
calcul_pivot ( 3, 0,-3) (-3, 0, 3) = Some (centre) ;
calcul_pivot (-3, 3, 0) ( 3,-3, 0) = Some (centre) ;
] ;;


run_test "calcul_pivot t2" "le pivot n'existe pas" [
  (* entre c1 et c2: 1 cases, n'est pas alignées *)
  calcul_pivot ( 0,-2, 2) ( 2, 0,-2) = None ;
  calcul_pivot ( 2, 0,-2) (-2, 2, 0) = None ;
  calcul_pivot (-2, 2, 0) ( 0,-2, 2) = None ;

  (* entre c1 et c2: 3 cases, n'est pas alignées *)
  calcul_pivot ( 4,-2,-2) (-4, 2, 2) = None ;
  calcul_pivot (-2, 4,-2) ( 2,-4, 2) = None ;
  calcul_pivot (-2,-2, 4) ( 2, 2,-4) = None ;

  (* c1 et c2: 5 cases, n'est pas alignées *)
  calcul_pivot ( 6,-3,-3) (-6, 3, 3) = None ;
  calcul_pivot (-3, 6,-3) ( 3,-6, 3) = None ;
  calcul_pivot (-3,-3, 6) ( 3, 3,-6) = None ;
] ;;


run_test "vec_et_dist" "le vecteur ou la distance incorrecte" [
  (* c1 et c2 non alignées *)
  vec_et_dist (-6, 3, 3) ( 6,-3,-3) = ((centre), 0) ;
  vec_et_dist ( 0,-3, 3) (-3, 3, 0) = ((centre), 0) ;
  vec_et_dist (centre) (centre) = ((centre), 0) ;

  (* c1 et c2 alignées, c1 vers c2 *)
  vec_et_dist ( 0,-3, 3) centre = (( 0, 1,-1), 3) ;
  vec_et_dist ( 0, 3,-3) centre = (( 0,-1, 1), 3) ;
  vec_et_dist (-3, 3, 0) centre = (( 1,-1, 0), 3) ;
  vec_et_dist ( 3,-3, 0) centre = ((-1, 1, 0), 3) ;
  vec_et_dist (-3, 0, 3) centre = (( 1, 0,-1), 3) ;
  vec_et_dist ( 3, 0,-3) centre = ((-1, 0, 1), 3) ;

  (* c1 et c2 alignées, c2 vers c1 *)
  vec_et_dist centre ( 0,-3, 3) = (( 0,-1, 1), 3) ;
  vec_et_dist centre ( 0, 3,-3) = (( 0, 1,-1), 3) ;
  vec_et_dist centre (-3, 3, 0) = ((-1, 1, 0), 3) ;
  vec_et_dist centre ( 3,-3, 0) = (( 1,-1, 0), 3) ;
  vec_et_dist centre (-3, 0, 3) = ((-1, 0, 1), 3) ;
  vec_et_dist centre ( 3, 0,-3) = (( 1, 0,-1), 3) ;
] ;;