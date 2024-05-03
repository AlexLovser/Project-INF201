(* ---------------------------------------------------------------------------
   inf201_El_Kortbi_Tabolskii_Q1-Q9.ml : cr Q1 Q9 projet: Groupe ima4

   Yassin El Kortbi    <elkortby@etu.univ-grenoble-alpes.fr> 
   Aleksandr Tabolskii <aleksandr.tabolskii@etu.univ-grenoble-alpes.fr>  
   Daniel Caille       <daniel.caille@etu.univ-grenoble-alpes.fr>
   Thomas Crelerot     <thomas.crelerot@etu.univ-grenoble-alpes.fr>
   Akram Bendouha      <akram.bendouha@etu.univ-grenoble-alpes.fr 
  ------------------------------------------------------------------------- *)

(* >> https://github.com/AlexLovser/Project-INF201 << *)

(* ======================================================================== *)
(* ======================= DEFINITION DE TYPES ============================ *)
(* ======================================================================== *)


(** 
  Dimension d'un plateau, note [dim] par la suite, est un parametre qui 
  encode la taille du plateau. Le plateau a [4 * dim + 1] lignes horizontales 
  que nousnumerotons de bas en haut de [-2 * dim] a [2 * dim] et similairement
  pour les lignes obliques.
*)
type dimension = int ;;


(** 
  La case est definie par trois coordonnees [(i, j, k)], la case au centre
  du plateau de jeu a pour coordonnees [(0, 0, 0)]. Les coordonnees
  representent:

  - i le numero de la ligne horizontale ;
  - j le numero de la ligne horizontale lorsqu'on a tourne le plateau d'un 
    tiers de tour dans le sens anti-horaire ;
  - k le numero de la ligne horizontale lorsqu'on a tourne le plateau d'un
    tiers de tour dans le sens horaire.
*)
type case = int * int * int ;;


(** 
  Les couleurs des joueurs. Le constructeur [Code] permet d'entrer les noms de
  joueur restreint a trois caracteres. La couleur [Libre] est une couleur en
  plus pour coder l'absence de joueur.
*)
type couleur = 
  | Vert 
  | Jaune 
  | Rouge 
  | Noir
  | Bleu
  | Marron
  | Libre 
  | Code of string
;;


(**
  Un pion d'une couleur [col] se situe sur une case [c] est code par un couple
  [(c, col)] que l'on appelle une case coloree.
*)
type case_coloree  = case * couleur ;;


(**
  Le [configuration] du jeu est donnee par un triplet forme d'une liste de
  cases colorees, une liste de joueurs et une dimension. La liste de cases 
  colorees donne l'emplacement des pions et leurs couleurs. On veillera a ce 
  que pour chaque case [c] il y ait au plus un pion sur cette case, 
  c'est-a-dire il y a au plus une couleur [col] tel que le couple [(c, col)]
  est dans la liste; l'absence de pion sur la case [c] sera code par l'absence
  de couple [(c, col)] dans la liste et non pas avec [(c, Libre)]. La liste de
  joueur permet de savoir a qui est le tour (tête de liste) et quel sera le 
  tour des suivants (en suivant l'ordre de la liste). Enfin même si elle ne 
  change pas au cours de la partie la [dimension] est donnee dans la 
  configuration car nous devons pouvoir acceder facilement a celle-ci et 
  pouvoir en changer si nous souhaitons faire une partie sur un plateau de 
  taille differente.
*)
type configuration = case_coloree list * couleur list * dimension ;;


(**
  Les coups seront decrits plus tard. Il en existe de deux sortes:
  - les deplacements unitaires (constructeur [Du])
  - les sauts multiples (constructeur [Sm])
*)
type coup = 
| Du of case * case 
| Sm of case list
;;


(**
  Le type [vecteur] est synonyme du type [case] comme un vecteur permettant 
  des translation avec les même propriet
*)
type vecteur = case ;; 


(* ======================================================================== *)
(* ============ INITIALISATION DE VARIABLE POUR LES TESTS 1 =============== *)
(* ======================================================================== *)


(* dimenstion du plateau *)
let dim : dimension = 3 ;; 

(* l'centre du plateau et vecteur nuls *)
let centre : case = (0, 0, 0) ;; 
let v_nuls : vecteur = (0, 0, 0) ;;

(**
  [(indice_valide x dim)] verifie si la coordonnee [x] et valide dans la 
  dimension [dim].
*)
let indice_valide (x:int) (dim:dimension): bool =
  -2 * dim <= x && x <= 2 * dim
;;

(* -2 <= x <= 2 est valide pour dim = 1 *)
let _ = assert (indice_valide (-2) 1 = true) ;;
let _ = assert (indice_valide 2 1 = true) ;;

(* -4 <= x <= 4 est valide pour dim = 2 *)
let _ = assert (indice_valide (-4) 2 = true) ;;
let _ = assert (indice_valide 4 2 = true) ;;

(* -6 <= x <= 6 est valide pour dim = 3 *)
let _ = assert (indice_valide (-6) 3 = true) ;;
let _ = assert (indice_valide 6 3 = true) ;;

(* -3 < x ou x > 3 n'est pas valide pour dim = 1 *)
let _ = assert (indice_valide (-3) 1 = false) ;;
let _ = assert (indice_valide 3 1 = false) ;;

(* -5 < x ou x > 3 n'est pas valide pour dim = 2 *)
let _ = assert (indice_valide (-5) 2 = false) ;;
let _ = assert (indice_valide 5 2 = false) ;;

(* -7 < x ou x > 7 n'est pas valide pour dim = 3 *)
let _ = assert (indice_valide (-7) 3 = false) ;;
let _ = assert (indice_valide 7 3 = false) ;;

(* Donc, -dim * 2 <= x <= dim * 2 est valide pour dim *)
let _ = assert (indice_valide (-dim * 2) dim = true) ;;
let _ = assert (indice_valide ( dim * 2) dim = true) ;;

(* Donc, -dim * 2 < x ou x > dim * 2 n'est pas valide pour dim *)
let _ = assert (indice_valide (-dim * 2 - 1) dim = false) ;;
let _ = assert (indice_valide ( dim * 2 + 1) dim = false) ;;


(* ======================================================================== *)
(* ============================= EXERCICE 1 =============================== *)
(* ======================================================================== *)

(*   
  1. Ce sont les cases du camp sud/camp de départ du plateau qui satisfont la 
  condition i < -dim ;;

  2. Ce sont les cases du camp nord/camp d'arrivé du plateau qui satisfont la 
  condition i > dim ;;

  3. Ce sont les cases du camp du côté supérieur gauche du plateau qui 
  satisfont la condition j < -dim ;;

  4. C'est la case supérieur du plateau qui satisfait la condition 
  (i, j, k) = (2dim, -dim, -dim) ;;

  5. C'est la case supérieur gauche du plan sud/ camp de départ du plateau qui
  satisfait la condition (i, j, k) = (-dim - 1, 1, dim) ;;

  6. Ce sont toutes les cases du plateau sauf celles des camps sud, supérieur 
  gauche et supérieur droit qui satisfont la condition
  i >= -dim et j >= -dim et k >= -dim.
*)

(**
  [(est_case c)] verifie si [c] est une case.
*)
let est_case (c:case): bool = 
  let i, j, k = c in i + j + k = 0 
;;

(* (i,j,k) = (0,0,0) => 0 + 0 + 0 = 0 *)
let _ = assert (est_case centre = true) ;;

(* (i,j,k) = (-2,1,1) => -2 + 1 + 1 = 0 *)
let _ = assert (est_case ((-2), 1, 1) = true) ;;

(* (i,j,k) = (-2,1,0) => -2 + 1 + 0 <> 0 *)
let _ = assert (est_case ((-2), 1, 0) = false) ;;

(* === EXERCICE 1 === *)

(* i < -dim *)
let _ = assert (est_case (-2 * dim, +1 * dim, +1 * dim)) ;; 

(* i > dim *)
let _ = assert (est_case (+2 * dim, -1 * dim, -1 * dim)) ;; 

(* j < -dim*)
let _ = assert (est_case (+1 * dim, -2 * dim, +1 * dim)) ;; 

(* (i,j,k) = (2dim, -dim, -dim) *)
let _ = assert (est_case (+2 * dim, -1 * dim, -1 * dim)) ;; 

(* (i,j,k) = (-dim - 1, 1, dim) *)
let _ = assert (est_case (-dim - 1, 1, dim)) ;;  

(* i >= -dim && j >= -dim && k >= -dim *)
let _ = assert (est_case (-dim, -dim, 2*dim)) ;; 


(* ======================================================================== *)
(* ============================= EXERCICE 2 =============================== *)
(* ======================================================================== *)


(** 
  [(est_dans_losange c dim)] verifie si la case [c] est dans le losange 
  North-South du plateau de dimension [dim].
*)
let est_dans_losange (c:case) (dim:dimension): bool = 
  let _, j, k = c in
    -dim <= j && j <= dim && 
    -dim <= k && k <= dim
;;    

(* coin superieur *)
let _ = assert (est_dans_losange (-6, 3, 3) dim = true) ;; 

(* coin gauche *)
let _ = assert (est_dans_losange ( 0,-3, 3) dim = true) ;; 

(* centre du plateau *)
let _ = assert (est_dans_losange centre dim = true) ;; 

(* coin droite *)
let _ = assert (est_dans_losange ( 0, 3,-3) dim = true) ;;

(* coin inferieur *)
let _ = assert (est_dans_losange ( 6,-3,-3) dim = true) ;;


(* ======================================================================== *)


(**
  [(est_dans_losange_2 c dim)] verifie si la case [c] est dans le losange 
  Northwest-Southeast du plateau de dimension [dim].
*)
let est_dans_losange_2 (c:case) (dim:dimension): bool = 
  let i, _, k = c in
    -dim <= i && i <= dim && 
    -dim <= k && k <= dim
;;

(* coin superieur gauche *)
let _ = assert (est_dans_losange_2 ( 3,-6, 3) dim = true) ;; 

(* coin superieur droite *)
let _ = assert (est_dans_losange_2 ( 3, 0,-3) dim = true) ;; 

(* centre du plateau *)
let _ = assert (est_dans_losange_2 centre dim = true) ;; 

(* coin inferieur gauche *)
let _ = assert (est_dans_losange_2 (-3, 0, 3) dim = true) ;; 

(* coin inferieur droite *)
let _ = assert (est_dans_losange_2 (-3, 6,-3) dim = true) ;; 


(* ======================================================================== *)


(**
  [(est_dans_losange_3 c dim)] verifie si la case [c] est dans le losange 
  Northeast-Southwest du plateau de dimension [dim].
*)
let est_dans_losange_3 (c:case) (dim:dimension): bool = 
  let i, j, _ = c in
    -dim <= i && i <= dim && 
    -dim <= j && j <= dim
;;

(* coin superieur gauche *)
let _ = assert (est_dans_losange_3 ( 3,-3, 0) dim = true) ;; 

(* coin superieur droite *)
let _ = assert (est_dans_losange_3 ( 3, 3,-6) dim = true) ;; 

(* centre du plateau *)
let _ = assert (est_dans_losange_3 centre dim = true) ;; 

(* coin inferieur gauche *)
let _ = assert (est_dans_losange_3 (-3, 3, 0) dim = true) ;; 

(* coin inferieur droite *)
let _ = assert (est_dans_losange_3 (-3,-3, 6) dim = true) ;; 


(* ======================================================================== *)
(* ============================= EXERCICE 3 =============================== *)
(* ======================================================================== *)


(**
  [(est_dans_etoile) c dim] verifie si la case [c] est dans l'etoile du
  plateau de dimension [dim].
*)
let est_dans_etoile (c:case) (dim:dimension): bool =
  (* l'union de trois losange est un etoile *)
  est_dans_losange c dim || 
  est_dans_losange_2 c dim || 
  est_dans_losange_3 c dim
;;

(* tour superieur *)
let _ = assert (est_dans_etoile ( 6,-3,-3) dim = true) ;;

(* tour superieur droite *)
let _ = assert (est_dans_etoile ( 3, 3,-6) dim = true) ;;

(* tour superieur gauche *)
let _ = assert (est_dans_etoile ( 3,-6, 3) dim = true) ;;

(* centre du plateau *)
let _ = assert (est_dans_etoile centre dim = true) ;;

(* tour inferieur *)
let _ = assert (est_dans_etoile (-6, 3, 3) dim = true) ;;

(* tour inferieur droite *)
let _ = assert (est_dans_etoile (-3, 6,-3) dim = true) ;;

(* tour inferieur gauche *)
let _ = assert (est_dans_etoile (-3, 6, 3) dim = true) ;;

(* ======================================================================== *)
(* ============================= EXERCICE 4 =============================== *)
(* ======================================================================== *)


(**
  [(tourner_case m c)] c'est la case [c] apres avoir fait tourner le plateau
  de [m] sixieme de tour dans le sens anti-horaire.
*)
let tourner_case (m:int) (c:case): case =
  (* reduction de nombre de fait pour tourner *)
  let m = m mod 6 in
  (* equation recursive *)
  let rec tourner_case_rec (m:int) (c:case): case = 
    let i, j, k = c in
    match m with
    | 0 -> i, j, k
    | m -> tourner_case_rec (m - 1) (-k, -i, -j) 
  in tourner_case_rec m c
;;

(* la case ne tourne pas si m = 0 *)
let _ = assert (tourner_case 0 ( 4, -2, -2) = ( 4,-2,-2)) ;;

(* la case du tour superieur tourne vers le tour superieur gauche *)
let _ = assert (tourner_case 1 ( 4, -2, -2) = ( 2,-4, 2)) ;;

(* la case du tour superieur tourne vers le tour inferieur gauche *)
let _ = assert (tourner_case 2 ( 4, -2, -2) = (-2,-2, 4)) ;;

(* la case du tour superieur tourne vers le tour inferieur *)
let _ = assert (tourner_case 3 ( 4, -2, -2) = (-4, 2, 2)) ;;

(* la case du tour superieur tourne vers le tour inferieur droite *)
let _ = assert (tourner_case 4 ( 4, -2, -2) = (-2, 4,-2)) ;;

(* la case du tour superieur tourne vers le tour superieur droite *)
let _ = assert (tourner_case 5 ( 4, -2, -2) = ( 2, 2,-4)) ;;

(* la case du tour superieur tourne vers luis même *)
let _ = assert (tourner_case 6 ( 4, -2, -2) = ( 4,-2,-2)) ;;


(* ======================================================================== *)
(* ============================= EXERCICE 5 =============================== *)
(* ======================================================================== *)


(**
  [(translate c v)] calcule la case par le translation du vecteur [v] a 
  partir de [c].
*)
let translate (c:case) (v:vecteur): case =
  let c1, c2, c3 = c    (* les coordonnees de la case c *)
  and v1, v2, v3 = v in (* les coordonnees du vectuer v *)
    v1 + c1, v2 + c2, v3 + c3 (* translation des coordonnees de v vers c *)
;;

(* 
  translation d'un vecteur nul par le centre est une case centre 
*) 
let _ = assert (translate v_nuls centre = centre) ;;

(* translation d'un vecteur nuls vers une case renvoie la case *)
let _ = assert (translate v_nuls (1, 2, -3) = (1, 2, -3)) ;;
   
(* 
  translation d'un n'importe quel vecteur vers centre renvoie une case 
  avec les coordonnees du vecteur. 
*) 
let _ = assert (translate (1, 0, -1) centre = (1, 0, -1)) ;;
   
(* Quelque tests *)
let _ = assert (translate (0, -1, 1) (-3, 2, 1) = (-3, 1, 2)) ;;
let _ = assert (translate (0, -1, 1) ( 0, 4,-4) = ( 0, 3,-3)) ;;
let _ = assert (translate (0, -1, 1) ( 0,-4, 4) = ( 0,-5, 5)) ;;


(* ======================================================================== *)
(* ============================= EXERCICE 6 =============================== *)
(* ======================================================================== *)


(**
  [(diff_case c1 c2)] c'est le vecteur de translation de [c2] vers [c1], 
  calculer par la difference entre les cases [c1] et [c2].
*)
let diff_case (c1:case) (c2:case): vecteur =
  let i1, j1, k1 = c1 
  and i2, j2, k2 = c2 in 
    (* la difference entre les coordonnees c1 et c2 *)
    i1 - i2, j1 - j2, k1 - k2 
;;

(* difference entre deux cases identique renvoie un vecteur nuls *)
let _ = assert (diff_case (1, 0, -1) (1, 0, -1) = v_nuls) ;;

(* 
  difference entre c1 non nuls et l'centre (c2) renvoie le vecteur de
  translation qui est egal a c1 
*)
let _ = assert (diff_case (1, 0, -1) centre = (1, 0, -1)) ;;

(*
  difference entre c2 non nuls et l'centre (c1) renvoie le vecteur de
  translation qui est le symetrique de c2 
*)
let _ = assert (diff_case centre (-1, 1, 0) = ( 1,-1, 0)) ;;
let _ = assert (diff_case centre (-6, 3, 3) = ( 6,-3,-3)) ;;
let _ = assert (diff_case centre (-3, 6,-3) = ( 3,-6, 3)) ;;
let _ = assert (diff_case centre ( 3, 3,-6) = (-3,-3, 6)) ;;


(* ======================================================================== *)


(**
  [(sont_cases_alignee c1 c2)] verifie si les cases [c1] et [c2] sont 
  alignees.
*)
let sont_cases_alignee (c1:case) (c2:case): bool =
  let i1, j1, k1 = c1 
  and i2, j2, k2 = c2 in 
    match () with (* comparasion entre chaque de coordonnees *)
    | _ when c1 = c2 -> false (* les doivent être de differentes valeurs *)
    | _ when i1 = i2 -> true (* s'ils sont alignees sur i, donc vrai *)
    | _ when j1 = j2 -> true (* s'ils sont alignees sur j, donc vrai *)
    | _ when k1 = k2 -> true (* s'ils sont alignees sur k, donc vrai *)
    | _ -> false (* si les cases ne sont pas alignees *)
;;

(* les cases alignees sur i *)
let _ = assert (sont_cases_alignee ( 0,-1, 1) ( 0, 2,-2) = true) ;;
let _ = assert (sont_cases_alignee ( 0, 2,-2) ( 0,-1, 1) = true) ;;
let _ = assert (sont_cases_alignee ( 0,-3, 3) ( 0, 3,-3) = true) ;;

(* les cases alignees sur j *)
let _ = assert (sont_cases_alignee (-1, 0, 1) ( 2, 0,-2) = true) ;;
let _ = assert (sont_cases_alignee ( 2, 0,-2) (-1, 0, 1) = true) ;;
let _ = assert (sont_cases_alignee (-3, 0, 3) ( 3, 0,-3) = true) ;;

(* les cases alignees sur k *)
let _ = assert (sont_cases_alignee ( 1,-1, 0) (-2, 2, 0) = true) ;;
let _ = assert (sont_cases_alignee (-2, 2, 0) ( 1,-1, 0) = true) ;;
let _ = assert (sont_cases_alignee ( 3,-3, 0) (-3, 3, 0) = true) ;;


(* ======================================================================== *)


(** 
  [(dist_coords c1 c2)] est un triplet de distances entre les coordonnees des
  cases [c1] et [c2].
*)
let dist_coords (c1:case) (c2:case): int * int * int =
  let i1, j1, k1 = c1
  and i2, j2, k2 = c2 in 
    let di = abs (i1 - i2) (* distance entre les coordonnees i *)
    and dj = abs (j1 - j2) (* distance entre les coordonnees j *)
    and dk = abs (k1 - k2) (* distance entre les coordonnees k *)
      in di, dj, dk (* triplet des distances entres i, j et k *)
;;

(* calcul de distances entres les coordonnees de cases *)
let _ = assert (dist_coords ( 1, 1,-2) ( 3, 3,-6) = (2,2, 4)) ;;
let _ = assert (dist_coords ( 0, 0, 0) ( 2, 2,-4) = (2,2, 4)) ;;
let _ = assert (dist_coords (-3,-3, 6) ( 3, 3,-6) = (6,6,12)) ;;
let _ = assert (dist_coords ( 0, 0, 0) ( 0, 0, 0) = (0,0, 0)) ;;


(* ======================================================================== *)


(**
  [(max_dist_cases c1 c2)] est la distance maximale entre les coordonnees des
  cases [c1] et [c2].
*)
let max_dist_cases (c1:case) (c2:case): int =
  let di, dj, dk = dist_coords c1 c2 in 
    max di (max dj dk) 
;;

let _ = assert (max_dist_cases ( 1, 1,-2) ( 3, 3,-6) =  4) ;;
let _ = assert (max_dist_cases ( 0, 0, 0) ( 2, 2,-4) =  4) ;;
let _ = assert (max_dist_cases (-3,-3, 6) ( 3, 3,-6) = 12) ;;
let _ = assert (max_dist_cases ( 0, 0, 0) ( 0, 0, 0) =  0) ;;


(* ======================================================================== *)


(**
  [(min_dist_cases c1 c2)] est la distance minimale entre les coordonnees des
  cases [c1] et [c2].
*)
let min_dist_cases (c1:case) (c2:case): int =
  let di, dj, dk = dist_coords c1 c2 in 
    min di (min dj dk)
;;

let _ = assert (min_dist_cases ( 1, 1,-2) ( 3, 3,-6) = 2) ;;
let _ = assert (min_dist_cases ( 0, 0, 0) ( 2, 2,-4) = 2) ;;
let _ = assert (min_dist_cases (-3,-3, 6) ( 3, 3,-6) = 6) ;;
let _ = assert (min_dist_cases ( 0, 0, 0) ( 0, 0, 0) = 0) ;;


(* ======================================================================== *)


(**
  [(compte_cases c1 c2)] est le nombre de cases entres les cases [c1] et 
  [c2]. Pour determiner ce nombre on prend la distance maximale si ils 
  sont alignees, sinon la distance minimale.
*)
let compte_cases (c1:case) (c2:case): int = 
  match () with
  (* si les cases sont egal *)
  | _ when c1 = c2 -> 0
  (* si les cases sont alignees *)
  | _ when sont_cases_alignee c1 c2 -> max_dist_cases c1 c2 - 1
  (* sinon ... *)
  | _ -> min_dist_cases c1 c2 - 1 
;;

(* entre c1 et c2: 0 cases *)
let _ = assert (compte_cases (1, 0, -1) (1, 0, -1) = 0) ;;
let _ = assert (compte_cases centre centre = 0) ;;

(* entre c1 et c2: 1 cases *)
let _ = assert (compte_cases ( 0,-1, 1) ( 0, 1,-1) = 1) ;;
let _ = assert (compte_cases ( 1, 0,-1) (-1, 0, 1) = 1) ;;
let _ = assert (compte_cases (-1, 1, 0) ( 1,-1, 0) = 1) ;;
let _ = assert (compte_cases ( 0,-2, 2) ( 2, 0,-2) = 1) ;;
let _ = assert (compte_cases ( 2, 0,-2) (-2, 2, 0) = 1) ;;
let _ = assert (compte_cases (-2, 2, 0) ( 0,-2, 2) = 1) ;;

(* entre c1 et c2: 2 cases *)
let _ = assert (compte_cases ( 2,-3, 1) ( 2, 0,-2) = 2) ;;
let _ = assert (compte_cases ( 2,-2, 0) ( 2, 1,-1) = 2) ;;

(*   c1 et c2: 3 cases *)
let _ = assert (compte_cases ( 0,-2, 2) ( 0, 2,-2) = 3) ;;
let _ = assert (compte_cases ( 2, 0,-2) (-2, 0, 2) = 3) ;;
let _ = assert (compte_cases (-2, 2, 0) ( 2,-2, 0) = 3) ;;
let _ = assert (compte_cases ( 4,-2,-2) (-4, 2, 2) = 3) ;;
let _ = assert (compte_cases (-2, 4,-2) ( 2,-4, 2) = 3) ;;
let _ = assert (compte_cases (-2,-2, 4) ( 2, 2,-4) = 3) ;;

(*   c1 et c2: 4 cases *)
let _ = assert (compte_cases ( 3,-3, 0) (-2, 2, 0) = 4) ;;
let _ = assert (compte_cases ( 3,-2,-1) (-2, 1,-1) = 4) ;;

(*   c1 et c2: 5 cases *)
let _ = assert (compte_cases ( 0,-3, 3) ( 0, 3,-3) = 5) ;;
let _ = assert (compte_cases ( 3, 0,-3) (-3, 0, 3) = 5) ;;
let _ = assert (compte_cases (-3, 3, 0) ( 3,-3, 0) = 5) ;;
let _ = assert (compte_cases ( 6,-3,-3) (-6, 3, 3) = 5) ;;
let _ = assert (compte_cases (-3, 6,-3) ( 3,-6, 3) = 5) ;;
let _ = assert (compte_cases (-3,-3, 6) ( 3, 3,-6) = 5) ;;


(* ======================================================================== *)
(* ============================= EXERCICE 7 =============================== *)
(* ======================================================================== *)


(**
  [(sont_cases_voisines c1 c2)] verifie si les cases [c1] et [c2] sont 
  voisines.
*)
let sont_cases_voisines (c1:case) (c2:case): bool =
  (* si les cases sont alignees et la distances entre eux est de 1 *)
  sont_cases_alignee c1 c2 && max_dist_cases c1 c2 = 1
;;


let _ = assert (sont_cases_voisines (centre) ( 0,-1, 1) = true) ;;
let _ = assert (sont_cases_voisines (centre) ( 0, 1,-1) = true) ;;
let _ = assert (sont_cases_voisines (centre) (-1, 0, 1) = true) ;;
let _ = assert (sont_cases_voisines (centre) ( 1, 0,-1) = true) ;;
let _ = assert (sont_cases_voisines (centre) (-1, 1, 0) = true) ;;
let _ = assert (sont_cases_voisines (centre) ( 1,-1, 0) = true) ;;

let _ = assert (sont_cases_voisines ( 1, 0,-1) centre = true) ;;
let _ = assert (sont_cases_voisines ( 1, 0,-1) ( 1, 1,-2) = true) ;;
let _ = assert (sont_cases_voisines ( 1, 0,-1) ( 2, 0,-2) = true) ;;
let _ = assert (sont_cases_voisines ( 1, 0,-1) ( 2,-1,-1) = true) ;;
let _ = assert (sont_cases_voisines ( 1, 0,-1) ( 0, 1,-1) = true) ;;


(* ======================================================================== *)
(* ============================= EXERCICE 8 =============================== *)
(* ======================================================================== *)


(**
  [(calcul_pivot c1 c2)] calcule le pivot entre les cases [c1] et [c2] si 
  elles sont alignees et le nombre de cases entre les deux est impair, sinon 
  on renvoie [None].
*)
let calcul_pivot (c1:case) (c2:case): case option =
  (* si le nombre de cases entre c1 et c2 est impair *)
  let est_impair = (compte_cases c1 c2) mod 2 = 1 
  (* les coordonnees du vecteur de translation de c2 vers c1 *)
  and i, j, k = diff_case c1 c2 in 
  (* le vecteur de translation de c2 vers le mi-chemin de c1 *) 
    let v = i/2, j/2, k/2 in
    (* les coordonnees de pivot *)
    let p = translate c2 v in
    if est_impair && sont_cases_alignee c1 c2 
      then Some(p) (* si impair et alignees, pivot existe *)
      else None    (* sinon, pivot n'existe pas *)
;;

(* entre c1 et c2: 1 cases, alignees *)
let _ = assert (calcul_pivot ( 0,-1, 1) ( 0, 1,-1) = Some (centre)) ;;
let _ = assert (calcul_pivot ( 1, 0,-1) (-1, 0, 1) = Some (centre)) ;;
let _ = assert (calcul_pivot (-1, 1, 0) ( 1,-1, 0) = Some (centre)) ;;

(* entre c1 et c2: 3 cases, alignees *)
let _ = assert (calcul_pivot ( 0,-2, 2) ( 0, 2,-2) = Some (centre)) ;;
let _ = assert (calcul_pivot ( 2, 0,-2) (-2, 0, 2) = Some (centre)) ;;
let _ = assert (calcul_pivot (-2, 2, 0) ( 2,-2, 0) = Some (centre)) ;;

(* entre c1 et c2: 5 cases, alignees *)
let _ = assert (calcul_pivot ( 0,-3, 3) ( 0, 3,-3) = Some (centre)) ;;
let _ = assert (calcul_pivot ( 3, 0,-3) (-3, 0, 3) = Some (centre)) ;;
let _ = assert (calcul_pivot (-3, 3, 0) ( 3,-3, 0) = Some (centre)) ;;

(* entre c1 et c2: 1 cases, n'est pas alignees *)
let _ = assert (calcul_pivot ( 0,-2, 2) ( 2, 0,-2) = None) ;;
let _ = assert (calcul_pivot ( 2, 0,-2) (-2, 2, 0) = None) ;;
let _ = assert (calcul_pivot (-2, 2, 0) ( 0,-2, 2) = None) ;;

(* entre c1 et c2: 3 cases, n'est pas alignees *)
let _ = assert (calcul_pivot ( 4,-2,-2) (-4, 2, 2) = None) ;;
let _ = assert (calcul_pivot (-2, 4,-2) ( 2,-4, 2) = None) ;;
let _ = assert (calcul_pivot (-2,-2, 4) ( 2, 2,-4) = None) ;;

(* c1 et c2: 5 cases, n'est pas alignees *)
let _ = assert (calcul_pivot ( 6,-3,-3) (-6, 3, 3) = None) ;;
let _ = assert (calcul_pivot (-3, 6,-3) ( 3,-6, 3) = None) ;;
let _ = assert (calcul_pivot (-3,-3, 6) ( 3, 3,-6) = None) ;;


(* ======================================================================== *)
(* ============================= EXERCICE 9 =============================== *)
(* ======================================================================== *)


(**
  [(vec_et_dist c1 c2)] est le couple [(v, d)] avec [v] le vecteur de 
  translation d'un deplacement unitaire des cases alignees [c1] vers [c2] et 
  avec [d] la distance entre c'est cases. Si le vecteur unitaire n'existe pas,
  alors en renvoie [((0, 0, 0), 0)].
*)
let vec_et_dist (c1:case) (c2:case): vecteur * int =
  (* si c1 = c2 ou si ils sont non alignees  alors on renvoie nul *)
  if c1 = c2 || not (sont_cases_alignee c1 c2) then (0, 0, 0), 0
  else (* sinon ... *)
    (* la distance entres les cases *)
    let d = max_dist_cases c1 c2
    (* les coordonnees du vecteur de translation de c2 vers c1 *)
    and i, j, k = diff_case c1 c2 in 
    (* les coordonnees du vecteur de translation unitaire de *)
    (* c2 vers c1 *)
    let i, j, k = i/d, j/d, k/d in
    (* le vecteur de translation unitaire de c1 vers c2 *)
    let v = i * (-1), j * (-1), k * (-1) in
      if est_case v 
        then v, d (* si c'est un vecteur *)
        else (0, 0, 0), 0 (*'sinon  *)
;;

(* c1 et c2 non alignees *)
let _ = assert (vec_et_dist (-6, 3, 3) ( 6,-3,-3) = ((centre), 0)) ;;
let _ = assert (vec_et_dist ( 0,-3, 3) (-3, 3, 0) = ((centre), 0)) ;;
let _ = assert (vec_et_dist (centre) (centre) = ((centre), 0)) ;;

(* c1 et c2 alignees, c1 vers c2 *)
let _ = assert (vec_et_dist ( 0,-3, 3) centre = (( 0, 1,-1), 3)) ;;
let _ = assert (vec_et_dist ( 0, 3,-3) centre = (( 0,-1, 1), 3)) ;;
let _ = assert (vec_et_dist (-3, 3, 0) centre = (( 1,-1, 0), 3)) ;;
let _ = assert (vec_et_dist ( 3,-3, 0) centre = ((-1, 1, 0), 3)) ;;
let _ = assert (vec_et_dist (-3, 0, 3) centre = (( 1, 0,-1), 3)) ;;
let _ = assert (vec_et_dist ( 3, 0,-3) centre = ((-1, 0, 1), 3)) ;;

(* c1 et c2 alignees, c2 vers c1 *)
let _ = assert (vec_et_dist centre ( 0,-3, 3) = (( 0,-1, 1), 3)) ;;
let _ = assert (vec_et_dist centre ( 0, 3,-3) = (( 0, 1,-1), 3)) ;;
let _ = assert (vec_et_dist centre (-3, 3, 0) = ((-1, 1, 0), 3)) ;;
let _ = assert (vec_et_dist centre ( 3,-3, 0) = (( 1,-1, 0), 3)) ;;
let _ = assert (vec_et_dist centre (-3, 0, 3) = ((-1, 0, 1), 3)) ;;
let _ = assert (vec_et_dist centre ( 3, 0,-3) = (( 1, 0,-1), 3)) ;;


(* ======================================================================= *)
(* =================== AFFICHAGE DE PLATEAU DU JEU ======================= *)
(* ======================================================================= *)


(**
  [(string_of_couleur col)] est une chaine de caracteres qui represente la 
  couleur [col].
*)
let string_of_couleur (col:couleur):string =
  match col with
  | Libre  -> " . "
  | Vert   -> " V "
  | Jaune  -> " J "
  | Rouge  -> " R "
  | Noir   -> " N "
  | Bleu   -> " B "
  | Marron -> " M "
  | Code s -> s  ;;


(** 
  [(transfo x y)] transforme les coordonnees cartesiennes [(x, y)] en 
  coordonnees de triplet [(u, v, w)].
*)
let transfo (x:int) (y:int): int * int * int = 
  let u = y                (* transformation en u *)
  and v = (x - y) / 2      (* transformation en v *)
  and w = (-x - y) / 2 in  (* transformation en w *)
    u, v, w (* transformation de (x,y) en (u, v, w) *)
;;


(**
  [(associe c lcc defaut)] vaut [col] si [(c,col)] est dans la liste et 
  [defaut] sinon. Si plusieurs couple avec [c]existe le premier est utilisé.
*)
let rec associe (c:case) (lcc:case_coloree list) (defaut:couleur): couleur =
    match lcc with
    | [] -> defaut
    | (k, col)::q when k = c -> col
    | _::q -> associe c q defaut
;;


(**
  [(affiche_ligne n m config)] est une ligne de plateau du jeu de la 
  coordonnee [i].
*)
let affiche_ligne (n:int) (m:int) (config:configuration): string =
  (* lcc est un case_coloree et dim est un dimension *)
  let (lcc, _, dim) = config in
  let rec affiche_ligne_aux (n:int) (m:int): string =
    if m = 4 * dim + 1 then " " (* fin de ligne *)
    else
      (* transformation de coordonnees cartesien au case *)
      let c = transfo m n in
      (* ceci est une inter-case (case inutile d'un damier) 
         ou hors de l'etoile *)
      if not ((n + m) mod 2 = 0) || not (est_dans_etoile c dim) then
        (* une place vide *)
        let nul = "   "
        (* concatenation d'une place vide avec les autres chaines *)         
        and s = affiche_ligne_aux n (m + 1) in nul ^ s
      else (* ceci est une case ou bien en dehors du plateau *)
        (* col c'est la couleur obtenue par l'associe *)
        let col = (string_of_couleur (associe c lcc Libre))
        (* concatenation d'une couleur avec les autres chaines *)
        and s = affiche_ligne_aux n (m + 1) in col ^ s
    in affiche_ligne_aux n m
;;


(**
  [(affiche)] affiche le plateau du jeu par la configuration [config].
*)
let affiche (config:configuration): unit =
  (* dim est un dimenstion *)
  let (_, _, dim) = config in 
  let max_coord =  2 * dim + 1
  and min_coord = -2 * dim - 1 
  and pas = 1 in
    (*
      (affiche_aux n) est une fonction recursive qui affiche par chaque 
      appelle une ligne du plateau, avec n le nombre de lignes qui allons
      max_coord jusqu'a min_coord.
    *)
    let rec affiche_aux (n:int): unit =
      (* 
        (-2 * dim - 1) c'est la valeur qui est plus petit de la coordonnee de
        dimension dim.
      *)
      if n = min_coord then ()
      else begin
        (* initialisation d'une ligne *)
        let ligne = affiche_ligne n (-4 * dim - 1) config in
        print_endline ligne; (* affichage de la ligne *)
        print_endline "\n";  (* le cursor dans vers la nouvelle ligne *)
        affiche_aux (n - pas)  (* appelle de la fonction recursive *)
      end
    
    in affiche_aux max_coord ;;


(* 
  Affichage d'une configuration d'un seul joueur de dimension 2
*)
let conf_1 = ( [((0, 0, 0), Jaune)], [Jaune], 2) ;; 
affiche conf_1 ;;

(*
  Affichage d'une configuration de trois joueurs de dimension 1
*)
let conf_reggae = (
  [((0,-1, 1),  Vert); ((0, 0, 0), Jaune);((0, 1,-1), Rouge)], 
  [Vert; Jaune; Rouge], 1
) ;; 
affiche conf_reggae ;;

(*
  Affichage d'une configuration vide de dimension 2
*)
let conf_vide = ([], [], 2);;
affiche conf_vide ;;

(*
  Affichage d'une configuration vide de dimension 1
*)
let conf_vide = ([], [], 1);;
affiche conf_vide ;;


(*
  A essayer apres avoir fait remplir_init
  affiche (remplir_init [Code "Ali"; Code "Bob"; Code "Jim"] 3);;
*)


(* ========================================================================= *)
(* ============================= EXERCICE 10 =============================== *)
(* ========================================================================= *)


(**
  [(tourner_liste l)] est la liste obtenue a partir de/en deplacant le premier
  element a la fin de la liste.
*)
let tourner_liste (l: 'a list): 'a list =
  match l with
  | [] -> []
  | hd::tl -> tl @ [hd]
;;

let _ = assert (tourner_liste ['a'; 'b'; 'c'] = ['b'; 'c'; 'a']) ;;
let _ = assert (tourner_liste [Vert; Jaune; Rouge] = [Jaune; Rouge; Vert]) ;;

(**
  [(der_liste l)] est le dernier element de la liste [l].
*)
let rec der_liste (l: 'a list): 'a =
  match l with
  | [] -> failwith "La liste est vide"
  | [x] -> x
  | _::tl -> der_liste tl
;;

let _ = assert (der_liste ['a'; 'b'; 'c'] = 'c') ;;
let _ = assert (der_liste [Vert; Jaune; Rouge] = Rouge) ;;


(* ========================================================================= *)
(* ============================= EXERCICE 11 =============================== *)
(* ========================================================================= *)


(**
  [(remplir_segment m (i,j,k))] est la liste de cases du segment horizontal
  dont la case la plus à gauche est [(i,j,k)] et comprenant [m] cases c’est à
  dire (remplir_segment m (i,j,k))=((i,j,k),(i,j + 1,k - 1), ..., 
  (i,j + m - 1,k - m + 1).
*)
let rec remplir_segment (m:int) (i, j, k: case): case list =
  if m <= 0 then []
  else (i, j, k) :: remplir_segment (m - 1) (i, j + 1, k - 1) 
;;  

let _ = assert (remplir_segment 1 (0, 0, 0) = [0, 0, 0]) ;;
let _ = assert (remplir_segment 3 (-4, 1, 3) = [(-4,1,3);(-4,2,2);(-4,3,1)])


(* ======================================================================== *)
(* ============================= EXERCICE 12 ============================== *)
(* ======================================================================== *)


(**
  [(remplir_triangle_bas m (i,j,k))] est la liste de cases du triangle dont les
  sommets sont (i,j,k), (i,j + m - 1,k - m + 1) et (i - m + 1, j + m -1, k).
*)
let rec remplir_triangle_bas (m:int) ((i, j, k):case): case list =
  if m <= 0 then []
  else 
    remplir_segment m (i, j, k) @ 
    remplir_triangle_bas (m - 1) (i - 1, j + 1, k) ;;
;;

let _ = assert (remplir_triangle_bas 1 (0, 0, 0) = [(0, 0, 0)]) ;;
let _ = assert (remplir_triangle_bas 3 (-4, 1, 3) = [(-4,1,3);(-4,2,2);
                (-4,3,1);(-5,2,3);(-5,3,2);(-6,3,3)]) ;;


(* ======================================================================== *)
(* ============================= EXERCICE 13 ============================== *)
(* ======================================================================== *)


(**
  [(remplir_triangle_haut m (i,j,k))] est la liste de cases du triangle dont les
  sommets sont (i,j,k), (i,j + m - 1,k - m + 1) et (i + m - 1,j,k - m + 1).
*)
let rec remplir_triangle_haut (m:int) ((i, j, k):case): case list =
  if m <= 0 then []
  else 
    remplir_segment m (i, j, k) @ 
    remplir_triangle_haut (m - 1) (i + 1, j, k - 1);;
;;

let _ = assert (remplir_triangle_haut 1 (0,0,0) = [(0,0,0)]) ;;
let _ = assert (remplir_triangle_haut 3 (-3,4,-1) = [(-3,4,-1);(-3,5,-2);
                (-3,6,-3);(-2,4,-2);(-2,5,-3);(-1,4,-3)])


(* ======================================================================== *)
(* ============================= EXERCICE 14 ============================== *)
(* ======================================================================== *)


(**
  [(colorie coul cl)] est la liste formée à partir des cases de [lc] en leur 
  ajoutant la couleur [coul].
*)
let rec colorie (coul: couleur) (lc: case list) : case_coloree list = 
  match lc with
  | [] -> []
  | h::q -> (h, coul)::(colorie coul q)
;;

let _ = assert (colorie Rouge [(0,0,0);(0,0,0)] = [((0,0,0), Rouge);
                ((0,0,0), Rouge)]) ;;


(* ======================================================================== *)
(* ============================= EXERCICE 15 ============================== *)
(* ======================================================================== *)


(**
  [(tourner_case_list lcc)] tourne une fois vers le sens anti-horaire tous les
  case_coloree, dans la liste de case_coloree [lcc].
*)
let rec tourner_case_list (lcc: case_coloree list): case_coloree list =
  match lcc with
  | [] -> []
  | (c, col)::q -> ((tourner_case 1 c), col) :: (tourner_case_list q) 
;;


(**
  [(tourner_case_list_multiple n lcc)] tourne [n] fois vers le sens anti-horaire
  tous les case_coloree, dans la liste de case_coloree [lcc].
*)
let rec tourner_case_list_multiple (n: int) (lcc: case_coloree list):
  case_coloree list =
  match n with
  | 0 -> lcc
  | _ -> (tourner_case_list_multiple (n - 1) (tourner_case_list lcc))
;;


(**
  [(tourner_config config)] est la configuration [conf] apres pasage au joueur
  suivant.
*)
let tourner_config (conf: configuration): configuration =
  (* liste de case_coloree, liste de joueurs et la dimension *)
  let plateau, joueurs, dim = conf in
  (* nombre de fois qu'il faut tourner les cases pour le joueur suivant *)
  let joueur_suivant = 6 / (List.length joueurs) in
    (* tourne les case_coloree vers le joueur suivant *)
    let plateau = tourner_case_list_multiple joueur_suivant plateau
    (* tourne la liste de joueurs *)
    and joueurs = tourner_liste joueurs in plateau, joueurs, dim
;;

(* dimision du plateau *)
let dim = 3 ;;

(* tour de joueur rouge *)
let plateau_1: case_coloree list = [
  ((-6,3,3), Vert); ((3,3,-6), Bleu); ((3,-6,3), Rouge);
] ;;

let joueurs_1: couleur list = [Vert; Rouge; Bleu] ;;
let conf_1: configuration = plateau_1, joueurs_1, dim ;;

(* tour de joueur jaune *)
let plateau_2: case_coloree list = [
  ((3,3,-6), Vert); ((3,-6,3), Bleu); ((-6,3,3), Rouge);
] ;;

let joueurs_2: couleur list = [Rouge; Bleu; Vert] ;;
let conf_2: configuration = plateau_2, joueurs_2, dim ;;

(* tour de joueur vert *)
let plateau_3: case_coloree list = [
  ((3,-6,3), Vert); ((-6,3,3), Bleu); ((3,3,-6), Rouge);
] ;;

let joueurs_3: couleur list = [Bleu; Vert; Rouge];;
let conf_3: configuration = plateau_3, joueurs_3, dim ;;

let _ = assert (tourner_config conf_1 = conf_2) ;;
affiche conf_1 ;;

let _ = assert (tourner_config conf_2 = conf_3) ;;
affiche conf_2 ;;

let _ = assert (tourner_config conf_3 = conf_1) ;;
affiche conf_3 ;;


(* ======================================================================== *)
(* ============================= EXERCICE 16 ============================== *)
(* ======================================================================== *)


(**
  [(init_plateau_aux plateau_acc joueurs dim nb)] initialise le plateau ou
  [plateau_acc] est l'accamulateur du plateau, [joueurs] est la liste de joueurs,
  [dim] la dimension et [nb] le nombre de fois de tourner le plateau.
*)
let rec init_plateau_aux (plateau_acc:case_coloree list) (joueurs: couleur list) 
  (dim: dimension) (nb: int) : case_coloree list =
  (* la longeur courante de la liste de joueurs *)
  match joueurs with
  | [] -> plateau_acc
  | joueur::joueurs_suivant -> 
    let tour = remplir_triangle_bas dim (-dim - 1, 1, dim) in
    let tour_coloree = colorie joueur tour in
    let suivant = (tourner_case_list_multiple nb plateau_acc) @ tour_coloree in
      init_plateau_aux suivant joueurs_suivant dim nb
;;

(**
  [(init_plateau joueurs dim)] initialise un plateau a partir de la liste 
  de joueurs [joueurs] et de la dimension [dim].
*)
let init_plateau (joueurs: couleur list) (dim:dimension): case_coloree list = 
  (* Nombre de fois qu'il faut tourner pour le joueur suivant *)
  let nb = (6 / List.length joueurs) in
  (* Initialise et puis tourne enconre une fois le plateau de joueurs *)
  tourner_case_list_multiple nb (init_plateau_aux [] joueurs dim nb)
;;

let dim = 3 ;;
let joueurs = [Vert; Rouge; Bleu] ;;
let plateau = init_plateau joueurs dim ;;

let conf_1 = (plateau, joueurs, dim) ;;
let conf_2 = tourner_config conf_1 ;;
let conf_3 = tourner_config conf_2 ;;

affiche conf_1 ;;
affiche conf_2 ;;
affiche conf_3 ;;

let remplir_init (joueurs: couleur list) (dim: dimension): configuration =
  init_plateau joueurs dim, joueurs, dim 
;;

(* #trace remplir_triangle_haut
#trace colorie
#trace tourner_cas_list_multiple
#trace init_remplir_plateau
#trace remplir_init *)

let test_init_conf = remplir_init [Rouge;] 1 ;;
affiche(test_init_conf) ;;


(* ======================================================================== *)
(* ============================= EXERCICE 17 ============================== *)
(* ======================================================================== *)


(* fonction associe se trouve avant la Question 10 *)

let quelle_couleur (c:case) ((cc_list, c_list, dim):configuration): couleur = 
  associe c cc_list Libre 
;;

(* Tests sur les configurations "test_init_conf" et "conf_reggae" *)
let test_init_conf = remplir_init [Vert;Jaune;Rouge;Noir;Bleu;Marron] 3 ;;

let _ = assert (quelle_couleur (0,0,0) test_init_conf = Libre) ;;
let _ = assert (quelle_couleur (-4,2,2) test_init_conf = Vert) ;;
let _ = assert (quelle_couleur (-2,-2,4) test_init_conf = Jaune) ;;
let _ = assert (quelle_couleur (2,-4,2) test_init_conf = Rouge) ;;
let _ = assert (quelle_couleur (4,-2,-2) test_init_conf = Noir) ;;
let _ = assert (quelle_couleur (2,3,-5) test_init_conf = Bleu) ;;
let _ = assert (quelle_couleur (-1,4,-3) test_init_conf = Marron) ;;


(* ======================================================================== *)
(* ============================= EXERCICE 18 ============================== *)
(* ======================================================================== *)


(**
  [(supprime_dans_config_aux plateau c)] est la case_coloree list [plateau] 
  dans laquelle on a supprime, la case coloree correspondant a la case [c].
*)
let rec supprime_dans_config_aux (ccl:case_coloree list) (c:case): 
case_coloree list =
   match ccl with
   | [] -> []
   | (c', _) :: tl when c = c' -> supprime_dans_config_aux tl c
   | hd :: tl -> hd :: supprime_dans_config_aux tl c
;;

(**
  [(supprime_dans_config conf c)] est la configuration [conf] dans laquelle on
  a supprime, dans la liste de cases colorees, la case coloree correspondant
  a la case [c].
*)
let supprime_dans_config (conf:configuration) (c:case): configuration =
  let plateau, joueurs, dim = conf in 
  let plateau = supprime_dans_config_aux plateau c in
    plateau, joueurs, dim
;;

let plt_1 = [((0,0,0),Libre); ((0,1,-1),Libre)]
let plt_2 = [((0,0,0),Libre)]

let _ = assert (supprime_dans_config (plt_1, [], 1) (0,1,-1) = (plt_2, [], 1))


(* ======================================================================== *)
(* ============================= EXERCICE 19 ============================== *)
(* ======================================================================== *)


(**
  [(trouver_couleur conf c)] trouve la couleur dans la case [c], s'il existe,
  sinon renvoie [Libre].
*)
let rec trouver_couleur (conf: case_coloree list) (c:case): couleur= 
  match conf with
  | [] -> Libre
  | [(c, x)] -> x
  | (v,x)::tl -> if v = c then x else trouver_couleur tl c;;
;;

(**
  [est_coup_valide1 conf (Du(c1,c2))] est vrai si et seulement si le coup
  (Du(c1, c2)) est valide dans la configuration [conf]. C'est a dire si et 
  seulement si les cases [c1] et [c2] sont voisines, [c1] contient un pion
  du joueur dont c'est le tour de joueur, [c2] est libre et [c2] est dans le
  losange Nord-Sud. La fonction actuelle est dans le question 25
*)
let [@warning "-8"] est_coup_valide1(conf:configuration) (Du(c1,c2):coup):
bool = 
  let (cc_list, c_list, dim) = conf in
  if sont_cases_voisines c1 c2 = true && 
    associe c1 cc_list Libre <> Libre && 
    associe c2 cc_list Libre = Libre && 
    est_dans_losange c2 dim = true && 
    associe c1 cc_list Libre = List.hd (c_list) then true 
  else false
;;

(*
  Cette fonction ne prend en compte que les coups unitaires, la fonction 
  prenant en compte les coups multuiples est à la question 25
*)


(* ======================================================================== *)
(* ============================= EXERCICE 20 ============================== *)
(* ======================================================================== *)


let [@warning "-8"] appliquer_coup1 (((case, couleur)::tl, c_list, dim): configuration) (Du(c1, c2): coup) : configuration = 
  ((List.map (fun (case, couleur) -> if case = c1 then (c2, couleur) else (case, couleur)) ((case, couleur)::tl)), c_list, dim)
;;

(* 
  j'ai fais cette fonciton avec un List.map car on suppose que que le coup 
  est valide (c'est qui a fait ?)
*)

(* 
  Cette fonction ne prend en compte que les coups unitaires, la fonction 
  prenant en compte les coups multuiples est à la question 25
*)

(* tests de quelques coup avec la fonction appliquer_coup *)

let coup1 = appliquer_coup1 (test_init_conf) (Du(( -4,1,3), ( -3,0,3))) ;;
let coup2 = appliquer_coup1 (coup1) (Du(( -3,0,3), ( -2,-1,3))) ;;
let coup3 = appliquer_coup1 (coup2) (Du(( -2,-1,3), ( -1,-1,2))) ;;
let coup4 = appliquer_coup1 (coup3) (Du(( -4,2,2), ( -3,1,2))) ;;
let coup5 = appliquer_coup1 (coup4) (Du(( -5,3,2), ( -4,2,2))) ;;

affiche coup1 ;;
affiche coup2 ;;
affiche coup3 ;;
affiche coup4 ;;
affiche coup5 ;;

let [@warning "-8"] rec appliquer_coup (((case, couleur)::tl, c_list, dim): configuration) (c: coup) : configuration = 
  match c with
  | Du (c1,c2) -> ((List.map (fun (case, couleur) -> if case = c1 then (c2, couleur) else (case, couleur)) ((case, couleur)::tl)), c_list, dim)
  | Sm ([]) -> ((case, couleur)::tl, c_list, dim)
  | Sm ([c1;c2]) -> let cx = Du(c1,c2) in appliquer_coup ((case, couleur)::tl, c_list, dim) cx
  | Sm (hd::t1) -> let c1 = hd in
    let c2 = der_liste t1 in ((List.map (fun (case, couleur) -> if case = c1 then (c2, couleur) else (case, couleur)) ((case, couleur)::tl)), c_list, dim);;

(* ======================================================================== *)
(* ============================= EXERCICE 25 ============================== *)
(* ======================================================================== *)

let [@warning "-8"] rec appliquer_coup (((case, couleur)::tl, c_list, dim): configuration) (c: coup) : configuration = 
  match c with
  | Du (c1,c2) -> ((List.map (fun (case, couleur) -> if case = c1 then (c2, couleur) else (case, couleur)) ((case, couleur)::tl)), c_list, dim)
  | Sm ([]) -> ((case, couleur)::tl, c_list, dim)
  | Sm ([c1;c2]) -> let cx = Du(c1,c2) in appliquer_coup ((case, couleur)::tl, c_list, dim) cx
  | Sm (hd::t1) ->  let c1 = hd in
      let c2 = der_liste t1 in ((List.map (fun (case, couleur) -> if case = c1 then (c2, couleur) else (case, couleur)) ((case, couleur)::tl)), c_list, dim);;


(**
  [est_coup_valide conf (Du(c1,c2))] est vrai si et seulement si le coup
  (Du(c1, c2)) est valide dans la configuration [conf]. C'est a dire si et 
  seulement si les cases [c1] et [c2] sont voisines, [c1] contient un pion
  du joueur dont c'est le tour de joueur, [c2] est libre et [c2] est dans le
  losange Nord-Sud.
*)
let rec est_coup_valide((cc_list, c_list, dim):configuration)(c:coup): bool= 
  match c with
  | Du (c1,c2) -> if sont_cases_voisines c1 c2=true && 
                 associe c1 cc_list Libre= List.hd (c_list) && 
                 associe c2 cc_list Libre=Libre && 
                 est_dans_losange c2 dim= true then true else false
  | Sm ([]) -> false
  | Sm ([c1]) -> if est_dans_losange c1 dim=true &&
                 associe c1 cc_list Libre= List.hd (c_list) then true else false
  | Sm ([c1;c2])-> let a,b,c=c1 in
      let x,y,z=c2 in
      let case_sautee =((x+a)/2, (y+b)/2, (z+c)/2) in if associe case_sautee cc_list Libre<>Libre && 
                                                         associe c1 cc_list Libre= List.hd (c_list) &&
                                                         sont_cases_voisines c1 case_sautee=true &&
                                                         sont_cases_voisines c2 case_sautee=true &&
                                                         est_dans_losange c2 dim then true else false 
  | Sm (hd::tl)-> let c1=hd in
      let c2 = List.hd tl in
      let cd= Sm([c1;c2]) in
      let inter:bool= est_coup_valide (cc_list, c_list, dim) cd in
      let cg= Sm(tl) in
      if inter=true then est_coup_valide (appliquer_coup (cc_list, c_list, dim) cd) cg else false;;

let coup10=Sm([(-5, 3, 2); (-3, 3, 0)]);;
assert ((est_coup_valide test_init_conf coup10)=true);;

let conf6=appliquer_coup test_init_conf (Sm([(-5, 3, 2); (-3, 3, 0)]));;
let coup11= Sm([(-3, 3, 0); (-5, 3, 2); (-3, 1, 2)]);;
assert ((est_coup_valide conf6 coup11)=true);;

let conf7=appliquer_coup test_init_conf (Du((-4, 2, 2), (-3, 2, 1)));;
let coup12=Sm([(-5, 3, 2); (-3, 3, 0); (-3, 1, 2)]);;
assert ((est_coup_valide conf7 coup12)=true);;

(*La version actuelle de mettre_a_jour_configuration vue précèdemment fonctionne déjà pour les sauts multiples*)


(* ======================================================================== *)
(* ============================= EXERCICE 21 ============================== *)
(* ======================================================================== *)


let mettre_a_jour_configuration (conf: configuration)(cp: coup) : configuration = 
  if est_coup_valide conf cp then 
    appliquer_coup conf cp   
  else
    failwith  " Ce coup n’est pas valide, le joueur doit rejouer" ;;
    
(* tests de quelques coup avec la fonction mettre_a_jour_configuration *)

let conf1 = mettre_a_jour_configuration (test_init_conf) (Du(( -4,1,3), ( -3,0,3))) ;;
let conf2 = mettre_a_jour_configuration (conf1) (Du(( -3,0,3), ( -2,-1,3))) ;;
let conf3 = mettre_a_jour_configuration (conf2) (Du(( -2,-1,3), ( -1,-1,2))) ;; 
(* let conf4 = mettre_a_jour_configuration (conf3) (Du(( -4,2,2), ( 0,0,0)));; j'ai mis ce test est en commentaire car il n'est pas possible, donc il renvoie failwith ce qui bloc la suite de l'éxécution*) 
 
affiche conf1 ;;
affiche conf2 ;;
affiche conf3 ;;


(* ======================================================================== *)
(* ============================= EXERCICE 22 ============================== *)
(* ======================================================================== *) 


let est_libre_seg ((a1,a2,a3): case) ((b1,b2,b3): case) (conf: configuration) : bool =
  let ((i, j, k), d) = vec_et_dist (a1,a2,a3) (b1,b2,b3) in (* pas besoin de condition au début pour voir si c1 et c2 sont alignées car ils sont supposées l'être*)
  let rec est_libre_entre c d =
    if d <= 0 then true (* si la distance=0 alors toutes les cases ont été tester *)
    else
      let c_suivante:case = (a1+(i*d),a2+(j*d),a3+(k*d)) in 
      match quelle_couleur c_suivante conf with
      | Libre -> est_libre_entre c_suivante (d - 1) (*si la case suivante est libre alors on continue la récursion *)
      | _ -> false (* si une case entre c1 et c2 n'est pas libre alors on retourne false *) 
  in
  est_libre_entre (a1,a2,a3) d ;;

(* tests de la fonction est_libre_seg *) 

let test_init_conf = remplir_init [Rouge;] 3 ;;

(* tests sur des cas où les cases alignées entre c1 et c2 sont libres *)
assert ((est_libre_seg(-4,3,1)(3,3,-6)(test_init_conf))=true);; 
assert ((est_libre_seg(-4,3,1)(3,-4,1)(test_init_conf))=true);; 
assert ((est_libre_seg(-4,1,3)(3,1,-4)(test_init_conf))=true);; 
assert ((est_libre_seg(-4,1,3)(3,-6,3)(test_init_conf))=true);; 

(* tests sur des cas où les cases alignées entre c1 et c2 ne sont pas libres *)
assert ((est_libre_seg(-5,2,3)(3,-6,3)(test_init_conf))=false);; 
assert ((est_libre_seg(-5,3,2)(3,3,-6)(test_init_conf))=false);; 

  
(* ======================================================================== *)
(* ============================= EXERCICE 23 ============================== *)
(* ======================================================================== *)

  
let est_saut ((a1,a2,a3): case) ((b1,b2,b3): case) (conf: configuration): bool =
  let ((i, j, k), d) = vec_et_dist (a1,a2,a3) (b1,b2,b3) in
  if d <> 2 then false (* si la distance n'est pas égale à 2, ce n'est pas un saut valide *)
  else if (est_libre_seg (a1,a2,a3) (b1,b2,b3) conf)=true then false (* Si la case entre est libre, ce n'est pas un saut valide *)
  else true ;;

(* tests de la fonction est_saut *) 

(* tests sur des cas où le saut est possible *)
assert ((est_saut(-5,2,3)( -3,0,3)(test_init_conf))=true);; 
assert ((est_saut(-5,2,3)(-3,2,1)(test_init_conf))=true);; 
assert ((est_saut(-5,3,2)(-3,1,2)(test_init_conf))=true);; 
assert ((est_saut(-5,3,2)(-3,3,0)(test_init_conf))=true);; 

(* tests sur des cas où le saut n'est pas possible *)
assert ((est_saut(-5,3,2)(3,1,-4)(test_init_conf))=false);; 
assert ((est_saut(-5,3,2)(centre)(test_init_conf))=false);; 
  

(* ======================================================================== *)
(* ============================= EXERCICE 24 ============================== *)
(* ======================================================================== *)

  
(* il y a un pattern matching exhaustive mais on peut l'ignorer car si la case list est <3 cela renvoie false *) 

let rec est_saut_multiple (c_list : case list) (conf: configuration) : bool = 
  let rec est_saut_multiple_rec (cl : case list) (conf: configuration) : bool =
    match cl with
    | [] | [_] -> true (* si la liste est vide ou contient une seule case à la fin de la récursive alors c'est un saut multiple valide *) 
    | (a1, a2, a3) :: (b1, b2, b3) :: tl -> 
        if est_saut (a1, a2, a3) (b1, b2, b3) conf then 
          let conf_suivante = appliquer_coup conf (Du((a1, a2, a3),(b1, b2, b3))) in
          est_saut_multiple_rec ((b1, b2, b3) :: tl) conf_suivante (* mettre à jour la configuration *)
        else
          false (* si un des sauts n'est pas valide alors le saut multiple n'est pas valide *)
  in
  est_saut_multiple_rec c_list conf
;;


(* tests de la fonction est_saut_multiple *)

(* tests sur des cas où le saut multiples est possible *)
assert ((est_saut_multiple[(-4,2,2);(-2,0,2)](coup5))=true);; (* tous les sauts font partie des saut multiple même un saut simple*)
assert ((est_saut_multiple[(-4,2,2);(-2,0,2);(0,-2,2)](coup5))=true);; 
assert ((est_saut_multiple[(-4,3,1);(-4,1,3);(-2,1,1)](coup5))=true);; 
assert ((est_saut_multiple[(-6,3,3);(-4,1,3);(-2,1,1)](coup4))=true);; 

(* tests sur des cas où le saut multiples n'est pas possible *)
assert ((est_saut_multiple[(-4,2,2);(-2,0,2);(0,0,0)](coup5))=false );; 
assert ((est_saut_multiple[(-4,3,1);(-4,1,3);(3,1,-4)](coup5))=false);;
assert ((est_saut_multiple[(-6,3,3);(-4,1,3);(3,-6,3)](coup4))=false);;

(*Verfier une partie*)


(* ======================================================================== *)
(* ============================= EXERCICE 26 ============================== *)
(* ======================================================================== *)


let score ((cc_list,c_list,dim):configuration):int= let col= List.hd c_list in List.fold_left(fun acc ((i,_,_),x)-> if x=col then acc+i else acc) 0 cc_list;;

let conf1=([((1,2,3), Vert);((4,2,3),Vert);((6,8,2),Rouge);((8,2,4),Bleu);((6,8,2),Rouge)],[Vert;Bleu;Rouge],3);;
let conf2=([((1,2,3), Rouge);((4,2,3),Vert);((6,8,2),Rouge);((8,2,4),Bleu);((6,8,2),Rouge)],[Rouge;Vert;Bleu],3);;
let conf3=([((4,2,3), Bleu);((4,2,3),Vert);((6,8,2),Rouge);((8,2,4),Bleu);((6,8,2),Rouge)],[Bleu;Vert;Rouge],3);;
assert ((score conf1)=5);;
assert ((score conf2)=13);;
assert ((score conf3)=12);;

let score_gagnant dim : int =
  let total = List.init dim (fun i -> (i + 1) * (2 * dim - i)) in
  List.fold_left (+) 0 total;;

score_gagnant 1 ;;
score_gagnant 2 ;;
score_gagnant 3 ;;
score_gagnant 4 ;;
(* score_gagnant 1 résultat : 2 *)
(* score_gagnant 2 résultat : 10 *)
(* score_gagnant 3 résultat : 28 *)
(* score_gagnant 4 résultat : 60 *)


(* ======================================================================== *)
(* ============================= EXERCICE 27 ============================== *)
(* ======================================================================== *)


let gagne ((cc_list,c_list,dim):configuration): bool = (score (cc_list,c_list,dim))= (score_gagnant dim) ;;

let conf4=([((2,-1,-1), Vert);((centre),Jaune)],[Vert;Jaune],1);;
let conf5=([((4,-2,-2), Vert);((3,-2,-1),Vert);((3,-1,-2),Vert);((0,-1,1),Bleu);((centre),Bleu);((0,1,-1),Bleu)],[Vert;Bleu],2);;
let conf6=([((-2,1,1), Vert);((centre),Jaune)],[Vert;Jaune],1);;
let conf7=([((-4,2,2), Vert);((3,-2,-1),Vert);((3,-1,-2),Vert);((0,-1,1),Bleu);((centre),Bleu);((0,1,-1),Bleu)],[Vert;Bleu],2);;
let conf8=([((3,-1,-1), Vert);((centre),Jaune)],[Vert;Jaune],1);;
let conf9=([((2,-2,-1), Vert);((centre),Jaune)],[Vert;Jaune],1);;

(* tests de la fonction gagne *) 

(* tests sur des cas où le où le protagoniste a gagner *)

assert ((gagne conf4)=true);;
assert ((gagne conf5)=true);;
assert ((gagne conf9)=true);;

(* tests sur des cas où le où le protagoniste n'a pas encore gagner *)
assert ((gagne conf6)=false);;
assert ((gagne conf7)=false);;
assert ((gagne conf8)=false);;


(* ======================================================================== *)
(* ============================= EXERCICE 28 ============================== *)
(* ======================================================================== *)


let rec est_partie ((cc_list,c_list,dim):configuration) (l:coup list): couleur= 
  match l with
  |[]-> Libre
  |hd::tl-> if est_coup_valide (cc_list,c_list,dim) hd = false then failwith "coup non valide" 
      else let conf2= mettre_a_jour_configuration (cc_list,c_list,dim) hd in
        if gagne conf2 = true then List.hd c_list 
        else
          let conf3 = tourner_config conf2
          in est_partie conf3 tl;;

let conf_new=([((-2,1,1), Vert);((2,-1,-1),Jaune)],[Vert;Jaune],1);; 
let l_c1=[Du((-2,1,1),(-1,0,1));Du((-2,1,1),(-1,0,1));Du((-1,0,1),(0,-1,1));Du((-1,0,1),(0,-1,1));Du((0,-1,1),(1,-1,0));Du((0,-1,1),(1,-1,0));Du((1,-1,0),(2,-1,-1));Du((1,-1,0),(2,-1,-1))];;
let l_c2=[Du((-2,1,1),(-1,0,1));Du((-2,1,1),(-1,0,1));Du((-1,0,1),(0,-1,1));Du((-1,0,1),(0,-1,1));Du((0,-1,1),(1,-1,0));Du((0,-1,1),(1,-1,0));Du((1,-1,0),(0,0,0));Du((1,-1,0),(2,-1,-1))];;

est_partie conf_new l_c1 ;;
est_partie conf_new l_c2 ;;

assert ((est_partie conf_new l_c1 )=Vert);;
assert ((est_partie conf_new l_c2 )=Jaune);;


(* ======================================================================== *)
(* ============================= EXERCICE 29 ============================== *)
(* ======================================================================== *)

(* supprimes tous les duplicats possibles
let rec supprime_duplicates (l: 'a list): 'a list =
  match l with
  | [] -> []
  | h::t -> h::supprime_duplicates(List.filter (fun x -> x <> h) t)
;;

(** creation d'une liste de cases adjacents en verifiant que la case et dans 
    le losange Nord-Sud *)
let init_adjacents ((i, j, k):case) (dim:dimension): case list  =
  let directions = [(1,-1,0); (1,0,-1); (0,1,-1); (-1,1,0); (-1,0,1); (0,-1,1)] in
  (* initialise les cases adjacents *)
  let adjacents = List.map (fun (di, dj, dk) -> (di + i, dj + j, dk + k)) directions in
  (* fait le filtrage de cases adjacents *)
  let adjacents = List.filter (fun c -> est_dans_losange c dim) adjacents in
    adjacents
;;

(** verifie si la case [c] dans [conf] est libre *)
let case_est_libre (conf:configuration) (c:case): bool =
  let plateau, _, dim = conf in associe c plateau Libre = Libre
;;

(** initilise la liste de coup possible pour la case dans conf *)
let case_deplacement_unitaire_possibles (conf:configuration) (c:case): (case * coup) list =
  let _, _, dim = conf in
  (* initialises les cases adjacents pour c *)
  let cases_adjacents = init_adjacents c dim in
  (* initialises les coup possibles pour c *)
  let coup_possible = List.filter (fun c -> case_est_libre conf c) cases_adjacents in
  (* initialises la liste de case et coups *)
  let resultat = List.fold_right (fun coup acc -> (coup,Du(c,coup))::acc) coup_possible [] in
  supprime_duplicates resultat
;;

(* ============================================ *)

let coup_possibles (conf:configuration) (c:case): (case * coup) list =
  let du_possible = case_deplacement_unitaire_possibles conf c in du_possible 
;; 

(* ============================================ *)


(* Tests ... *)

let mega_conf: configuration = [((3, -2, -1), Vert); ((-1, 0, 1), Vert)], [Vert], 3 ;;
affiche mega_conf ;;

coup_possibles mega_conf (-3, 2, 1) ;; *)

(* Pour l'instant la fonciton coup_possible vérifie que les coup possible dans le cas 
  des déplacement unitaire et les sauts simples*)

  let adjacents ((x, y, z):case) : case list  =
  let directions = [(1,-1,0); (1,0,-1); (0,1,-1); (-1,1,0); (-1,0,1); (0,-1,1)] in
  List.map (fun (dx, dy, dz) -> (x + dx, y + dy, z + dz)) directions ;;

let adjacents1 ((x, y, z):case) : case list  =
  let directions1 = [(2,-2,0); (2,0,-2); (0,2,-2); (-2,2,0); (-2,0,2); (0,-2,2)] in
  List.map (fun (dx, dy, dz) -> (x + dx, y + dy, z + dz)) directions1 ;;

let coup_possibles (conf) (c: case): (case * coup) list = 
  let case_v = adjacents c in
  let rec l (case_v:case list) (c:case) : (case * coup) list = 
    match case_v with 
    |[] -> []
    |hd::tl-> if est_coup_valide (conf) (Du(c,hd)) = false then l tl c
        else (hd,Du(c,hd))::(l tl c )
  in
  let case_v1 = adjacents1 c in
  let rec l1 (case_v1:case list) (c:case) : (case * coup) list = 
    match case_v1 with 
    |[] -> []
    |hd::tl-> if est_coup_valide (conf) (Sm[c;hd]) = false then l1 tl c
        else (hd,Sm[c;hd])::(l1 tl c )
  in
  (l case_v c)@(l1 case_v1 c)
    
;;

let conf_new1=([((0,0,0), Vert);((2,-1,-1),Jaune)],[Vert;Jaune],1);; 

(* tests *)

coup_possibles conf_new (-2,1,1) ;; 
coup_possibles conf_new1 (0,0,0) ;;
affiche conf_new1;;
coup_possibles test_init_conf (-5,3,2) ;;
coup_possibles test_init_conf(-5,2,3);;
coup_possibles coup2 (-5,2,3) ;;
affiche coup2;;

(*
let adjacents ((x, y, z):case) : case list  =
  let directions = [(1,-1,0); (1,0,-1); (0,1,-1); (-1,1,0); (-1,0,1); (0,-1,1)] in
  List.map (fun (dx, dy, dz) -> (x + dx, y + dy, z + dz)) directions ;;

let coup_possibles_unitaires (conf) (c: case): (case * coup) list = 
    let case_v = adjacents c in
    let rec l (case_v:case list) (c:case) : (case * coup) list = 
      match case_v with 
      |[] -> []
      |hd::tl-> if est_coup_valide (conf) (Du(c,hd)) = false then l tl c
          else (hd,Du(c,hd))::(l tl c )
    in 
    l case_v c
  ;;


let case_est_occupee (c: case) (conf: configuration): bool =
  let cc_list, c_list, dim = conf in
  (associe c cc_list Libre) <> Libre
;;

let trouver_les_cases_occupees_autour(c: case) (conf: configuration): case list =
  let adj = adjacents c in
  List.filter (fun x -> case_est_occupee x conf) adj
;;

(* let rec coup_possibles_compliquees (conf) (c: case): (case * coup) list = 
    let case_v = adjacents c in case_v

    List.map (fun (c: case) -> )
    
  ;; *)


(* let coup_possibles (conf) (c: case): (case * coup) list = 
  let unitaires = coup_possibles_unitaires conf c case in
  let compliquees = coup_possibles_unitaires conf c case in (* TODO *)
  
  unitaires @ compliquees
    
;; *)

let conf_new1=([((0,0,0), Vert);((2,-1,-1),Jaune)],[Vert;Jaune],3);; 

(* tests de la fonction intermediaire adjacents*)
adjacents (centre);;
adjacents (-2,1,1);; 

(* tests de la fonction coup_possibles*)
(* coup_possibles conf_new (-2,1,1) ;; 
coup_possibles conf_new1 (0,0,0) ;; *)

affiche conf_new1 ;;
adjacents (0, 0, 1);;
let cc_list, c_list, dim = conf_new1 ;;
trouver_les_cases_occupees_autour (0, 0, 1) conf_new1;;
case_est_occupee  (0, 0, 1) conf_new1;;
*)

(* ======================================================================== *)
(* ============================= EXERCICE 29 ============================== *)
(* ======================================================================== *)

let rec que_mes_pions ((cc_list, c_list, dim) : configuration) : case list =
  match cc_list with
  | [] -> []
  | (c, col) :: tl ->
      if col = List.hd c_list then
        c :: que_mes_pions (tl, c_list, dim)
      else
        que_mes_pions (tl, c_list, dim)

let rec tous_les_coups (liste_case: case list) (config: configuration) : (case * coup) list =
  match liste_case with
  | [] -> []
  | h :: t -> (coup_possibles config h) @ tous_les_coups t config

let score_coup (cp: coup) : int =
  match cp with
  | Du(c1, c2) -> let (i, _, _), (i2, _, _) = c1, c2 in  abs(i2 - i)
  | _ -> 0 (* Gestion d'un cas par défaut *)

let rec heuristique (liste_coup: (case * coup) list) : coup =
  match liste_coup with
  | [] -> failwith "Pas de coup valide dans la liste" (* Gestion d'un cas vide *)
  | [(_, cp)] -> cp (* Si on a un seul coup, on le retourne directement *)
  | (_, cp) :: reste ->
      let meilleur_coup = List.fold_left
          (fun acc (_, x) -> if score_coup acc < score_coup x then acc else x) cp liste_coup
      in
      meilleur_coup

let strategie_gloutonne (config: configuration) : coup =
  let mes_cases = que_mes_pions config in
  let coup_liste = tous_les_coups mes_cases config in
  heuristique coup_liste
let config =  remplir_init [Vert; Rouge] 3
let mes_pions = que_mes_pions config
let coup_liste = tous_les_coups mes_pions config
let meilleur_coup = heuristique coup_liste;;
strategie_gloutonne config;;
strategie_gloutonne coup2 ;;