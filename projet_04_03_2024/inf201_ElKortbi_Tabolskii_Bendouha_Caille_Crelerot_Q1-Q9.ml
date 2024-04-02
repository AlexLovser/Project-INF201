(* ---------------------------------------------------------------------------
   inf201_El_Kortbi_Tabolskii_Q1-Q9.ml : cr Q1 Q9 projet: Groupe ima4

   Yassin El Kortbi    <elkortby@etu.univ-grenoble-alpes.fr> 
   Aleksandr Tabolskii <aleksandr.tabolskii@etu.univ-grenoble-alpes.fr>  
   Daniel Caille       <daniel.caille@etu.univ-grenoble-alpes.fr>
   Thomas Crelerot     <thomas.crelerot@etu.univ-grenoble-alpes.fr>
   Akram Bendouha      <akram.bendouha@etu.univ-grenoble-alpes.fr 
  ------------------------------------------------------------------------- *)

(* REMARQUE:
  il y a quelques types/fonctions avec des warning, c'est a cause de dune pour
  bien l'executer.
*)


(* SPECIFICATION COMPLET (PDF), PROJET AVEC DUNE ET ETC. 
    >> https://github.com/AlexLovser/Project-INF201 <<
*)


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
type [@warning "-27"] couleur = 
  | Vert 
  | Jaune 
  | Rouge 
  | Noir [@warning "-37"]  
  | Bleu [@warning "-37"] 
  | Marron [@warning "-37"] 
  | Libre 
  | Code of string [@warning "-37"] 
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
type [@warning "-34"] coup = 
| Du of case * case [@warning "-37"]  
| Sm of case list   [@warning "-37"] 
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


(* ======================================================================== *)


(**
  Renvoie un nombre entier aleatoire dans l'intervalle [[a, b]].
*)
let randint (a:int) (b:int): int =
  Random.int (b - a + 1) + a
;;


(* si l'intervale est sigleton n, alors le nombre est n *)
assert(randint (-1) (-1) = -1) ;;
assert(randint 0 0 = 0) ;;
assert(randint 1 1 = 1) ;;


(* TODO *)
(* le nombre n est entre les bornes n - a et n + b *)
(* assert(
  let a, b = 5, 10 in
  let n = randint (7 - a) (7 + b) in a - n <= n && n <= n + b
) ;;

assert(
  let a, b = -3, 20 in
  let n = randint (10 - a) (10 + b) in a - n <= n && n <= n + b
) ;;

assert(
  let a, b = 0, 0 in 
  let n = randint (3 - a) (3 + b) in n - a <= n && n <= n + b
) ;; *)


(* ======================================================================== *)


(**
  Renvoie un triplet entier aleatoire dans l'intervalle 
  [[a, b] * [a, b] * [a, b]]. 
*)
let randtriplet (a:int) (b:int): int * int * int =
  let x = randint a b
  and y = randint a b
  and z = randint a b in x, y, z
;;

(* si l'intervale est un singleton n, alors le triplet est (n, n, n) *)
assert(randtriplet (-1) (-1) = (-1, -1, -1)) ;;
assert(randtriplet 0 0 = (0, 0, 0)) ;;
assert(randtriplet 1 1 = (1, 1, 1)) ;;

(* le nombre n est entre les bornes n - a et n + b dans le triplet *)
assert(
  let a, b = 5, 10
  and n = 7 in
  let n3 = randtriplet (n - a) (n + b) 
    in (a - n, a - n, a - n) <= n3 && n3 <= (n + b, n + b, n + b)
) ;;

assert(
  let a, b = -10, 20
  and n = 15 in
  let n3 = randtriplet (n - a) (n + b) 
    in (a - n, a - n, a - n) <= n3 && n3 <= (n + b, n + b, n + b)
) ;;

assert(
  let a, b = 0, 0
  and n = 3 in
  let n3 = randtriplet (n - a) (n + b) 
    in (a - n, a - n, a - n) <= n3 && n3 <= (n + b, n + b, n + b)
) ;;


(* ======================================================================== *)


(**
  [(indice_valide x dim)] verifie si la coordonnee [x] et valide dans la 
  dimension [dim].
*)
let indice_valide (x:int) (dim:dimension): bool =
  -2 * dim <= x && x <= 2 * dim
;;

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
assert (est_case centre = true) ;;

(* (i,j,k) = (-2,1,1) => -2 + 1 + 1 = 0 *)
assert (est_case ((-2), 1, 1) = true) ;;

(* (i,j,k) = (-2,1,0) => -2 + 1 + 0 <> 0 *)
assert (est_case ((-2), 1, 0) = false) ;;

(* === EXERCICE 1 === *)

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
assert (est_dans_losange (-6, 3, 3) dim = true) ;; 

(* coin gauche *)
assert (est_dans_losange ( 0,-3, 3) dim = true) ;; 

(* centre du plateau *)
assert (est_dans_losange centre dim = true) ;; 

(* coin droite *)
assert (est_dans_losange ( 0, 3,-3) dim = true) ;;

(* coin inferieur *)
assert (est_dans_losange ( 6,-3,-3) dim = true) ;;


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
assert (est_dans_losange_2 ( 3,-6, 3) dim = true) ;; 

(* coin superieur droite *)
assert (est_dans_losange_2 ( 3, 0,-3) dim = true) ;; 

(* centre du plateau *)
assert (est_dans_losange_2 centre dim = true) ;; 

(* coin inferieur gauche *)
assert (est_dans_losange_2 (-3, 0, 3) dim = true) ;; 

(* coin inferieur droite *)
assert (est_dans_losange_2 (-3, 6,-3) dim = true) ;; 


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
assert (est_dans_losange_3 ( 3,-3, 0) dim = true) ;; 

(* coin superieur droite *)
assert (est_dans_losange_3 ( 3, 3,-6) dim = true) ;; 

(* centre du plateau *)
assert (est_dans_losange_3 centre dim = true) ;; 

(* coin inferieur gauche *)
assert (est_dans_losange_3 (-3, 3, 0) dim = true) ;; 

(* coin inferieur droite *)
assert (est_dans_losange_3 (-3,-3, 6) dim = true) ;; 


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
assert (est_dans_etoile ( 6,-3,-3) dim = true) ;;

(* tour superieur droite *)
assert (est_dans_etoile ( 3, 3,-6) dim = true) ;;

(* tour superieur gauche *)
assert (est_dans_etoile ( 3,-6, 3) dim = true) ;;

(* centre du plateau *)
assert (est_dans_etoile centre dim = true) ;;

(* tour inferieur *)
assert (est_dans_etoile (-6, 3, 3) dim = true) ;;

(* tour inferieur droite *)
assert (est_dans_etoile (-3, 6,-3) dim = true) ;;

(* tour inferieur gauche *)
assert (est_dans_etoile (-3, 6, 3) dim = true) ;;

(* ======================================================================== *)


(**
  Renvoie une case aleatoire non nuls dans l'etoile de dimension [dim].
*)
let rec randcase (dim:dimension): case =
  let c = randtriplet (-dim * 2) (dim * 2) in
  if c <> (0, 0, 0) && est_case c && est_dans_etoile c dim then c 
  else randcase dim
;;

(* un case aleatoire est toujour un case *)
assert(let c = randcase 1 in c <> centre && est_case c) ;;
assert(let c = randcase 2 in c <> centre && est_case c) ;;
assert(let c = randcase 3 in c <> centre && est_case c) ;;
assert(let c = randcase 4 in c <> centre && est_case c) ;;
assert(let c = randcase 5 in c <> centre && est_case c) ;;


(* ======================================================================== *)


(**
  Renvoie un vecteur aleatoire non nuls de la taille 1.
*)
let randvec (): vecteur = randcase 1 ;;

(* un vecteur aleatoire est toujour un vecteur *)
assert(let v = randvec () in v <> centre && est_case v) ;;
assert(let v = randvec () in v <> centre && est_case v) ;;
assert(let v = randvec () in v <> centre && est_case v) ;;
assert(let v = randvec () in v <> centre && est_case v) ;;
assert(let v = randvec () in v <> centre && est_case v) ;;


(* ======================================================================== *)
(* ============ INITIALISATION DE VARIABLE POUR LES TESTS 2 =============== *)
(* ======================================================================== *)

(* une case aleatoire de taille dim *)
let r_case : case = randcase dim ;;

(* un vecteur avec une combinaison de -1, 0 et 1 non nuls *)
let r_vec : vecteur = randvec () ;;

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
assert (tourner_case 0 ( 4, -2, -2) = ( 4,-2,-2)) ;;

(* la case du tour superieur tourne vers le tour superieur gauche *)
assert (tourner_case 1 ( 4, -2, -2) = ( 2,-4, 2)) ;;

(* la case du tour superieur tourne vers le tour inferieur gauche *)
assert (tourner_case 2 ( 4, -2, -2) = (-2,-2, 4)) ;;

(* la case du tour superieur tourne vers le tour inferieur *)
assert (tourner_case 3 ( 4, -2, -2) = (-4, 2, 2)) ;;

(* la case du tour superieur tourne vers le tour inferieur droite *)
assert (tourner_case 4 ( 4, -2, -2) = (-2, 4,-2)) ;;

(* la case du tour superieur tourne vers le tour superieur droite *)
assert (tourner_case 5 ( 4, -2, -2) = ( 2, 2,-4)) ;;

(* la case du tour superieur tourne vers luis même *)
assert (tourner_case 6 ( 4, -2, -2) = ( 4,-2,-2)) ;;

(* la case du centre tourne vers lui même dans tous les cas *)
assert (tourner_case (randint 0 1000) centre = centre) ;;


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
assert (translate v_nuls centre = centre) ;;

(* translation d'un vecteur nuls vers une case renvoie la case *)
assert (translate v_nuls r_case = r_case) ;;
   
(* 
  translation d'un n'importe quel vecteur vers centre renvoie une case 
  avec les coordonnees du vecteur. 
*) 
assert (translate r_vec centre = r_vec) ;;
   
(* Quelque tests *)
assert (translate (0, -1, 1) (-3, 2, 1) = (-3, 1, 2)) ;;
assert (translate (0, -1, 1) ( 0, 4,-4) = ( 0, 3,-3)) ;;
assert (translate (0, -1, 1) ( 0,-4, 4) = ( 0,-5, 5)) ;;


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
assert (diff_case r_case r_case = v_nuls) ;;

(* 
  difference entre c1 non nuls et l'centre (c2) renvoie le vecteur de
  translation qui est egal a c1 
*)
assert (diff_case r_case centre = r_case) ;;

(*
  difference entre c2 non nuls et l'centre (c1) renvoie le vecteur de
  translation qui est le symetrique de c2 
*)
assert (diff_case centre (-1, 1, 0) = ( 1,-1, 0)) ;;
assert (diff_case centre (-6, 3, 3) = ( 6,-3,-3)) ;;
assert (diff_case centre (-3, 6,-3) = ( 3,-6, 3)) ;;
assert (diff_case centre ( 3, 3,-6) = (-3,-3, 6)) ;;


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
assert (sont_cases_alignee ( 0,-1, 1) ( 0, 2,-2) = true) ;;
assert (sont_cases_alignee ( 0, 2,-2) ( 0,-1, 1) = true) ;;
assert (sont_cases_alignee ( 0,-3, 3) ( 0, 3,-3) = true) ;;

(* les cases alignees sur j *)
assert (sont_cases_alignee (-1, 0, 1) ( 2, 0,-2) = true) ;;
assert (sont_cases_alignee ( 2, 0,-2) (-1, 0, 1) = true) ;;
assert (sont_cases_alignee (-3, 0, 3) ( 3, 0,-3) = true) ;;

(* les cases alignees sur k *)
assert (sont_cases_alignee ( 1,-1, 0) (-2, 2, 0) = true) ;;
assert (sont_cases_alignee (-2, 2, 0) ( 1,-1, 0) = true) ;;
assert (sont_cases_alignee ( 3,-3, 0) (-3, 3, 0) = true) ;;


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
assert (dist_coords ( 1, 1,-2) ( 3, 3,-6) = (2,2, 4)) ;;
assert (dist_coords ( 0, 0, 0) ( 2, 2,-4) = (2,2, 4)) ;;
assert (dist_coords (-3,-3, 6) ( 3, 3,-6) = (6,6,12)) ;;
assert (dist_coords ( 0, 0, 0) ( 0, 0, 0) = (0,0, 0)) ;;


(* ======================================================================== *)


(**
  [(max_dist_cases c1 c2)] est la distance maximale entre les coordonnees des
  cases [c1] et [c2].
*)
let max_dist_cases (c1:case) (c2:case): int =
  let di, dj, dk = dist_coords c1 c2 in 
    max di (max dj dk) 
;;

assert (max_dist_cases ( 1, 1,-2) ( 3, 3,-6) =  4) ;;
assert (max_dist_cases ( 0, 0, 0) ( 2, 2,-4) =  4) ;;
assert (max_dist_cases (-3,-3, 6) ( 3, 3,-6) = 12) ;;
assert (max_dist_cases ( 0, 0, 0) ( 0, 0, 0) =  0) ;;


(* ======================================================================== *)


(**
  [(min_dist_cases c1 c2)] est la distance minimale entre les coordonnees des
  cases [c1] et [c2].
*)
let min_dist_cases (c1:case) (c2:case): int =
  let di, dj, dk = dist_coords c1 c2 in 
    min di (min dj dk)
;;

assert (min_dist_cases ( 1, 1,-2) ( 3, 3,-6) = 2) ;;
assert (min_dist_cases ( 0, 0, 0) ( 2, 2,-4) = 2) ;;
assert (min_dist_cases (-3,-3, 6) ( 3, 3,-6) = 6) ;;
assert (min_dist_cases ( 0, 0, 0) ( 0, 0, 0) = 0) ;;


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
assert (compte_cases r_case r_case = 0) ;;
assert (compte_cases centre centre = 0) ;;

(* entre c1 et c2: 1 cases *)
assert (compte_cases ( 0,-1, 1) ( 0, 1,-1) = 1) ;;
assert (compte_cases ( 1, 0,-1) (-1, 0, 1) = 1) ;;
assert (compte_cases (-1, 1, 0) ( 1,-1, 0) = 1) ;;
assert (compte_cases ( 0,-2, 2) ( 2, 0,-2) = 1) ;;
assert (compte_cases ( 2, 0,-2) (-2, 2, 0) = 1) ;;
assert (compte_cases (-2, 2, 0) ( 0,-2, 2) = 1) ;;

(* entre c1 et c2: 2 cases *)
assert (compte_cases ( 2,-3, 1) ( 2, 0,-2) = 2) ;;
assert (compte_cases ( 2,-2, 0) ( 2, 1,-1) = 2) ;;

(*   c1 et c2: 3 cases *)
assert (compte_cases ( 0,-2, 2) ( 0, 2,-2) = 3) ;;
assert (compte_cases ( 2, 0,-2) (-2, 0, 2) = 3) ;;
assert (compte_cases (-2, 2, 0) ( 2,-2, 0) = 3) ;;
assert (compte_cases ( 4,-2,-2) (-4, 2, 2) = 3) ;;
assert (compte_cases (-2, 4,-2) ( 2,-4, 2) = 3) ;;
assert (compte_cases (-2,-2, 4) ( 2, 2,-4) = 3) ;;

(*   c1 et c2: 4 cases *)
assert (compte_cases ( 3,-3, 0) (-2, 2, 0) = 4) ;;
assert (compte_cases ( 3,-2,-1) (-2, 1,-1) = 4) ;;

(*   c1 et c2: 5 cases *)
assert (compte_cases ( 0,-3, 3) ( 0, 3,-3) = 5) ;;
assert (compte_cases ( 3, 0,-3) (-3, 0, 3) = 5) ;;
assert (compte_cases (-3, 3, 0) ( 3,-3, 0) = 5) ;;
assert (compte_cases ( 6,-3,-3) (-6, 3, 3) = 5) ;;
assert (compte_cases (-3, 6,-3) ( 3,-6, 3) = 5) ;;
assert (compte_cases (-3,-3, 6) ( 3, 3,-6) = 5) ;;


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


assert (sont_cases_voisines (centre) ( 0,-1, 1) = true) ;;
assert (sont_cases_voisines (centre) ( 0, 1,-1) = true) ;;
assert (sont_cases_voisines (centre) (-1, 0, 1) = true) ;;
assert (sont_cases_voisines (centre) ( 1, 0,-1) = true) ;;
assert (sont_cases_voisines (centre) (-1, 1, 0) = true) ;;
assert (sont_cases_voisines (centre) ( 1,-1, 0) = true) ;;

assert (sont_cases_voisines ( 1, 0,-1) centre = true) ;;
assert (sont_cases_voisines ( 1, 0,-1) ( 1, 1,-2) = true) ;;
assert (sont_cases_voisines ( 1, 0,-1) ( 2, 0,-2) = true) ;;
assert (sont_cases_voisines ( 1, 0,-1) ( 2,-1,-1) = true) ;;
assert (sont_cases_voisines ( 1, 0,-1) ( 0, 1,-1) = true) ;;


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
assert (calcul_pivot ( 0,-1, 1) ( 0, 1,-1) = Some (centre)) ;;
assert (calcul_pivot ( 1, 0,-1) (-1, 0, 1) = Some (centre)) ;;
assert (calcul_pivot (-1, 1, 0) ( 1,-1, 0) = Some (centre)) ;;

(* entre c1 et c2: 3 cases, alignees *)
assert (calcul_pivot ( 0,-2, 2) ( 0, 2,-2) = Some (centre)) ;;
assert (calcul_pivot ( 2, 0,-2) (-2, 0, 2) = Some (centre)) ;;
assert (calcul_pivot (-2, 2, 0) ( 2,-2, 0) = Some (centre)) ;;

(* entre c1 et c2: 5 cases, alignees *)
assert (calcul_pivot ( 0,-3, 3) ( 0, 3,-3) = Some (centre)) ;;
assert (calcul_pivot ( 3, 0,-3) (-3, 0, 3) = Some (centre)) ;;
assert (calcul_pivot (-3, 3, 0) ( 3,-3, 0) = Some (centre)) ;;

(* entre c1 et c2: 1 cases, n'est pas alignees *)
assert (calcul_pivot ( 0,-2, 2) ( 2, 0,-2) = None) ;;
assert (calcul_pivot ( 2, 0,-2) (-2, 2, 0) = None) ;;
assert (calcul_pivot (-2, 2, 0) ( 0,-2, 2) = None) ;;

(* entre c1 et c2: 3 cases, n'est pas alignees *)
assert (calcul_pivot ( 4,-2,-2) (-4, 2, 2) = None) ;;
assert (calcul_pivot (-2, 4,-2) ( 2,-4, 2) = None) ;;
assert (calcul_pivot (-2,-2, 4) ( 2, 2,-4) = None) ;;

(* c1 et c2: 5 cases, n'est pas alignees *)
assert (calcul_pivot ( 6,-3,-3) (-6, 3, 3) = None) ;;
assert (calcul_pivot (-3, 6,-3) ( 3,-6, 3) = None) ;;
assert (calcul_pivot (-3,-3, 6) ( 3, 3,-6) = None) ;;


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
assert (vec_et_dist (-6, 3, 3) ( 6,-3,-3) = ((centre), 0)) ;;
assert (vec_et_dist ( 0,-3, 3) (-3, 3, 0) = ((centre), 0)) ;;
assert (vec_et_dist (centre) (centre) = ((centre), 0)) ;;

(* c1 et c2 alignees, c1 vers c2 *)
assert (vec_et_dist ( 0,-3, 3) centre = (( 0, 1,-1), 3)) ;;
assert (vec_et_dist ( 0, 3,-3) centre = (( 0,-1, 1), 3)) ;;
assert (vec_et_dist (-3, 3, 0) centre = (( 1,-1, 0), 3)) ;;
assert (vec_et_dist ( 3,-3, 0) centre = ((-1, 1, 0), 3)) ;;
assert (vec_et_dist (-3, 0, 3) centre = (( 1, 0,-1), 3)) ;;
assert (vec_et_dist ( 3, 0,-3) centre = ((-1, 0, 1), 3)) ;;

(* c1 et c2 alignees, c2 vers c1 *)
assert (vec_et_dist centre ( 0,-3, 3) = (( 0,-1, 1), 3)) ;;
assert (vec_et_dist centre ( 0, 3,-3) = (( 0, 1,-1), 3)) ;;
assert (vec_et_dist centre (-3, 3, 0) = ((-1, 1, 0), 3)) ;;
assert (vec_et_dist centre ( 3,-3, 0) = (( 1,-1, 0), 3)) ;;
assert (vec_et_dist centre (-3, 0, 3) = ((-1, 0, 1), 3)) ;;
assert (vec_et_dist centre ( 3, 0,-3) = (( 1, 0,-1), 3)) ;;


(* ======================================================================== *)
(* =================== AFFICHAGE DE PLATEAU DU JEU ======================== *)
(* ======================================================================== *)


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
let [@warning "-27"] rec
  associe (c:case) (lcc:case_coloree list) (defaut:couleur): couleur =
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


let conf_1 = ( [((0, 0, 0), Jaune)], [Jaune], 2) ;; 
affiche conf_1 ;;


let conf_reggae = (
  [((0,-1, 1),  Vert); ((0, 0, 0), Jaune);((0, 1,-1), Rouge)], 
  [Vert; Jaune; Rouge], 1
) ;; 
affiche conf_reggae ;;


let conf_vide = ([], [], 2);;
affiche conf_vide ;;


let conf_vide = ([], [], 1);;
affiche conf_vide ;;


(*
  A essayer apres avoir fait remplir_init
  affiche (remplir_init [Code "Ali"; Code "Bob"; Code "Jim"] 3);;
*)

(*Question 10*)

let tourner_liste lst =
  match lst with
  |[] -> []
  |hd::tl -> tl @ [hd]

let rec der_liste lst =
  match lst with
  |[] -> failwith "La liste est vide"
  |[x] -> x
  |_::tl -> der_liste tl

(*Question 11*)

let rec remplir_segment (i, j, k: case) (m:int): case list =
  if m <= 0 then []
  else (i, j, k) :: remplir_segment (i, j + 1, k - 1) (m - 1) ;;  

(*Question 12*)

let rec remplir_triangle_bas (i, j, k) m =
  if m <= 0 then []
  else 
    remplir_segment (i, j, k) m @ remplir_triangle_bas (i - 1, j + 1, k) (m - 1);;

(*Question 13*)

let rec remplir_triangle_haut (i, j, k) m =
  if m <= 0 then []
  else 
    remplir_segment (i, j, k) m @ remplir_triangle_haut (i + 1, j , k - 1) (m - 1);;


let board: case list = [(1, 2, 3); (1, 2, 3) ; (1, 2, 3) ; (1, 2, 3)] ;;

(*Question 14*)
let rec colorie (coul: couleur) (lc: case list) : case_coloree list = 
  (* let current_color = match coul with | Code _ -> Libre | _ -> coul in *)
  match lc with
  | [] -> []
  | h::q -> (h, coul)::(colorie coul q)
;;

(* Tests *)
(* let _ = assert ((colorie (Code "XUY") board) = [
  ((1, 2, 3), Libre) ; 
  ((1, 2, 3), Libre) ; 
  ((1, 2, 3), Libre) ; 
  ((1, 2, 3), Libre) 
]) ;; *)


(*Question 15*)

let rec tourner_cas_list (lc: case_coloree list): case_coloree list =
  match lc with
  | [] -> []
  | (cell, color)::q -> ((tourner_case 1 cell), color)::(tourner_cas_list q) 
;;

let rec tourner_cas_list_multiple (lc: case_coloree list) (n: int): case_coloree list =
  match n with
  | 0 -> lc
  | _ -> (tourner_cas_list_multiple (tourner_cas_list lc) (n - 1))
;;

let tourner_config (conf: configuration) : configuration =
  let grid, players, dim = conf in
  tourner_cas_list grid, players, dim
;;


let before_colored_board = [
  (tourner_case 0 (1, 2, 3), Libre) ; 
  (tourner_case 0 (1, 2, 3), Libre) ; 
  (tourner_case 0 (1, 2, 3), Libre) ; 
  (tourner_case 0 (1, 2, 3), Libre) 
] ;;


let after_colored_board = [
  (tourner_case 1 (1, 2, 3), Libre) ; 
  (tourner_case 1 (1, 2, 3), Libre) ; 
  (tourner_case 1 (1, 2, 3), Libre) ; 
  (tourner_case 1 (1, 2, 3), Libre) 
] ;;

let players: couleur list = [] ;;

let before_conf: configuration = (before_colored_board, players, 3) ;;
let after_conf: configuration = (after_colored_board, players, 3) ;;

let _ = assert ((tourner_config before_conf) = after_conf)

let rec length (array: 'a list): int = 
  match array with
  | [] -> 0
  | h::q -> 1 + length q
;;


let rec init_remplir_plateau (joueurs: couleur list) (dim: dimension) (nplayers: int): case_coloree list =
  let current_len = length joueurs in

  match joueurs with
  | [] -> []
  | h::q -> 
    (* let triangle = remplir_triangle_haut (dim + 1, -dim, -1) dim in *)
    let triangle = remplir_triangle_haut (dim + 1, -dim, -1) dim in
    let colored_triangle = colorie h triangle in
    let nrotations = 6 / nplayers in

    let current = tourner_cas_list_multiple colored_triangle (nrotations * (nplayers - current_len)) in 
    (* let current = colored_triangle in *)
    let next = init_remplir_plateau q dim nplayers in

    current @ next
;;



let remplir_init (joueurs: couleur list) (dim: dimension): configuration =
  let nbj = length joueurs in
  let plat = (init_remplir_plateau joueurs dim nbj) in
  plat, joueurs, dim 
;;

(* #trace remplir_triangle_haut
#trace colorie
#trace tourner_cas_list_multiple
#trace init_remplir_plateau
#trace remplir_init *)

let test_init_conf = (
  remplir_init 
  [Rouge ; Vert; Jaune; Marron; ]  4
) ;;



affiche(test_init_conf)

(* (i, j, k)
::(i + 1, j + 1, k + 1)
::(i + 1, j, k)
::(i, j + 1, k)
::(i, j, k + 1)
::(i + 1, j + 1, k)
::(i + 1, j, k + 1)
::(i, j + 1, k + 1)
::(i - 1, j - 1, k - 1)
::(i - 1, j, k)
::(i, j - 1, k)
::(i, j, k - 1)
::(i - 1, j - 1, k)
::(i - 1, j, k - 1)
::(i, j - 1, k - 1) *)

(*Question 18*)
let rec supprime_dans_config (conf:case_coloree list)(c:case):case_coloree list=
   match conf with
   | [] -> []
   | (c', _) :: tl when c = c' -> supprime_dans_config tl c
   | hd :: tl -> hd :: supprime_dans_config tl c;;

(*Question 19*)

