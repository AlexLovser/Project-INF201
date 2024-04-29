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

let rec tourner_config1 ((cc_list, c_list, dim):configuration):configuration=
  let n (c_list:couleur list):int= 6/(List.length c_list) in
  let rec tourner_cc (cc_list:case_coloree list):case_coloree list=
    match cc_list with
    |[]->[]
    |(cell, color)::q -> ((tourner_case (n c_list) cell), color)::(tourner_cc q) in
  tourner_cc cc_list, tourner_liste c_list, dim
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

(*Question 16*)
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


let rec tourner_joueurs (n: int) (plat: case_coloree list): case_coloree list =
  match n with
  | 0 -> plat
  | n -> tourner_joueurs (n - 1) (tourner_cas_list plat)

let remplir_init (joueurs: couleur list) (dim: dimension): configuration =
  let nbj = length joueurs in
  let plat = (init_remplir_plateau joueurs dim nbj) in
  tourner_joueurs 3 plat, joueurs, dim 
;;

(* #trace remplir_triangle_haut
#trace colorie
#trace tourner_cas_list_multiple
#trace init_remplir_plateau
#trace remplir_init *)

let test_init_conf = (
  remplir_init 
  [Rouge;]  3
) ;;



affiche(test_init_conf) ;;

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

(*Question 17*)

(* fonction associe se trouve avant la Question 10 *)

let quelle_couleur (c:case)((cc_list, c_list, dim):configuration):couleur= associe c cc_list Libre ;;

(* tests de la fonction quelle_couleur *) 

(* tests sur les configurations "test_init_conf" et "conf_reggae" *)

assert ((quelle_couleur ( 1,5,3) (test_init_conf)) = Libre);; 
assert ((quelle_couleur centre (test_init_conf)) = Libre);;
assert ((quelle_couleur ( -4,1,3) (test_init_conf)) = Rouge);;
assert ((quelle_couleur ( -6,3,3) (test_init_conf)) = Rouge);;
assert ((quelle_couleur ( 0,-1,1) (conf_reggae)) = Vert);;
assert ((quelle_couleur  centre (conf_reggae)) = Jaune);; 
assert ((quelle_couleur ( 0,1,-1) (conf_reggae)) = Rouge);;


(*Question 18*)

let rec supprime_dans_config (conf:case_coloree list)(c:case):case_coloree list=
   match conf with
   | [] -> []
   | (c', _) :: tl when c = c' -> supprime_dans_config tl c
   | hd :: tl -> hd :: supprime_dans_config tl c;;

(*Question 19*)

let rec trouver_couleur(conf: case_coloree list)(c:case):couleur= 
  match conf with
  |[]-> Libre
  |[(c, x)]->x
  |(v,x)::tl->if v=c then x else trouver_couleur tl c;;
  
let [@warning "-8"] est_coup_valide1((cc_list, c_list, dim):configuration)(Du(c1,c2):coup): bool= 
  if sont_cases_voisines c1 c2=true && 
     associe c1 cc_list Libre<>Libre && 
     associe c2 cc_list Libre=Libre && 
     est_dans_losange c2 dim= true && 
     associe c1 cc_list Libre =List.hd (c_list) then true 
  else false;;
(*Cette fonction ne prend en compte que les coups unitaires, la fonction prenant en compte les coups multuiples est à la question 25*)
(*Question 20*) 

let [@warning "-8"] appliquer_coup1 (((case, couleur)::tl, c_list, dim): configuration) (Du(c1, c2): coup) : configuration = 
  ((List.map (fun (case, couleur) -> if case = c1 then (c2, couleur) else (case, couleur)) ((case, couleur)::tl)), c_list, dim)
(* j'ai fais cette fonciton avec un List.map car on suppose que que le coup est valide*) ;;
(*Cette fonction ne prend en compte que les coups unitaires, la fonction prenant en compte les coups multuiples est à la question 25*)

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


(*Question 25*)

let [@warning "-8"] rec appliquer_coup (((case, couleur)::tl, c_list, dim): configuration) (c: coup) : configuration = 
  match c with
  |Du(c1,c2)-> ((List.map (fun (case, couleur) -> if case = c1 then (c2, couleur) else (case, couleur)) ((case, couleur)::tl)), c_list, dim)
  |Sm([])->((case, couleur)::tl, c_list, dim)
  |Sm([c1;c2])-> let cx= Du(c1,c2) in appliquer_coup ((case, couleur)::tl, c_list, dim) cx
  |Sm(hd::t1)->  let c1=hd in
      let c2 = der_liste t1 in ((List.map (fun (case, couleur) -> if case = c1 then (c2, couleur) else (case, couleur)) ((case, couleur)::tl)), c_list, dim);;
      
let rec est_coup_valide((cc_list, c_list, dim):configuration)(c:coup): bool= 
  match c with
  |Du(c1,c2)->if sont_cases_voisines c1 c2=true && 
                 associe c1 cc_list Libre= List.hd (c_list) && 
                 associe c2 cc_list Libre=Libre && 
                 est_dans_losange c2 dim= true then true else false
  |Sm([])-> false
  |Sm([c1])-> if est_dans_losange c1 dim=true &&
                 associe c1 cc_list Libre= List.hd (c_list) then true else false
  |Sm([c1;c2])-> let a,b,c=c1 in
      let x,y,z=c2 in
      let case_sautee =((x+a)/2, (y+b)/2, (z+c)/2) in if associe case_sautee cc_list Libre<>Libre && 
                                                         associe c1 cc_list Libre= List.hd (c_list) &&
                                                         sont_cases_voisines c1 case_sautee=true &&
                                                         sont_cases_voisines c2 case_sautee=true &&
                                                         est_dans_losange c2 dim then true else false 
  |Sm(hd::tl)-> let c1=hd in
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

(*Question 21*) 

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


(*Question 22*) 
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

(* tests sur des cas où les cases alignées entre c1 et c2 sont libres *)
assert ((est_libre_seg(-4,3,1)(3,3,-6)(test_init_conf))=true);; 
assert ((est_libre_seg(-4,3,1)(3,-4,1)(test_init_conf))=true);; 
assert ((est_libre_seg(-4,1,3)(3,1,-4)(test_init_conf))=true);; 
assert ((est_libre_seg(-4,1,3)(3,-6,3)(test_init_conf))=true);; 

(* tests sur des cas où les cases alignées entre c1 et c2 ne sont pas libres *)
assert ((est_libre_seg(-5,2,3)(3,-6,3)(test_init_conf))=false);; 
assert ((est_libre_seg(-5,3,2)(3,3,-6)(test_init_conf))=false);; 

  
(*Question 23*) 
  
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
  
(*Question 24*) 
  
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


(*Question 26*)
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


(*Question 27*)
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

(*Question 28*)
let rec est_partie ((cc_list,c_list,dim):configuration) (l:coup list): couleur= 
  match l with
  |[]-> Libre
  |hd::tl-> if est_coup_valide (cc_list,c_list,dim) hd = false then failwith "coup non valide" 
      else let conf2= mettre_a_jour_configuration (cc_list,c_list,dim) hd in
        if gagne conf2 = true then List.hd c_list 
        else
          let conf3 = tourner_config1 conf2
          in est_partie conf3 tl;;

let conf_new=([((-2,1,1), Vert);((2,-1,-1),Jaune)],[Vert;Jaune],1);; 
let l_c1=[Du((-2,1,1),(-1,0,1));Du((-2,1,1),(-1,0,1));Du((-1,0,1),(0,-1,1));Du((-1,0,1),(0,-1,1));Du((0,-1,1),(1,-1,0));Du((0,-1,1),(1,-1,0));Du((1,-1,0),(2,-1,-1));Du((1,-1,0),(2,-1,-1))];;
let l_c2=[Du((-2,1,1),(-1,0,1));Du((-2,1,1),(-1,0,1));Du((-1,0,1),(0,-1,1));Du((-1,0,1),(0,-1,1));Du((0,-1,1),(1,-1,0));Du((0,-1,1),(1,-1,0));Du((1,-1,0),(0,0,0));Du((1,-1,0),(2,-1,-1))];;

est_partie conf_new l_c1 ;;
est_partie conf_new l_c2 ;;

assert ((est_partie conf_new l_c1 )=Vert);;
assert ((est_partie conf_new l_c2 )=Jaune);;

(*Question 29*) 
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

(*Question 30*)
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


  
