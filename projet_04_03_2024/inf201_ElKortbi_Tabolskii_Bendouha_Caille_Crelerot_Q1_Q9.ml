(* ----------------------------------------------------------------------------
   inf201_El_Kortbi_Tabolskii_Q1-Q9.ml : cr Q1 Q9 projet: Groupe ima4_C

   Yassin El Kortbi    <elkortby@etu.univ-grenoble-alpes.fr> 
   Aleksandr Tabolskii <aleksandr.tabolskii@etu.univ-grenoble-alpes.fr>  
   Daniel Caille       <daniel.caille@etu.univ-grenoble-alpes.fr>
   Thomas Crelerot     <thomas.crelerot@etu.univ-grenoble-alpes.fr>
   Akram Bendouha      <akram.bendouha@etu.univ-grenoble-alpes.fr 
  -------------------------------------------------------------------------- *)

(* REMARQUE:
  il y a quelques types/fonctions avec des warning, c'est a cause de dune pour 
  bien l'executer.
*)

(* https://github.com/AlexLovser/Project-INF201 *)


(** 
  Dimension d'un plateau, noté [dim] par la suite, est un paramètre qui encode 
  la taille du plateau. Le plateau a [4 * dim + 1] lignes horizontales que nous
  numérotons de bas en haut de [-2 * dim] à [2 * dim] et similairement pour les
  lignes obliques.
*)
type dimension = int ;;


(** 
  La case est définie par trois coordonnées [(i, j, k)], la case au centre
  du plateau de jeu a pour coordonnées [(0, 0, 0)]. Les coordonnées
  représentent:

  - i le numéro de la ligne horizontale ;
  - j le numéro de la ligne horizontale lorsqu'on a tourné le plateau d'un 
    tiers de tour dans le sens anti-horaire ;
  - k le numéro de la ligne horizontale lorsqu'on a tourné le plateau d'un
    tiers de tour dans le sens horaire.
*)
type case = int * int * int ;;


(** 
  Les couleurs des joueurs. Le constructeur [Code] permet d'entrer les noms de
  joueur restreint à trois caractères. La couleur [Libre] est une couleur en
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
  Un pion d'une couleur [col] se situe sur une case [c] est codé par un couple
  [(c, col)] que l'on appelle une case colorée.
*)
type case_coloree  = case * couleur ;;


(**
  Le [configuration] du jeu est donnée par un triplet formé d'une liste de
  cases colorées, une liste de joueurs et une dimension. La liste de cases 
  colorées donne l'emplacement des pions et leurs couleurs. On veillera à ce 
  que pour chaque case [c] il y ait au plus un pion sur cette case, 
  c'est-à-dire il y a au plus une couleur [col] tel que le couple [(c, col)]
  est dans la liste; l'absence de pion sur la case [c] sera codé par l'absence
  de couple [(c, col)] dans la liste et non pas avec [(c, Libre)]. La liste de
  joueur permet de savoir à qui est le tour (tête de liste) et quel sera le 
  tour des suivants (en suivant l'ordre de la liste). Enfin même si elle ne 
  change pas au cours de la partie la [dimension] est donnée dans la 
  configuration car nous devons pouvoir accéder facilement à celle-ci et 
  pouvoir en changer si nous souhaitons faire une partie sur un plateau de 
  taille différente.
*)
type configuration = case_coloree list * couleur list * dimension ;;


(**
  Les coups seront décrits plus tard. Il en existe de deux sortes:
  - les déplacements unitaires (constructeur [Du])
  - les sauts multiples (constructeur [Sm])
*)
type [@warning "-34"] coup = 
| Du of case * case [@warning "-37"]  
| Sm of case list   [@warning "-37"] 
;;


(**
  Le type [vecteur] est synonyme du type [case] comme un vecteur permettant des
  translation avec les même proprièt
*)
type vecteur = case ;; 


(**
  [(indice_valide x dim)] vérifie si la coordonnée [x] et valide dans la 
  dimension [dim].
*)
let indice_valide (x:int) (dim:dimension): bool =
  -2 * dim <= x && x <= 2 * dim
;;


(**
  [(est_case c)] vérifie si [c] est une case.
*)
let est_case (c:case): bool = 
  let i, j, k = c in i + j + k = 0 
;;

(**
  [(est_dans_losange c dim)] vérifie si la case [c] est dans le losange 
  North-South du plateau de dimension [dim].
*)
let est_dans_losange (c:case) (dim:dimension): bool = 
  let _, j, k = c in
    -dim <= j && j <= dim && 
    -dim <= k && k <= dim
;;    


(**
  [(est_dans_losange_2 c dim)] vérifie si la case [c] est dans le losange 
  Northwest-Southeast du plateau de dimension [dim].
*)
let est_dans_losange_2 (c:case) (dim:dimension): bool = 
  let i, _, k = c in
    -dim <= i && i <= dim && 
    -dim <= k && k <= dim
;;


(**
  [(est_dans_losange_3 c dim)] vérifie si la case [c] est dans le losange 
  Northeast-Southwest du plateau de dimension [dim].
*)
let est_dans_losange_3 (c:case) (dim:dimension): bool = 
  let i, j, _ = c in
    -dim <= i && i <= dim && 
    -dim <= j && j <= dim
;;

(**
  [(est_dans_etoile) c dim] vérifie si la case [c] est dans l'étoile du plateau
  de dimension [dim].
*)
let est_dans_etoile (c:case) (dim:dimension): bool =
  (* l'union de trois losange est un étoile *)
  est_dans_losange c dim || 
  est_dans_losange_2 c dim || 
  est_dans_losange_3 c dim
;;


(**
  [(tourner_case m c)] c'est la case [c] après avoir fait tourner le plateau de
  [m] sixième de tour dans le sens anti-horaire.
*)
let tourner_case (m:int) (c:case): case =
  (* réduction de nombre de fait pour tourner *)
  let m = m mod 6 in
  (* équation récursive *)
  let rec tourner_case_rec (m:int) (c:case): case = 
    let i, j, k = c in
    match m with
    | 0 -> i, j, k
    | m -> tourner_case_rec (m - 1) (-k, -i, -j) 
  in tourner_case_rec m c
;;


(**
  [(translate c v)] calcule la case par le translation du vecteur [v] à partir 
  de [c].
*)
let translate (c:case) (v:vecteur): case =
  let c1, c2, c3 = c    (* les coordonnées de la case c *)
  and v1, v2, v3 = v in (* les coordonnées du vectuer v *)
    v1 + c1, v2 + c2, v3 + c3 (* translation des coordonnées de v vers c *)
;;


(**
  [(diff_case c1 c2)] c'est le vecteur de translation de [c2] vers [c1], 
  calculer par la différence entre les cases [c1] et [c2].
*)
let diff_case (c1:case) (c2:case): vecteur =
  let i1, j1, k1 = c1 
  and i2, j2, k2 = c2 in 
    (* la différence entre les coordonnées c1 et c2 *)
    i1 - i2, j1 - j2, k1 - k2 
;;


(**
  [(sont_cases_alignee c1 c2)] vérifie si les cases [c1] et [c2] sont alignées.
*)
let sont_cases_alignee (c1:case) (c2:case): bool =
  let i1, j1, k1 = c1 
  and i2, j2, k2 = c2 in 
    match () with (* comparasion entre chaque de coordonnées *)
    | _ when c1 = c2 -> false (* les doivent être de différentes valeurs *)
    | _ when i1 = i2 -> true (* s'ils sont alignées sur i, donc vrai *)
    | _ when j1 = j2 -> true (* s'ils sont alignées sur j, donc vrai *)
    | _ when k1 = k2 -> true (* s'ils sont alignées sur k, donc vrai *)
    | _ -> false (* si les cases ne sont pas alignées *)
;;


(** 
  [(dist_coords c1 c2)] est un triplet de distances entre les coordonnées des 
  cases [c1] et [c2].
*)
let dist_coords (c1:case) (c2:case): int * int * int =
  let i1, j1, k1 = c1
  and i2, j2, k2 = c2 in 
    let di = abs (i1 - i2) (* distance entre les coordonnées i *)
    and dj = abs (j1 - j2) (* distance entre les coordonnées j *)
    and dk = abs (k1 - k2) (* distance entre les coordonnées k *)
      in di, dj, dk (* triplet des distances entres i, j et k *)
;;


(**
  [(max_dist_cases c1 c2)] est la distance maximale entre les coordonnées des 
  cases [c1] et [c2].
*)
let max_dist_cases (c1:case) (c2:case): int =
  let di, dj, dk = dist_coords c1 c2 in 
    max di (max dj dk) 
;;



(**
  [(min_dist_cases c1 c2)] est la distance minimale entre les coordonnées des 
  cases [c1] et [c2].
*)
let min_dist_cases (c1:case) (c2:case): int =
  let di, dj, dk = dist_coords c1 c2 in 
    min di (min dj dk)
;;


(**
  [(compte_cases c1 c2)] est le nombre de cases entres les cases [c1] et [c2]. 
  Pour determiner ce nombre on prendent la distance maximale si ils sont 
  alignées, sinon la distance minimale.
*)
let compte_cases (c1:case) (c2:case): int = 
  match () with
  (* si les cases sont égal *)
  | _ when c1 = c2 -> 0
  (* si les cases sont alignées *)
  | _ when sont_cases_alignee c1 c2 -> max_dist_cases c1 c2 - 1
  (* sinon ... *)
  | _ -> min_dist_cases c1 c2 - 1 
;;


(**
  [(sont_cases_voisines c1 c2)] vérifie si les cases [c1] et [c2] sont 
  voisines.
*)
let sont_cases_voisines (c1:case) (c2:case): bool =
  (* si les cases sont alignées et la distances entre eux est 1 *)
   sont_cases_alignee c1 c2 && max_dist_cases c1 c2 = 1
 ;;


(**
  [(calcul_pivot c1 c2)] calcul le pivot entre les cases [c1] et [c2] si ils 
  sont alignées et le nombre de cases entre les deux est impair, sinon [None].
*)
let calcul_pivot (c1:case) (c2:case): case option =
  (* si le nombre de cases entre c1 et c2 est impair *)
  let est_impair = (compte_cases c1 c2) mod 2 = 1 
  (* les coordonnées du vecteur de translation de c2 vers c1 *)
  and i, j, k = diff_case c1 c2 in 
  (* le vecteur de translation de c2 vers le mi-chemin de c1 *) 
    let v = i/2, j/2, k/2 in
    (* les coordonnées de pivot *)
    let p = translate c2 v in
    if est_impair && sont_cases_alignee c1 c2 
      then Some(p) (* si impair et alignées, pivot existe *)
      else None    (* sinon, pivot n'existe pas *)
;;


(**
  [(vec_et_dist c1 c2)] est le couple [(v, d)] avec [v] le vecteur de 
  translation d'un déplacement unitaire des cases alignées [c1] vers [c2] et 
  avec [d] la distance entre c'est cases. Si le vecteur unitaire n'existe pas, 
  alors en renvoie [((0, 0, 0), 0)].
*)
let vec_et_dist (c1:case) (c2:case): vecteur * int =
  (* si c1 = c2 ou non alignées renvoie nuls *)
  if c1 = c2 || not (sont_cases_alignee c1 c2) then (0, 0, 0), 0
  else (* sinon ... *)
    (* la distance entres les cases *)
    let d = max_dist_cases c1 c2
    (* les coordonnées du vecteur de translation de c2 vers c1 *)
    and i, j, k = diff_case c1 c2 in 
    (* les coordonnées du vecteur de translation unitaire de *)
    (* c2 vers c1 *)
    let i, j, k = i/d, j/d, k/d in
    (* le vecteur de translation unitaire de c1 vers c2 *)
    let v = i * (-1), j * (-1), k * (-1) in
      if est_case v 
        then v, d (* si c'est un vecteur *)
        else (0, 0, 0), 0 (*'sinon  *)
;;


(*
  =============================================================================
  | AFFICHAGE                                                                 |
  =============================================================================
*)

(** 
  Transforme des coordonnees cartesiennes [(x, y)] en coordonnees de case
  [(i, j, k)]
*)
let transfo x y = (y, (x - y) / 2,(-x - y) / 2);;


let [@warning "-27"] associe (a:'a) (l:('a*'b) list) (defaut:'b): 'b = 
  defaut
;;


let couleur2string (coul:couleur):string =
  match coul with
  | Libre  -> " . "
  | Vert   -> " V "
  | Jaune  -> " J "
  | Rouge  -> " R "
  | Noir   -> " N "
  | Bleu   -> " B "
  | Marron -> " M "
  | Code s -> s  ;;


let rec affiche_ligne (n:int) (m:int) (config:configuration): string =
  let (lcc, _, dim) = config in
    if m = 4 * dim + 1 then " " (* fin de ligne *)
    else
      let c = transfo m n in
      if not ((n + m) mod 2 = 0) || not (est_dans_etoile c dim) then (*ceci est
        une inter-case (case inutile d'un damier) ou hors de l'etoile *)
        "   " ^ affiche_ligne n (m + 1) config
      else (*ceci est une case ou bien en dehors du plateau*)
       (couleur2string (associe c lcc Libre)) ^ affiche_ligne n (m + 1) config
;;


let affiche (config:configuration): unit =
  let (_, _, dim) = config in
    let rec affiche_aux n =
      if n = - 2 * dim - 1 then ()
      else begin
        print_endline (affiche_ligne n (-4 * dim - 1) config);
        print_endline "\n";
        affiche_aux (n - 1)
      end 
    in affiche_aux (2*dim+1);;


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

(*
  =============================================================================
  | TESTS                                                                     |
  =============================================================================
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
  if c <> (0, 0, 0) && est_case c && est_dans_etoile c dim then c 
  else randcase dim
;;

(**
  Renvoie un vecteur alèatoire non nuls de la taille 1.
*)
let randvec (): vecteur = randcase 1 ;;

(* dimenstion du plateau *)
let dim : dimension = 3 ;; 

(* l'centre du plateau et vecteur nuls *)
let centre : case = (0, 0, 0) ;; 
let v_nuls : vecteur = (0, 0, 0) ;;

(* une case alèatoire de taille dim *)
let r_case : case = randcase dim ;;

(* un vecteur avec une combinaison de -1, 0 et 1 non nuls *)
let r_vec : vecteur = randvec () ;;


(* Testing de fonctions *)

print_endline "Testing: Begin" ;;


print_endline "Testing: 'randint'" ;;

(* si l'intervale est sigleton n, alors le nombre est n *)
assert(randint (-1) (-1) = -1) ;;
assert(randint 0 0 = 0) ;;
assert(randint 1 1 = 1) ;;

(* le nombre n est entre les bornes n - a et n + b *)
assert(
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
) ;;


print_endline "Testing: 'randtriplet'" ;;

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


print_endline "Testing: 'randcase'" ;;

(* un case aléatoire est toujour un case *)
assert(let c = randcase 1 in c <> centre && est_case c) ;;
assert(let c = randcase 2 in c <> centre && est_case c) ;;
assert(let c = randcase 3 in c <> centre && est_case c) ;;
assert(let c = randcase 4 in c <> centre && est_case c) ;;
assert(let c = randcase 5 in c <> centre && est_case c) ;;


print_endline "Testing: 'randvec'" ;;

(* un vecteur aléatoire est toujour un vecteur *)
assert(let v = randvec () in v <> centre && est_case v) ;;
assert(let v = randvec () in v <> centre && est_case v) ;;
assert(let v = randvec () in v <> centre && est_case v) ;;
assert(let v = randvec () in v <> centre && est_case v) ;;
assert(let v = randvec () in v <> centre && est_case v) ;;


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

(* centre du plateau *)
assert (est_dans_losange centre dim = true) ;; 

(* coin droite *)
assert (est_dans_losange ( 0, 3,-3) dim = true) ;;

(* coin inférieur *)
assert (est_dans_losange ( 6,-3,-3) dim = true) ;;


print_endline "Testing: 'est_dans_losange_2'" ;;

(* coin supérieur gauche *)
assert (est_dans_losange_2 ( 3,-6, 3) dim = true) ;; 

(* coin supérieur droite *)
assert (est_dans_losange_2 ( 3, 0,-3) dim = true) ;; 

(* centre du plateau *)
assert (est_dans_losange_2 centre dim = true) ;; 

(* coin inférieur gauche *)
assert (est_dans_losange_2 (-3, 0, 3) dim = true) ;; 

(* coin inférieur droite *)
assert (est_dans_losange_2 (-3, 6,-3) dim = true) ;; 


print_endline "Testing: 'est_dans_losange_3'" ;;

(* coin supérieur gauche *)
assert (est_dans_losange_3 ( 3,-3, 0) dim = true) ;; 

(* coin supérieur droite *)
assert (est_dans_losange_3 ( 3, 3,-6) dim = true) ;; 

(* centre du plateau *)
assert (est_dans_losange_3 centre dim = true) ;; 

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

(* centre du plateau *)
assert (est_dans_etoile centre dim = true) ;;

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

(* la case du centre tourne vers lui même dans tous les cas *)
assert (tourner_case (randint 0 1000) centre = centre) ;;


print_endline "Testing: 'translate'" ;;

(* translation d'un vecteur nuls par l'centre est une case 
   centre *)
assert (translate v_nuls centre = centre) ;;

(* translation d'un vecteur nuls vers une case renvoie la case *)
assert (translate v_nuls r_case = r_case) ;;

(* translation d'un n'importe quel vecteur vers centre renvoie une case 
  avec les coordonnées du vecteur. *)
assert (translate r_vec centre = r_vec) ;;

(* Quelque tests *)
assert (translate (0, -1, 1) (-3, 2, 1) = (-3, 1, 2)) ;;
assert (translate (0, -1, 1) ( 0, 4,-4) = ( 0, 3,-3)) ;;
assert (translate (0, -1, 1) ( 0,-4, 4) = ( 0,-5, 5)) ;;


print_endline "Testing: 'diff_case'" ;;

(* difference entre deux cases identique renvoie un vecteur nuls *)
assert (diff_case r_case r_case = v_nuls) ;;

(* difference entre c1 non nuls et l'centre (c2) renvoie le vecteur de
   translation qui est égal à c1 *)
assert (diff_case r_case centre = r_case) ;;

(* difference entre c2 non nuls et l'centre (c1) renvoie le vecteur de
   translation qui est le symetrique de c2 *)
assert (diff_case centre (-1, 1, 0) = ( 1,-1, 0)) ;;
assert (diff_case centre (-6, 3, 3) = ( 6,-3,-3)) ;;
assert (diff_case centre (-3, 6,-3) = ( 3,-6, 3)) ;;
assert (diff_case centre ( 3, 3,-6) = (-3,-3, 6)) ;;


print_endline "Testing: 'sont_cases_alignee'" ;;

(* les cases alignées de coordonnées i *)
assert (sont_cases_alignee ( 0,-1, 1) ( 0, 2,-2) = true) ;;
assert (sont_cases_alignee ( 0, 2,-2) ( 0,-1, 1) = true) ;;
assert (sont_cases_alignee ( 0,-3, 3) ( 0, 3,-3) = true) ;;

(* les cases alignées de coordonnées j *)
assert (sont_cases_alignee (-1, 0, 1) ( 2, 0,-2) = true) ;;
assert (sont_cases_alignee ( 2, 0,-2) (-1, 0, 1) = true) ;;
assert (sont_cases_alignee (-3, 0, 3) ( 3, 0,-3) = true) ;;

(* les cases alignées de coordonnées k *)
assert (sont_cases_alignee ( 1,-1, 0) (-2, 2, 0) = true) ;;
assert (sont_cases_alignee (-2, 2, 0) ( 1,-1, 0) = true) ;;
assert (sont_cases_alignee ( 3,-3, 0) (-3, 3, 0) = true) ;;


print_endline "Testing: 'dist_coords'" ;;

(* calcul de distances entres les coordonnées de cases *)
assert (dist_coords ( 1, 1,-2) ( 3, 3,-6) = (2,2, 4)) ;;
assert (dist_coords ( 0, 0, 0) ( 2, 2,-4) = (2,2, 4)) ;;
assert (dist_coords (-3,-3, 6) ( 3, 3,-6) = (6,6,12)) ;;
assert (dist_coords ( 0, 0, 0) ( 0, 0, 0) = (0,0, 0)) ;;


print_endline "Testing: 'max_dist_cases'" ;;

assert (max_dist_cases ( 1, 1,-2) ( 3, 3,-6) =  4) ;;
assert (max_dist_cases ( 0, 0, 0) ( 2, 2,-4) =  4) ;;
assert (max_dist_cases (-3,-3, 6) ( 3, 3,-6) = 12) ;;
assert (max_dist_cases ( 0, 0, 0) ( 0, 0, 0) =  0) ;;


print_endline "Testing: 'min_dist_cases'" ;;

assert (min_dist_cases ( 1, 1,-2) ( 3, 3,-6) = 2) ;;
assert (min_dist_cases ( 0, 0, 0) ( 2, 2,-4) = 2) ;;
assert (min_dist_cases (-3,-3, 6) ( 3, 3,-6) = 6) ;;
assert (min_dist_cases ( 0, 0, 0) ( 0, 0, 0) = 0) ;;


print_endline "Testing: 'compte_cases'" ;;

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


print_endline "Testing: 'sont_cases_voisines'" ;;

assert (sont_cases_voisines (centre) ( 0,-1, 1) = true) ;;
assert (sont_cases_voisines (centre) ( 0, 1,-1) = true) ;;
assert (sont_cases_voisines (centre) (-1, 0, 1) = true) ;;
assert (sont_cases_voisines (centre) ( 1, 0,-1) = true) ;;
assert (sont_cases_voisines (centre) (-1, 1, 0) = true) ;;
assert (sont_cases_voisines (centre) ( 1,-1, 0) = true) ;;


print_endline "Testing: 'calcul_pivot'" ;;

(* entre c1 et c2: 1 cases, alignées *)
assert (calcul_pivot ( 0,-1, 1) ( 0, 1,-1) = Some (centre)) ;;
assert (calcul_pivot ( 1, 0,-1) (-1, 0, 1) = Some (centre)) ;;
assert (calcul_pivot (-1, 1, 0) ( 1,-1, 0) = Some (centre)) ;;

(* entre c1 et c2: 3 cases, alignées *)
assert (calcul_pivot ( 0,-2, 2) ( 0, 2,-2) = Some (centre)) ;;
assert (calcul_pivot ( 2, 0,-2) (-2, 0, 2) = Some (centre)) ;;
assert (calcul_pivot (-2, 2, 0) ( 2,-2, 0) = Some (centre)) ;;

(* entre c1 et c2: 5 cases, alignées *)
assert (calcul_pivot ( 0,-3, 3) ( 0, 3,-3) = Some (centre)) ;;
assert (calcul_pivot ( 3, 0,-3) (-3, 0, 3) = Some (centre)) ;;
assert (calcul_pivot (-3, 3, 0) ( 3,-3, 0) = Some (centre)) ;;

(* entre c1 et c2: 1 cases, n'est pas alignées *)
assert (calcul_pivot ( 0,-2, 2) ( 2, 0,-2) = None) ;;
assert (calcul_pivot ( 2, 0,-2) (-2, 2, 0) = None) ;;
assert (calcul_pivot (-2, 2, 0) ( 0,-2, 2) = None) ;;

(* entre c1 et c2: 3 cases, n'est pas alignées *)
assert (calcul_pivot ( 4,-2,-2) (-4, 2, 2) = None) ;;
assert (calcul_pivot (-2, 4,-2) ( 2,-4, 2) = None) ;;
assert (calcul_pivot (-2,-2, 4) ( 2, 2,-4) = None) ;;

(* c1 et c2: 5 cases, n'est pas alignées *)
assert (calcul_pivot ( 6,-3,-3) (-6, 3, 3) = None) ;;
assert (calcul_pivot (-3, 6,-3) ( 3,-6, 3) = None) ;;
assert (calcul_pivot (-3,-3, 6) ( 3, 3,-6) = None) ;;


print_endline "Testing: 'vec_et_dist'" ;;

(* c1 et c2 non alignées *)
assert (vec_et_dist (-6, 3, 3) ( 6,-3,-3) = ((centre), 0)) ;;
assert (vec_et_dist ( 0,-3, 3) (-3, 3, 0) = ((centre), 0)) ;;
assert (vec_et_dist (centre) (centre) = ((centre), 0)) ;;

(* c1 et c2 alignées, c1 vers c2 *)
assert (vec_et_dist ( 0,-3, 3) centre = (( 0, 1,-1), 3)) ;;
assert (vec_et_dist ( 0, 3,-3) centre = (( 0,-1, 1), 3)) ;;
assert (vec_et_dist (-3, 3, 0) centre = (( 1,-1, 0), 3)) ;;
assert (vec_et_dist ( 3,-3, 0) centre = ((-1, 1, 0), 3)) ;;
assert (vec_et_dist (-3, 0, 3) centre = (( 1, 0,-1), 3)) ;;
assert (vec_et_dist ( 3, 0,-3) centre = ((-1, 0, 1), 3)) ;;

(* c1 et c2 alignées, c2 vers c1 *)
assert (vec_et_dist centre ( 0,-3, 3) = (( 0,-1, 1), 3)) ;;
assert (vec_et_dist centre ( 0, 3,-3) = (( 0, 1,-1), 3)) ;;
assert (vec_et_dist centre (-3, 3, 0) = ((-1, 1, 0), 3)) ;;
assert (vec_et_dist centre ( 3,-3, 0) = (( 1,-1, 0), 3)) ;;
assert (vec_et_dist centre (-3, 0, 3) = ((-1, 0, 1), 3)) ;;
assert (vec_et_dist centre ( 3, 0,-3) = (( 1, 0,-1), 3)) ;;

print_endline "Testing: END" ;;
