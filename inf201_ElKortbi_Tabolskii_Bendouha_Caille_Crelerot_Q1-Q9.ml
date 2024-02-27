(* ---------------------------------------------------------------------
   inf201_El_Kortbi_Tabolskii_Q1-Q9.ml : cr Q1 Q9 projet: Groupe ima4_C

   Yassin El Kortbi    <elkortby@etu.univ-grenoble-alpes.fr> 
   Aleksandr Tabolskii <aleksandr.tabolskii@etu.univ-grenoble-alpes.fr>  
  ---------------------------------------------------------------------- *)

(* https://github.com/AlexLovser/Project-INF201 *)

(** 
  La [dimension] d'un plateau, noté [dim] par la suite, est un paramètre qui
  encode la taille du plateau. Le plateau a [4 * dim + 1] lignes horizontales
  que nous numérotons de bas en haut de [-2 * dim] à [2 * dim] et 
  similairement pour les lignes obliques.
*)
type dimension = int ;;


(** 
  Une [case] est définie par trois coordonnées [(i, j, k)], la case au centre
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
  Le type [couleur] représente les couleurs des joueurs. Le constructeur
  [Code] vous permet d'entrer vos propes noms de joueur restreint à trois
  caractères. La couleur [Libre] est une couleur en plus pour coder
  l'absence de joueur (dans une case ou pour le gagnant d'une partie).
*)
type couleur = Vert | Jaune | Rouge | Noir | Bleu | Marron
               | Libre 
               | Code of string  
;;


(**
  Le fait qu'un pion d'une couleur [col] se situe sur une case [c] est codé
  par un couple [(c, col)] que l'on appelle une case colorée.
*)
type case_coloree  = case * couleur ;;


(**
  Le [configuration] du jeu est donnée par un triplet formé d'une liste de
  cases colorées, une liste de joueurs et une dimension (un entier). La 
  liste decases colorées donne l'emplacement des pions et leurs couleurs. On
  veillera à ce que pour chaque case [c] il y ait au plus un pion sur cette
  case, c'est-à-dire il y a au plus une couleur [col] tel que le couple [(c, 
  col)] est dans la liste; l'absence de pion sur la case [c] sera codé par
  l'absence de couple [(c, col)] dans la liste et non pas avec [(c, Libre)].
  La liste de joueur permet de savoir à qui est le tour (tête de liste) et
  quel sera le tour des suivants (en suivant l'ordre de la liste). Enfin
  même si elle ne change pas au cours de la partie la [dimension] est donnée
  dans la configuration car nous devons pouvoir accéder facilement à celle-ci
  et pouvoir en changer si nous souhaitons faire une partie sur un plateau
  de taille différente.
*)
type configuration = case_coloree list * couleur list * dimension ;;


(**
  Les coups seront décrits plus tard. Il en existe de deux sortes:
  - les déplacements unitaires (constructeur [Du])
  - les sauts multiples (constructeur [Sm])
*)
type coup = Du of case * case | Sm of case list ;;


(**
  Nous définissons le type [vecteur] comme synonyme du type [case] car les
  triplets d'entiers [(i, j, k)] tels, que [i + j + k = 0] servent aussi
  bien comme case de la grille que comme vecteur permettant des translation.
*)
type vecteur = case ;; 


(**
  Vérifie si la coordonnée [x] et valide dont la dimension [dim].
*)
let indice_valide (x:int) (dim:dimension): bool =
  -2 * dim <= x && x <= 2 * dim
;;

(**
  Vérifie si [c] est une case, par [i + j + k = 0].
*)
let est_case (c:case): bool = let i, j, k = c in i + j + k = 0 ;;

(**
  Vérifie si la case [c]  est une case dans le losange North-South d'étoile 
  du plateau de dimension [dim].
*)
let est_dans_losange (c:case) (dim:dimension): bool = 
  let i, j, k = c in
    -dim <= j && j <= dim && 
    -dim <= k && k <= dim
;;     

(**
  Vérifie si la case [c] est dans le losange Northwest-Southeast d'étoile 
  du plateau de dimension [dim].
*)
let est_dans_losange_2 (c:case) (dim:dimension): bool = 
  let i, j, k = c in
    -dim <= i && i <= dim && 
    -dim <= k && k <= dim
;; 


(**
  Vérifie si la case [c] est dans le losange Northeast-Southwest d'étoile
  du plateau de dimension [dim].
*)
let est_dans_losange_3 (c:case) (dim:dimension): bool = 
  let i, j, k = c in
    -dim <= i && i <= dim && 
    -dim <= j && j <= dim
;; 


(**
  Vérifie si la case [c] est dans l'étoile du plateau de dimension [dim].
*)
let est_dans_etoile (c:case) (dim:dimension): bool =
  est_dans_losange c dim || 
  est_dans_losange_2 c dim || 
  est_dans_losange_3 c dim
;;

(**
  La case [c] est la case après avoir fait tourner le plateau de [m] sixième de
  tour dans le sens anti-horaire. Au debut on calcule [m mod 6],
  car chaque foit quand on tourne le sixieme foit on renvien a la position
  initiale, puis on effectue une transformation de case [c] avec les 
  coordonnées [(i, j, k)] vers [(-k, -i, -j)] récursivement [m] fois.
*)
let rec tourner_case (m:int) (c:case): case =
  (* si la case est l'origine pas de sense la tourner *)
  if c = (0, 0, 0) then c else 
    let i, j, k = c
    and m = m mod 6 in (* pas de sense tourner plusieur fois*)
      match m with
      | 0 -> i, j, k
      | m -> tourner_case (m - 1) (-k, -i, -j)
;;


(**
  Calcule la case par translation de vecteur [v] à partir de [c].
*)
let translate (c:case) (v:vecteur): case =
  let c1, c2, c3 = c
  and v1, v2, v3 = v in
    c1 + v1, c2 + v2, c3 + v3 
;;


(**
  La différence entre les coordonnées des cases [c1] et [c2] est le vecteur de 
  translation de [c2] vers [c1].
*)
let diff_case (c1:case) (c2:case): vecteur =
  let i1, j1, k1 = c1
  and i2, j2, k2 = c2 in 
    i1 - i2, j1 - j2, k1 - k2
;;


(**
  Vérifie si les cases [c1] et [c2] sont alignées.
*)
let sont_cases_alignee (c1:case) (c2:case): bool =
  let i1, j1, k1 = c1
  and i2, j2, k2 = c2 in
    match c1, c2 with
    | _ when i1 = i2 -> true 
    | _ when j1 = j2 -> true
    | _ when k1 = k2 -> true
    | _ -> false
;;


(** 
  Triplet d'entiers de distances entre les coordonnées [(i1, j1, k1)] et 
  [(i2, j2, k2)] des cases [c1] et [c2].
*)
let dist_entre_coordonnees (c1:case) (c2:case): int * int * int =
  let i1, j1, k1 = c1
  and i2, j2, k2 = c2
    in let di = abs (i1 - i2) (* distance entre les coordonnées i *)
       and dj = abs (j1 - j2) (* distance entre les coordonnées j *)
       and dk = abs (k1 - k2) (* distance entre les coordonnées k *)
         in di, dj, dk
;;


(**
  Distance maximale entre les coordonnées des cases [c1] et [c2].
*)
let max_dist_cases (c1:case) (c2:case): int =
    let di, dj, dk = dist_entre_coordonnees c1 c2 in max di (max dj dk) ;;
;;


(**
  Distance minimale entre les coordonnées des cases [c1] et [c2].
*)
let min_dist_cases (c1:case) (c2:case): int =
  let di, dj, dk = dist_entre_coordonnees c1 c2 in min di (min dj dk) ;;
;;


(**
  Nombre de cases entres les cases [c1] et [c2].
*)
let compte_cases (c1:case) (c2:case): int = 
  if sont_cases_alignee c1 c2 
  then max_dist_cases c1 c2 - 1 (* si les cases sont alignées *)
  else min_dist_cases c1 c2 - 1 (* sinon *)
;;


(**
  Vérifie si les cases [c1] et [c2] sont voisines, c'est à dire que la distance
  entre leurs coordonnées et égal à 1 et ils sont alignées.
*)
let sont_cases_voisines (c1:case) (c2:case): bool =
  if sont_cases_alignee c1 c2 then max_dist_cases c1 c2 = 1 else false
;;


(**
  Calcule le mi-chemin (pivot) entre les cases [c1] et [c2], c'est à dire que
  les cases sont alignées et le nombre de cases entres les deux est impair.
*)
let calcul_pivot (c1:case) (c2:case): case option =
  let est_impair = (compte_cases c1 c2) mod 2 = 1 
  and sont_alignees = sont_cases_alignee c1 c2 in
    let i, j, k = translate c1 c2 in
      if est_impair && sont_alignees then Some(i/2, j/2, k/2) else None
;;


(**
  Supposons que [c1] et [c2] sont des cases alignées, alors il existe un 
  vecteur [v] tels qu'il correspond au vecteur de translation d'un déplacement
  unitaire et que si on applique la distance [d] entre les cases, [d] fois 
  cette translation en [c1] on arrive dans la case [c2]. Si ce vecteur n'existe
  pas on renvoie un vecteur nuls et une distance négatif.
*)
let vec_et_dist (c1:case) (c2:case): vecteur * int =
  if c1 = (0,0,0) && c2 = (0,0,0) then (0,0,0), -1 
  else 
    let d = max_dist_cases c1 c2
    and i, j, k = diff_case c1 c2 in 
    let v = i / d * (-1), j / d * (-1), k / d * (-1) in
      if est_case v then v, d else (0, 0, 0), -1 
;;


(* AFFICHAGE (fonctionne si les fonctions au dessus sont remplies) *)
(* transfo transforme des coordonnees cartesiennes (x,y) en coordonnees de case
  (i, j, k) *)
let associe (a:'a) (l:('a*'b) list) (defaut:'b):'b = 
    defaut
;;

let transfo x y = (y, (x-y)/2,(-x-y)/2);;


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


let rec affiche_ligne (n:int) (m:int) (config:configuration) : string =
  let (lcc,_,dim)=config in
    if m = (4 * dim) + 1 then " " (* fin de ligne *)
    else
      let c = transfo m n in
      if not ((n+m) mod 2 = 0) || not (est_dans_etoile c dim) then (*ceci est 
        une inter-case (case inutile d'un damier) ou hors de l'etoile *)
        "   "^ affiche_ligne n (m + 1) config
      else (*ceci est une case ou bien en dehors du plateau*)
       (couleur2string (associe c lcc Libre)) ^ affiche_ligne n (m + 1) config
;;


let affiche (config:configuration):unit =
  let (_,_,dim)=config in
    let rec affiche_aux n =
      if n = - 2 * dim - 1 then ()
      else
      begin
      print_endline (affiche_ligne n (-4*dim-1) config);
      print_endline "\n";
      affiche_aux (n - 1)
      end
    in
    affiche_aux (2*dim+1);;


let conf_1=([((0,0,0),Jaune)],[Jaune],2);;
affiche conf_1;;

let conf_reggae=(
  [((0,-1,1),Vert);((0,0,0),Jaune);((0,1,-1),Rouge)],[Vert;Jaune;Rouge],1
  );;
affiche conf_reggae;;
let conf_vide=([],[],2);;
affiche conf_vide;;

let conf_vide=([],[],1);;
affiche conf_vide;;

(*
  A essayer apres avoir fait remplir_init
  affiche (remplir_init [Code "Ali";Code "Bob";Code "Jim"] 3);;
*)