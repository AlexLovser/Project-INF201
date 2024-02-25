(* ---------------------------------------------------------------------
   inf201_ElKortbi_Tabolskii_Bendouha_Caille_Crelerot_Q1-Q9.ml : 
   cr Q1 Q9 projet: Groupe ima4_K

   Akram Bendouha      <akram.bendouha@etu.univ-grenoble-alpes.fr 
   Aleksandr Tabolskii <aleksandr.tabolskii@etu.univ-grenoble-alpes.fr>  
   Yassin El Kortbi    <elkortby@etu.univ-grenoble-alpes.fr> 
   Daniel Caille       <daniel.caille@etu.univ-grenoble-alpes.fr> 
   Thomas Crelerot     <thomas.crelerot@etu.univ-grenoble-alpes.fr>
  ----------------------------------------------------------------------*)

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
  x >= -2 * dim && x <= 2 * dim
;;

(** Question 1

  Vérifie si [c] est une case, par [i + j + k = 0].
*)
let est_case (c:case): bool =
  let i, j, k = c in 
    i + j + k = 0
;;

let associe (a:'a) (l:('a*'b) list) (defaut:'b):'b = 
  defaut
;;

(** Question 2

  Vérifie si la case [c]  est une case dans le losange North-South d'étoile 
  du plateau de dimension [dim].
*)
let est_dans_losange (c:case) (dim:dimension): bool = 
  let _, j, k = c in
    -dim <= j && j <= dim && 
    -dim <= k && k <= dim 
;;     


(**
  Vérifie si la case [c] est dans le losange Northwest-Southeast d'étoile 
  du plateau de dimension [dim].
*)
let est_dans_losange_2 (c:case) (dim:dimension): bool = 
  let i, _, k = c in
    -dim <= i && i <= dim && 
    -dim <= k && k <= dim 
;; 


(**
  Vérifie si la case [c] est dans le losange Northeast-Southwest d'étoile
  du plateau de dimension [dim].
*)
let est_dans_losange_3 (c:case) (dim:dimension): bool = 
  let i, j, _ = c in
    -dim <= i && i <= dim && 
    -dim <= j && j <= dim 
;; 


(** Question 3

  Vérifie si la case [c] est dans l'étoile du plateau de dimension [dim].
*)
let est_dans_etoile (c:case) (dim:dimension): bool =
  est_dans_losange c dim ||
  est_dans_losange_2 c dim ||
  est_dans_losange_3 c dim
;;


(** Question 4

  La case [c] est la case après avoir fait tourner le plateau de [m] sixième de
  tour dans le sens anti-horaire. Au debut on calcule [m mod 6],
  car chaque foit quand on tourne le sixieme foit on renvien a la position
  initiale, puis on effectue une transformation de case [c] avec les 
  coordonnées [(i, j, k)] vers [(-k, -i, -j)] récursivement [m] fois.
*)
let rec tourner_case (m:int) (c:case): case =
  let i, j, k = c
  and m = m mod 6 in
    match m with
    | 0 -> i, j, k
    | m -> tourner_case (m - 1) (-k, -i, -j)
;;


(** Question 5

  Calcule la case obtenue par translation de vecteur [v] à partir de [c], c'est
  à dire [translate v c = (c1 + v1, c2 + v2, c3 + v3)] en notant [(c1, c2, c3) 
  = c] et [(v1, v2, v3) = v]. 
*)
let translate (c:case) (v:vecteur): case =
  let c1, c2, c3 = c
  and v1, v2, v3 = v in
    c1 + v1, c2 + v2, c3 + v3 
;;


(** Question 6

  Calcule la différence de chacune des coordonnées pour donner un vecteur de
  translation, c'est à dire [diff_case c1 c2 = (i1 + i2, j1 + j2, k1 + k2)] 
  en notant [(i1, j1, k1) = c1] et [(i2, j2, k2) = c2].
*)
let diff_case (c1:case) (c2:case): vecteur =
  let i1, j1, k1 = c1
  and i2, j2, k2 = c2 in
    i1 - i2, j1 - j2, k1 - k2
;;


(** 
  Supposons que les cases [c1] et [c2] sont alignées, alors entre c'est deux
  cases un des trois coordonnées [(i, j, k)] est nul, donc on doit au
  début déterminer la distance entre les coordonnées et puis renvoie
  le maximum, car a cause de la spécification de [case] si un des coordonnées
  et nul, alors les deux autres sont symétrique entre des deux. 
*)
let dist_entre_cases (c1:case) (c2:case): int =
  let i1, j1, k1 = c1
  and i2, j2, k2 = c2
    in let di = abs (i1 - i2) (* distance entre i *)
       and dj = abs (j1 - j2) (* distance entre j *)
       and dk = abs (k1 - k2) (* distance entre k *)
         in max di (max dj dk)
;;


(** Question 7

  Supposons que [c1] et [c2] sont des cases alignées, alors il sont voisines si 
  et seulement si la distance [d] entre les deux est égale à 1.
*)
let sont_cases_voisines (c1:case) (c2:case): bool =
  let d = dist_entre_cases c1 c2 in
    match d with
    | d when d == 1 -> true
    | _ -> false 
;;

 
(** Question 8

  Supposons que [c1] et [c2] sont des cases alignées, alors il existe une case 
  [p] tels que c'est un semi-chemin entre les deux, c'est à dire que [p] est 
  une voisines entre les deux cases.
  @return [Some(p)] s'il existe
  @return [None] s'il n'existe pas
*)
let calcul_pivot (c1:case) (c2:case): case option =
  let (i,j,k) = translate c1 c2 in 
    let p = i / 2, j / 2, k / 2 in
      match c1, c2 with
      | c1, c2 when sont_cases_voisines c1 p && sont_cases_voisines c2 p -> Some(p)
      | _ -> None
;;


(**
  Calcule le modulo entre les coordonnées de la case [c] et du vecteur [v]. Si 
  un des coordonnées du vecteur [v] est nul, alors c'est nul pour cette 
  coordonnée.
*)
let mod_case (c:case) (v:vecteur): vecteur =
  let c1, c2, c3 = c
  and v1, v2, v3 = v in
    let i = if v1 != 0 then c1 mod v1 else 0
    and j = if v2 != 0 then c2 mod v2 else 0
    and k = if v3 != 0 then c3 mod v3 else 0 in
      i, j, k
;;


(** Question 9

  Supposons que [c1] et [c2] sont des cases alignées, alors il existe un 
  vecteur [v] tels qu'il correspond au vecteur de translation d'un déplacement
  unitaire et que si on applique la distance [d] entre les cases, [d] fois 
  cette translation en [c1] on arrive dans la case [c2]. Si ce vecteur n'existe 
  pas on renvoie un vecteur nul et une distance négatif.
*)
let vec_et_dist (c1:case) (c2:case): vecteur * int =
  let d = dist_entre_cases c1 c2 in 
  let i, j, k = diff_case c1 c2 in 
    let v0 = (0, 0, 0)
    and v = i / d * (-1), j / d * (-1), k / d * (-1) in
    match v with
    | v when mod_case c1 v = v0 && mod_case c2 v = v0 -> v, d
    | _ -> v0, -1
;;


(* AFFICHAGE (fonctionne si les fonctions au dessus sont remplies) *)
(* transfo transforme des coordonnees cartesiennes (x,y) en coordonnees de case 
  (i, j, k) *)
let transfo x y = (y, (x-y)/2,(-x-y)/2);;


let couleur2string (coul:couleur):string =
  match coul with
  | Libre -> " . "
  | Code s -> s  
  | Vert -> " V "
  | Jaune -> " J "
  | Rouge -> " R "
  | Noir -> " N "
  | Bleu -> " B "
  | Marron -> " M ";;


let rec affiche_ligne (n:int) (m:int) (config:configuration) : string =
  let (lcc,_,dim)=config in
    if m = (4 * dim) + 1 then " " (* fin de ligne *)
    else
      let c = transfo m n in
      if not ((n+m) mod 2 = 0) || not (est_dans_etoile c dim) then (*ceci est 
        une inter-case (case inutile d'un damier) ou hors de l'etoile *)
        "   "^ affiche_ligne n (m + 1) config
      else (*ceci est une case ou bien en dehors du plateau*)
       (couleur2string (associe c lcc Libre)) ^ affiche_ligne n (m + 1) config;;


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
let conf_reggae=([((0,-1,1),Vert);((0,0,0),Jaune);((0,1,-1),Rouge)],
[Vert;Jaune;Rouge],1);;
affiche conf_reggae;;
let conf_vide=([],[],2);;
affiche conf_vide;;

let conf_vide=([],[],1);;
affiche conf_vide;;

(*
  A essayer apres avoir fait remplir_init
  affiche (remplir_init [Code "Ali";Code "Bob";Code "Jim"] 3);;
*)

(**
  Vérifie si les coordonnées des cases [c1] et [c2] sont égals.
*)
let case_identique (c1:case) (c2:case): bool =
  let i1, j1, k1 = c1
  and i2, j2, k2 = c2 in
    i1 == i2 && j1 == j2 && k1 == k2 
;;

(**
  Multiplie la case [c] par un entier [n].
*)
let mult_case_entier ((i,j,k):case) (n:int): case =
  i * n, j * n, k * n
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


(* Testing "calcul_pivot" *)
calcul_pivot ( 0,-1, 1) ( 0, 1,-1) ;;
calcul_pivot (-2, 0, 2) ( 0, 0, 0) ;;
calcul_pivot (-2, 2, 0) ( 0, 0, 0) ;;

(* Testing "vec_et_dist" *)
let v, d = vec_et_dist ( 0, 2,-2) ( 0, 0, 0) ;;
translate (mult_case_entier v d) (0, 2,-2) ;;

let v, d = vec_et_dist ( 0, 6,-6) ( 0,-6, 6) ;;
translate (mult_case_entier v d) (0, 6,-6) ;;
