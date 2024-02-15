Sys.command "clear" ;;


(* Definitions de types de jeu "Les dames chinoises" *)

(* 
  Le type "dimension" d'un plateau, note dim par la suite, est un parametre qui 
  encode la taille du plateau. Le plateau a 4*dim+1 lignes horizonteles que 
  numerotons de bas en haut de -2*dim à 2*dim et similairement pour les lignes 
  obliques. 
*)
type dimension = int ;; 
(* restreint aux entiers strictement positifs *)


(*
  Le type "case" est definie par trois coordoonnees (i,j,k), la case au centre du 
  plateau de jeu a pour coordonnees (0,0,0). Les coordonnees representent :
    i) le numero de la ligne horizontale
    j) le numero de la ligne horizontale lorsqu'on a tourner le plateau d'un tiers
       de tour dans le sens anti-horaire
    k) le numero de la ligne horizontale lorsqu'on a tourner le plateau d'un tiers
       de tour dans le sens horaire
*)
type case = int * int * int ;; 
(* restreint au triplet tels (i,j,k) tels que i + j + k = 0*)


(* 
  Le type "couleur" represente les couleurs des joueurs, ou le constructeurs Code
  vous permet d'entrer vos propres noms de joueur. La couleur Libre est une
  couleur en plus pour coder l'absence de joueur.
*)
type couleur = Vert | Jaune | Rouge | Noir | Bleu | Marron
| Code of string (* une chaine restreinte a 3 caracteres *)
| Libre ;; (* represente l'absence de joueur *)


(*
  Le type "case_coloree" represente une case coloree  
*)
type case_coloree = case * couleur ;;


(*
  Le type "configuration" c'est une configuration de jeu represtente les
  proprietes suivantes :
    1) La liste de case colorees donne l'emplacement des pions et leurs couleurs;
    2) La liste de couleur c'est une liste de joueur qui permet de savoir a qui
       est le tour (tete de liste) et quel sera le tour des suivants (en suivant
       l'ordre de la liste). Sans case libre.
    3) La dimension du jeu.
*)
type configuraion = case_coloree list * couleur list * dimension ;;


(*
  Le type "coup" represente de sortes de coups:
    1) Les deplacements unitaires (constructeur Du)
    2) Les sauts multiples (constructeur Sm)
*)
type coup = Du of case * case | Sm of case list ;;


(* Les fonctions de tests du jeu "Les dames chinoises" *)

(* 
  La fonction "(indice_valide x dim)" verifie si l'indice du coordonnee x dans la 
  dimension dim est valide.
*)                                                  (* Reponse du systeme *)
let indice_valide (x:int) (dim:dimension): bool =   (* val indice_valide : int -> dimension -> bool = <fun> *)
  x >= -2 * dim && x <= 2 * dim ;;



(*
  La fonction "est_case" verifie si la case (i,j,k) satisfait le type "case".
*)                                  (* Reponse du systeme *)
let est_case (i,j,k:case): bool =   (* val est_case : case -> bool = <fun> *)
  i + j + k = 0 ;;


(* Question 1. Tests de differents conditions de cases *)

(* Expression *)                         (* Reponse du systeme *)
let dim : dimension = 1 ;;                  (* val dim : dimension = 1 *)

(* i < -dim *)
let case_1 : case = (-dim - 1, 1, 1) ;;     (* val case_1 : case = (-2, 1, 1) *)
assert (est_case case_1) ;;                 (* - : unit = () *)  

(* i > dim *)
let case_2 : case = (2, -1, -1) ;;          (* val case_2 : case = (2, -1, -1) *)
assert (est_case case_2) ;;                 (* - : unit = () *)   

(* j < -dim *)
let case_3 : case = (1, -dim - 1, 1) ;;     (* val case_3 : case = (1, -2, 1) *)
assert (est_case case_3) ;;                 (* - : unit = () *)   

(* (i,j,k) = (2dim, -dim, -dim) *)
let case_4 : case = (2*dim, -dim, -dim) ;;  (* val case_4 : case = (2, -1, -1) *)
assert (est_case case_4) ;;                 (* - : unit = () *) 

(* (i,j,k) = (-dim - 1, 1, dim) *)
let case_5 : case = (-dim - 1, 1, dim) ;;   (* val case_5 : case = (-2, 1, 1) *)
assert (est_case case_5) ;;                 (* - : unit = () *)   

(* i >= -dim && j >= -dim && k >= -dim *)
let case_6 : case = (0, 0, 0) ;;            (* val case_6 : case = (0, 0, 0) *)
assert (est_case case_6) ;;                 (* - : unit = () *) 


(* Question 2
  La fonction "est_dans_losange" verifie si la case est dans
  losange Nord-Sud.   
*)                                                      (* Reponse du systeme *)
let est_dans_losange (c:case) (dim:dimension): bool =   (* val est_dans_losange : case -> dimension -> bool = <fun> *)
  let i, j, k = c in
  j >= -dim && j <= dim && k >= -dim && k <= dim ;;


(* Question 3
  La fonction "est_dans_etoile" verifie si la case est dans
  l'etoile.
*)
let est_dans_centre ((i,j,k):case) (dim:dimension): bool =
  abs i <= dim && abs j <= dim && abs k <= dim ;;

let est_dans_triangle_1 ((i,j,k):case) (dim:dimension): bool =
   dim <= i && i <= dim*2 && 
  -dim <= j && j <= 0     && 
  -dim <= k && k <= 0     ;;

let est_dans_triangle_2 ((i,j,k):case) (dim:dimension): bool =
  true ;;

let est_dans_triangle_3 ((i,j,k):case) (dim:dimension): bool =
  true ;;

let est_dans_triangle_4 ((i,j,k):case) (dim:dimension): bool =
  true ;;

let est_dans_triangle_5 ((i,j,k):case) (dim:dimension): bool =
  true ;;

let est_dans_triangle_6 ((i,j,k):case) (dim:dimension): bool =
  true ;;

let est_dans_etoile (c:case) (dim:dimension): bool =
  (est_dans_centre c dim) || 
  (est_dans_triangle_1 c dim) ;;


(* Question 4 
  La fonction "tourner_case" est la case c après avoir fait tourner le plateau m de
  sixième de tour dans le sens  anti-horaira.
*)
let rec tourner_case (m:int) (c:case): case =
    match m,c with
    | _,_ -> (0,0,0) ;;