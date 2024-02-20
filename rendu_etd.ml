
type dimension = int ;; (* restreint aux entiers strictement positifs *)

type case    = int * int * int ;; (* restreint au triplet tels (i, j, k) tels que i + j + k = 0 *)
type vecteur = int * int * int ;; (* restreint au triplet tels (i, j, k) tels que i + j + k = 0 *)

type couleur = Vert | Jaune | Rouge | Noir | Bleu | Marron (* Les couleurs des joueurs *)
               | Libre 
               | Code of string (* une chaine restreinte a 3 caracteres *) ;;


type case_coloree  = case * couleur ;;

type configuration = case_coloree list * couleur list * dimension ;; (* sans case libre *)
          
type coup = Du of case * case | Sm of case list ;;

let indice_valide (x:int) (dim:dimension): bool =
  x >= -2 * dim && x <= 2 * dim
;;

let est_case ((i,j,k):case): bool =
  (i + j + k = 0)
;;

let associe (a:'a) (l:('a*'b) list) (defaut:'b):'b = 
  defaut
;;

(* A MODIFIER en Q2 *)
let est_dans_losange ((i,j,k):case) (dim:dimension): bool = 
  -dim <= j && j <= dim && -dim <= k && k <= dim 
;;          

let est_dans_losange_2 ((i,j,k):case) (dim:dimension): bool = 
  -dim <= i && i <= dim && -dim <= k && k <= dim 
;;   

let est_dans_losange_3 ((i,j,k):case) (dim:dimension): bool = 
  -dim <= i && i <= dim && -dim <= j && j <= dim 
;;   

(* A MODIFIER en Q3 *)
let est_dans_etoile (c:case) (dim:dimension): bool =
  est_dans_losange c dim ||
  est_dans_losange_2 c dim ||
  est_dans_losange_3 c dim
;;

(* QUESTION 4 *)
let [@warning "-8"] rec tourner_case (m:int) ((i,j,k):case): case =
  let mm = m mod 6 in
    match mm with
    | 0 -> i, j, k
    | m -> tourner_case (m - 1) (-k, -i, -j)
;;

(*Question 5*)
let translate ((c1,c2,c3):case) ((v1,v2,v3):vecteur): case =
  let i = c1 + v1
  and j = c2 + v2
  and k = c3 + v3 
    in i, j, k 
;;

(*Question 6*)
let diff_case ((c11,c12,c13):case) ((c21,c22,c23):case): vecteur =
  let v1 = c11 - c21
  and v2 = c12 - c22
  and v3 = c13 - c23 
    in v1, v2, v3
;;

(* Question 7 *)
let sont_cases_voisines (c1:case) (c2:case): bool =
  let c = diff_case c1 c2 in
      match c with
      |  0,  1, -1 
      |  1,  0, -1 
      |  0, -1,  1 
      | -1,  0,  1 
      |  1, -1,  0
      | -1,  1,  0 -> true
      |          _ -> false ;;
 
(* Question 8 *)
let calcul_pivot (c1:case) (c2:case): case option =
  None (* TODO *)
;;

(* Question 9 *)
let vec_et_dist (c1:case) (c2:case): vecteur * int =
  (0,0,0), 0 (* TODO *)
;;

(* AFFICHAGE (fonctionne si les fonctions au dessus sont remplies) *)
(* transfo transforme des coordonnees cartesiennes (x,y) en coordonnees de case (i, j, k) *)
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
      if not ((n+m) mod 2 = 0) || not (est_dans_etoile c dim) then (*ceci est une inter-case (case inutile d'un damier) ou hors de l'etoile*)
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
let conf_reggae=([((0,-1,1),Vert);((0,0,0),Jaune);((0,1,-1),Rouge)],[Vert;Jaune;Rouge],1);;
affiche conf_reggae;;
let conf_vide=([],[],2);;
affiche conf_vide;;

let conf_vide=([],[],1);;
affiche conf_vide;;

let conf_vide=([],[],3);;
affiche conf_vide;;

(*A essayer apres avoir fait remplir_init
affiche (remplir_init [Code "Ali";Code "Bob";Code "Jim"] 3);;
*)