
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

(* Question 1 *)

let est_case ((i,j,k):case): bool =
  (i + j + k = 0)
;;

let dim : dimension = 3 ;;

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

let associe (a:'a) (l:('a*'b) list) (defaut:'b):'b = 
  defaut
;;

(* Question 2 *)
let est_dans_losange ((i,j,k):case) (dim:dimension): bool = 
  -dim <= j && j <= dim && -dim <= k && k <= dim 
;;          

(* Question 3 *)
let est_dans_losange_2 ((i,j,k):case) (dim:dimension): bool = 
  -dim <= i && i <= dim && -dim <= k && k <= dim 
;;   

let est_dans_losange_3 ((i,j,k):case) (dim:dimension): bool = 
  -dim <= i && i <= dim && -dim <= j && j <= dim 
;;   

let est_dans_etoile (c:case) (dim:dimension): bool =
  est_dans_losange c dim ||
  est_dans_losange_2 c dim ||
  est_dans_losange_3 c dim
;;

(* Question 5 *)
let rec tourner_case (m:int) ((i,j,k):case): case =
  let mm = m mod 6 in
    match mm with
    | 0 -> i, j, k
    | m -> tourner_case (m - 1) (-k, -i, -j)
;;

(* Question 5 *)
let translate ((c1,c2,c3):case) ((v1,v2,v3):vecteur): case =
  c1 + v1, c2 + v2, c3 + v3 
;;

(* Question 6 *)
let diff_case ((c11,c12,c13):case) ((c21,c22,c23):case): vecteur =
  c11 - c21, c12 - c22, c13 - c23 
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
  let (i,j,k) = translate c1 c2 in 
    let p = i / 2, j / 2, k / 2 in
      match c1, c2 with
      | c1, c2 when sont_cases_voisines c1 p && sont_cases_voisines c2 p -> Some(p)
      | _ -> None
;;

(* Question 9 *)
let max2 (a:int) (b:int): int =
  if a > b then a else b 
;;


(* La fonction renvoie la distances entres les cases alignees *)
let dist_entre_case_alignees (c1:case) (c2:case): int =
  let i1, j1, k1 = c1
  and i2, j2, k2 = c2
    in let di = abs (i1 - i2)
       and dj = abs (j1 - j2)
       and dk = abs (k1 - k2)
         in max2 di (max2 dj dk)
;;

let vec_et_dist (c1:case) (c2:case): vecteur * int =
  let d = dist_entre_case_alignees c1 c2
    in let i, j, k = diff_case c1 c2
    in let v = i / d * (-1), j / d * (-1), k / d * (-1)
         in v, d  
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

