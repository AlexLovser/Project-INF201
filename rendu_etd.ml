
(* 
  i -
  j / 
  k \   
*)

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
  j >= -dim && j <= dim && k >= -dim && k <= dim 
;;          

(* A MODIFIER en Q3 *)
let est_dans_centre ((i,j,k):case) (dim:dimension): bool =
  1 * -dim <= j && j <= 1 * +dim &&
  1 * -dim <= i && i <= 1 * +dim &&
  1 * -dim <= k && k <= 1 * +dim 
;;
  
let est_dant_tour_1 ((i,j,k):case) (dim:dimension): bool =
  1 * -dim <= j && j <  0 * +dim &&
  1 * +dim <  i && i <= 2 * +dim &&
  1 * -dim <= k && k <  0 * +dim 
;;

let est_dant_tour_2 ((i,j,k):case) (dim:dimension): bool =
  0 * +dim <= j && j <= 1 * +dim &&
  0 * +dim <= i && i <= 1 * +dim &&
  2 * -dim <= k && k <  1 * -dim 
;;

let est_dant_tour_3 ((i,j,k):case) (dim:dimension): bool =
  1 * +dim <  j && j <= 6 * +dim &&
  1 * -dim <= i && i <= 0 * +dim &&
  1 * -dim <= k && k <= 0 * +dim 
;;

let est_dant_tour_4 ((i,j,k):case) (dim:dimension): bool =
  0 * +dim <  j && j <= 1 * +dim &&
  2 * -dim <= i && i <  1 * -dim &&
  0 * +dim <  k && k <= 1 * +dim 
;;

let est_dant_tour_5 ((i,j,k):case) (dim:dimension): bool =
  1 * -dim <= j && j <  0 * +dim &&
  1 * -dim <= i && i <  0 * +dim &&
  1 * +dim <  k && k <= 2 * +dim 
;;

let est_dant_tour_6 ((i,j,k):case) (dim:dimension): bool =
  2 * -dim <= j && j <  1 * -dim &&
  0 * +dim <  i && i <= 1 * +dim &&
  0 * +dim <  k && k <= 1 * +dim 
;;

let est_dans_etoile (c:case) (dim:dimension): bool =
  est_dans_centre c dim ||
  est_dant_tour_1 c dim ||
  est_dant_tour_2 c dim ||
  est_dant_tour_3 c dim ||
  est_dant_tour_4 c dim ||
  est_dant_tour_5 c dim ||
  est_dant_tour_6 c dim
;;


(* QUESTION 4 *)
let [@warning "-8"] rec tourner_case (m:int) ((i,j,k):case): case =
    match m with
    | 0 -> (-k,-i,-j)
    | m -> tourner_case (m - 1) (i,j,k)
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

(* let dim : dimension = 3 ;;
let case_tour_1 : case = (2, -1,-1) ;;
let case_tour_2 : case = ( 3,-6, 3) ;;
let case_tour_3 : case = (-3,-3, 6) ;;
let case_tour_4 : case = (-6, 3, 3) ;;
let case_tour_5 : case = ( 3, 3,-6) ;;
let case_tour_6 : case = ( 3,-6, 3) ;;

tourner_case 1 case_tour_1 dim ;;
tourner_case 1 case_tour_2 dim ;;
tourner_case 1 case_tour_3 dim ;;
tourner_case 1 case_tour_4 dim ;;
tourner_case 1 case_tour_5 dim ;;
tourner_case 1 case_tour_6 dim ;; *)