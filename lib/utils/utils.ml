include Types
include Case
include Math
include StringUtils
include Output

let mult_case_entier ((i,j,k):case) (n:int): case =
  i * n, j * n, k * n
;;

let indice_valide (x:int) (dim:dimension): bool =
  x >= -2 * dim && x <= 2 * dim
;;

let [@warning "-27"] associe (a:'a) (l:('a * 'b) list) (defaut:'b): 'b = 
  defaut
;;

(** AFFICHAGE (fonctionne si les fonctions au dessus sont remplies). 
    transfo transforme des coordonnees cartesiennes (x,y) en coordonnees de case (i, j, k) *)
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
  let (lcc, _, dim) = config in
    if m = (4 * dim) + 1 then " " (* fin de ligne *)
    else
      let c = transfo m n in
      if not ((n + m) mod 2 = 0) || not (est_dans_etoile c dim) then (*ceci est une inter-case (case inutile d'un damier) ou hors de l'etoile*)
        "   " ^ affiche_ligne n (m + 1) config
      else (*ceci est une case ou bien en dehors du plateau*)
       (couleur2string (associe c lcc Libre)) ^ affiche_ligne n (m + 1) config;;


let affiche (config:configuration):unit =
  let (_,_,dim) = config in
    let rec affiche_aux n =
      if n = - 2 * dim - 1 then () else
      begin
        print_endline (affiche_ligne n (-4 * dim - 1) config);
        print_endline "\n";
        affiche_aux (n - 1)
      end
    in
    affiche_aux (2*dim+1);;

(*
  A essayer apres avoir fait remplir_init
  affiche (remplir_init [Code "Ali";Code "Bob";Code "Jim"] 3);;
*)