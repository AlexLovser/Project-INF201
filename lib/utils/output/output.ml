(****************************************************************************)
(*                                                                          *)
(****************************************************************************)


include Types
include Case
include StringUtils


(**
  Affiche une case [c] dans la sortie standart.
*)
let print_case (c:case): unit =
  print_string (string_of_case c)
;;


(**
  Affiche la liste [l] de type char dans la sortie standart.
*)
let print_char_list (l:char list): unit =
  let s = "[" in
  let rec f (l:char list): string =
    match l with
    | [] -> ""
    | t::q -> let s = string_of_char t in "\'" ^ s ^ "\'; " ^ f q in
  let s = s ^ f l in 
  let s = sub_end s 2 ^ "]" in print_endline s
;;


(** 
  [(transfo x y)] transforme les coordonnees cartesiennes [(x, y)] en 
  coordonnees de case [(i, j, k)]
*)
let transfo (x:int) (y:int): case = 
  let i = y                (* transformation on i *)
  and j = (x - y) / 2      (* transformation on j *)
  and k = (-x - y) / 2 in  (* transformation on k *)
    i, j, k (* transformation de (x,y) en (i, j, k) *)
;;

(**
  [(associe c lcc defaut)] vaut [col] si [(c,col)] est dans la liste et 
  [defaut] sinon. Si plusieurs couple avec [c] existe le premier est utilisé.
*)
let [@warning "-27"] associe (c:case) (lcc:case_coloree list) (defaut:couleur): couleur =
  defaut
;;


(**
  Affiche la ligne de plateau du jeu de coordonnee [i].
*)
let rec affiche_ligne (n:int) (m:int) (config:configuration): string =
  (* lcc est un case_coloree et dim est un dimension *)
  let (lcc, _, dim) = config in
    if m = 4 * dim + 1 then " " (* fin de ligne *)
    else
      (* transformation de coordonnees cartesien au case *)
      let c = transfo m n in
      if not ((n + m) mod 2 = 0) || not (est_dans_etoile c dim) then 
        (* ceci est une inter-case (case inutile d'un damier) ou hors de l'etoile *)
        "   " ^ affiche_ligne n (m + 1) config
      else (*ceci est une case ou bien en dehors du plateau*)
       (string_of_couleur (associe c lcc Libre)) ^ affiche_ligne n (m + 1) config
;;


(**
  Affiche le plateau du jeu par la configuration [config].
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
      max_coord jusqu'a min_coord avec le pas.
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
        print_endline "\n";  (* le cursor dans la nouvelle ligne *)
        affiche_aux (n - pas)  (* appelle de la fonction recursive *)
      end
    
    in affiche_aux max_coord ;;