include CaseString


(**
  Vérifie si la coordonnée [x] et valide dans la dimension [dim].
*)
let indice_valide (x:int) (dim:dimension): bool =
  -2 * dim <= x && x <= 2 * dim
;;

(**
  Vérifie si [c] est une case.
*)
let est_case (c:case): bool = 
  let i, j, k = c in i + j + k = 0 
;;

(**
  Vérifie si [c] est une case dans le losange North-South du plateau de 
  dimension [dim].
*)
let est_dans_losange (c:case) (dim:dimension): bool = 
  let _, j, k = c in
    -dim <= j && j <= dim && 
    -dim <= k && k <= dim
;;     

(**
  Vérifie si la case [c] est dans le losange Northwest-Southeast du plateau de 
  dimension [dim].
*)
let est_dans_losange_2 (c:case) (dim:dimension): bool = 
  let i, _, k = c in
    -dim <= i && i <= dim && 
    -dim <= k && k <= dim
;; 


(**
  Vérifie si la case [c] est dans le losange Northeast-Southwest du plateau de
  dimension [dim].
*)
let est_dans_losange_3 (c:case) (dim:dimension): bool = 
  let i, j, _ = c in
    -dim <= i && i <= dim && 
    -dim <= j && j <= dim
;; 

(**
  Vérifie si la case [c] est dans l'étoile de dimension [dim].
*)
let est_dans_etoile (c:case) (dim:dimension): bool =
  est_dans_losange c dim ||    (* l'union de trois losange est un étoile *)
  est_dans_losange_2 c dim || 
  est_dans_losange_3 c dim
;;

(**
  La case [c] est la case après avoir fait tourner le plateau de [m] sixième de
  tour dans le sens anti-horaire.
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
  Calcule la case par translation de vecteur [v] à partir de [c].
*)
let translate (c:case) (v:vecteur): case =
  let c1, c2, c3 = c    (* les coordonnées de la case c *)
  and v1, v2, v3 = v in (* les coordonnées du vectuer v *)
    v1 + c1, v2 + c2, v3 + c3 (* translation des coordonnées de v vers c *)
;;


(**
  La différence entre les des cases [c1] et [c2] est le vecteur de translation 
  de [c2] vers [c1].
*)
let diff_case (c1:case) (c2:case): vecteur =
  let i1, j1, k1 = c1    
  and i2, j2, k2 = c2 in 
    i1 - i2, j1 - j2, k1 - k2 (* la différence entre les coordonnées c1 et c2 *)
;;


(**
  Vérifie si les cases [c1] et [c2] sont alignées.
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
  Un triplet de distances entre les coordonnées des cases [c1] et [c2].
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
  Distance maximale entre les coordonnées des cases [c1] et [c2].
*)
let max_dist_cases (c1:case) (c2:case): int =
  let di, dj, dk = dist_coords c1 c2 in max di (max dj dk) 
;;



(**
  Distance minimale entre les coordonnées des cases [c1] et [c2].
*)
let min_dist_cases (c1:case) (c2:case): int =
  let di, dj, dk = dist_coords c1 c2 in min di (min dj dk)
;;


(**
  Nombre de cases entres les cases [c1] et [c2]. Pour determiner ce nombre on
  doit prendre la distance maximale si ils sont alignées, sinon la distance
  minimale.
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
  Vérifie si les cases [c1] et [c2] sont voisines.
*)
let sont_cases_voisines (c1:case) (c2:case): bool =
  (* si les cases sont alignées et la distances entre eux est 1 *)
  sont_cases_alignee c1 c2 && max_dist_cases c1 c2 = 1
;;


(**
  Calcul le pivot entre les cases [c1] et [c2] s'il existe, sinon [None].
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
