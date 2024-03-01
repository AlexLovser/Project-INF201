include Types
include Math


(** 
  Vérifie si [c] est une case, par [i + j + k = 0].
*)
let est_case (c:case): bool =
  let i, j, k = c in 
    i + j + k = 0
;;


(**
  Vérifie si les coordonnées des cases [c1] et [c2] sont égals.
*)
let case_identique (c1:case) (c2:case): bool =
  let i1, j1, k1 = c1
  and i2, j2, k2 = c2 in
    i1 == i2 && j1 == j2 && k1 == k2 
;;


(**
  Vérifie si les coordonnées des cases [c1] et [c2] sont différentes.
*)
let case_different (c1:case) (c2:case): bool =
  not (case_identique c1 c2)
;;


(**
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


(**
  Vérifie si la case [c] est dans l'étoile du plateau de dimension [dim].
*)
let est_dans_etoile (c:case) (dim:dimension): bool =
  est_dans_losange c dim ||
  est_dans_losange_2 c dim ||
  est_dans_losange_3 c dim
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
         in max3 di dj dk
;;


(** 
  La case [c] est la case après avoir fait tourner le plateau de [m] sixième de
  tour dans le sens anti-horaire. Au debut on calcule [m mod 6],
  car chaque foit quand on tourne le sixieme foit on renvien a la position
  initiale, puis on effectue une transformation de case [c] avec les coordonnées
  [(i, j, k)] vers [(-k, -i, -j)] récursivement [m] fois.
*)
let rec tourner_case (m:int) (c:case): case =
  let i, j, k = c
  and m = m mod 6 in
    match m with
    | 0 -> i, j, k
    | m -> tourner_case (m - 1) (-k, -i, -j)
;;


(** 
  Calcule la case obtenue par translation de vecteur [v] à partir de [c], c'est
  à dire [translate v c = (c1 + v1, c2 + v2, c3 + v3)] en notant [(c1, c2, c3) 
  = c] et [(v1, v2, v3) = v]. 
*)
let translate (c:case) (v:vecteur): case =
  let c1, c2, c3 = c
  and v1, v2, v3 = v in
    c1 + v1, c2 + v2, c3 + v3 
;;


(** 
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
  Supposons que [c1] et [c2] sont des cases alignées, alors il sont voisines si et
  seulement si la distance [d] entre les deux est égale à 1.
*)
let sont_cases_voisines (c1:case) (c2:case): bool =
  let d = dist_entre_cases c1 c2 in
    match d with
    | d when d == 1 -> true
    | _ -> false 
;;


(** 
  Supposons que [c1] et [c2] sont des cases alignées, alors il existe une case 
  [p] tels que c'est un semi-chemin entre les deux, c'est à dire que [p] est une
  voisines entre les deux cases.
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
  Calcule le modulo entre les coordonnées de la case [c] et du vecteur [v]. Si un
  des coordonnées du vecteur [v] est nul, alors c'est nul pour cette coordonnée.
*)
let mod_case (c:case) (v:vecteur): vecteur =
  let c1, c2, c3 = c
  and v1, v2, v3 = v in
    let i = if v1 != 0 then c1 mod v1 else 0
    and j = if v2 != 0 then c2 mod v2 else 0
    and k = if v3 != 0 then c3 mod v3 else 0 in
      i, j, k
;;

(**
  Supposons que [c1] et [c2] sont des cases alignées, alors il existe un vecteur
  [v] tels qu'il correspond au vecteur de translation d'un déplacement unitaire
  et que si on applique la distance [d] entre les cases, [d] fois cette translation
  en [c1] on arrive dans la case [c2]. Si ce vecteur n'existe pas on renvoie
  un vecteur nul et une distance négatif.
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


(* FONCTION EXPERIMENTALES !!! *)


(** Renvoie une case aleatoire de dimension dim *)
let random_case (dim:dimension): case =
  let rec random (a:int) (b:int): case =
    let i = random_int a b 
    and j = random_int a b
    and k = random_int a b in
      let c = i, j, k in
        match c with
        | c when est_case c && est_dans_etoile c dim -> c
        | _ -> random a b in random (-2 * dim) (2 * dim)
;;