(* Implimentation de types differents *)
#use "src/types/types.ml" ;;

(* 
  |-------------------------|
  | Fonctions de type "int" |
  |-------------------------|
*)

(* Renvoie le maximum entre deux entiers *)
let max2 (a:int) (b:int): int =
  if a > b then a else b 
;;

(* Renvoie le maximum entre trois entiers *)
let max3 (a:int) (b:int) (c:int) =
  max2 a (max2 b c)
;;

(* Renvoie un entier aleatoire entre a et b inclus *)
let random_int (a:int) (b:int): int = (* a >= 0 and b >= 0 *)
  a + Random.int (b - a + 1)
;;

(* 
  |--------------------------|
  | Fonctions de type "bool" |
  |--------------------------|
*)

(* Verifie si les coordonnees i, j et k du c est une case *)
let est_case (c:case): bool =
  let i, j, k = c in (i + j + k) = 0
;;

let est_dans_losange ((i,j,k):case) (dim:dimension): bool = 
  -dim <= j && j <= dim && -dim <= k && k <= dim 
;;          

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

let case_identique ((c11,c12,c13):case) ((c21,c22,c23):case): bool =
  c11 == c21 && c12 == c22 && c13 == c23 
;;

let case_different (c1:case) (c2:case): bool =
  not (case_identique c1 c2)
;;


(* La fonction renvoie la distances entres les cases alignees *)
let dist_entre_case (c1:case) (c2:case): int =
  let i1, j1, k1 = c1
  and i2, j2, k2 = c2
    in let di = abs (i1 - i2)
       and dj = abs (j1 - j2)
       and dk = abs (k1 - k2)
         in max3 di dj dk
;;

(* 
  |--------------------------|
  | Fonctions de type "case" |
  |--------------------------|
*)

(* Renvoie une case aleatoire de dimension dim *)
let rec random_case (dim:dimension): case =
  let a = -2 * dim
  and b =  2 * dim in
    let c = random_int a b, random_int a b, random_int a b in
      match c with
      | c when est_case c && est_dans_etoile c dim -> c
      | _ -> random_case (dim)
;;

let rec tourner_case (m:int) (c:case): case =
  let i, j, k = c
  and mm = m mod 6 in
    match mm with
    | 0 -> i, j, k
    | m -> tourner_case (m - 1) (-k, -i, -j)
;;

let translate ((c1,c2,c3):case) ((v1,v2,v3):vecteur): case =
  c1 + v1, c2 + v2, c3 + v3 
;;

let diff_case ((c11,c12,c13):case) ((c21,c22,c23):case): vecteur =
  c11 - c21, c12 - c22, c13 - c23 
;;

let sont_cases_voisines (c1:case) (c2:case): bool =
  let c = diff_case c1 c2 in
      match c with
      |  0,  1, -1 
      |  1,  0, -1 
      |  0, -1,  1 
      | -1,  0,  1 
      |  1, -1,  0
      | -1,  1,  0 -> true
      |          _ -> false 
;;

let calcul_pivot (c1:case) (c2:case): case option =
  let (i,j,k) = translate c1 c2 in 
    let p = i / 2, j / 2, k / 2 in
      match c1, c2 with
      | c1, c2 when sont_cases_voisines c1 p && sont_cases_voisines c2 p -> Some(p)
      | _ -> None
;;

let vec_et_dist (c1:case) (c2:case): vecteur * int =
  let d = dist_entre_case c1 c2
    in let i, j, k = diff_case c1 c2
    in let v = i / d * (-1), j / d * (-1), k / d * (-1)
         in v, d  
;;

let mult_case_entier ((i,j,k):case) (n:int): case =
  i * n, j * n, k * n
;;