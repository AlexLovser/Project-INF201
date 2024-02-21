type case = int * int * int ;;
type dimension = int ;;
type vecteur = case ;;

(* Mettons ici tous les fonction inutiles  *)

let centre_segment_1 ((i,j,k):case) (dim:dimension): bool =
  1 * -dim <= j && j <= 0 * +dim &&
  0 * +dim <= i && i <= 1 * +dim &&
  1 * -dim <= k && k <= 0 * +dim 
;;

let centre_segment_2 ((i,j,k):case) (dim:dimension): bool =
  0 * +dim <= j && j <= 1 * +dim &&
  0 * +dim <= i && i <= 1 * +dim &&
  1 * -dim <= k && k <= 0 * +dim 
;;

let centre_segment_3 ((i,j,k):case) (dim:dimension): bool =
  0 * +dim <= j && j <= 1 * +dim &&
  1 * -dim <= i && i <= 0 * +dim &&
  1 * -dim <= k && k <= 0 * +dim 
;;

let centre_segment_4 ((i,j,k):case) (dim:dimension): bool =
  0 * +dim <= j && j <= 1 * +dim &&
  1 * -dim <= i && i <= 0 * +dim &&
  0 * +dim <= k && k <= 1 * +dim 
;;

let centre_segment_5 ((i,j,k):case) (dim:dimension): bool =
  1 * -dim <= j && j <= 0 * +dim &&
  1 * -dim <= i && i <= 0 * +dim &&
  0 * +dim <= k && k <= 1 * +dim 
;;

let centre_segment_6 ((i,j,k):case) (dim:dimension): bool =
  1 * -dim <= j && j <= 0 * +dim &&
  0 * +dim <= i && i <= 1 * +dim &&
  0 * +dim <= k && k <= 1 * +dim 
;;

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

(* fonction pour question 9 *)
let [@warning "-8"] ordre_croissante (a:int) (b:int) (c:int): int * int * int =
  match a, b, c with
  | a, b, c when a <= b && b <= c && a <= c -> a, b, c
  | a, b, c when a <= c && c <= b && a <= b -> a, c, b
  | a, b, c when b <= a && a <= c && b <= c -> b, a, c
  | a, b, c when b <= c && c <= a && b <= a -> b, c, a
  | a, b, c when c <= a && a <= b && c <= b -> c, a, b
  | a, b, c when c <= b && b <= a && c <= a -> c, b, a
;;

(* Question 9 *)
let vec_et_dist ((i1,j1,k1):case) ((i2,j2,k2):case): vecteur * int =
  let di = abs(i1 - i2)
  and dj = abs(j1 - j2)
  and dk = abs(k1 - k2)
    in let (d1,d2,d3) = (ordre_croissante di dj dk)
       and ( i, j, k) = (diff_case (i1,j1,k1) (i2,j2,k2))
         in match i, j, k with
            | i, j, k when i1 == i2 && j1 == j2 && k1 == k2 -> (0,0,0), 0
            | i, j, k when d1 != 0 && est_case (i/d1, j/d1, k/d1) -> (i/d1, j/d1, k/d1), d1
            | i, j, k when d2 != 0 && est_case (i/d2, j/d2, k/d2) -> (i/d2, j/d2, k/d2), d2
            | i, j, k when d3 != 0 && est_case (i/d3, j/d3, k/d3) -> (i/d3, j/d3, k/d3), d3
            | _ -> (i1,j1,k1), 1