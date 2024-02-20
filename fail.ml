type case = int * int * int ;;
type dimension = int ;;

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