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
