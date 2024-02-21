type dimension = int ;; (* restreint aux entiers strictement positifs *)

type case    = int * int * int ;; (* restreint au triplet tels (i, j, k) tels que i + j + k = 0 *)
type vecteur = int * int * int ;; (* restreint au triplet tels (i, j, k) tels que i + j + k = 0 *)

type couleur = Vert | Jaune | Rouge | Noir | Bleu | Marron (* Les couleurs des joueurs *)
               | Libre 
               | Code of string (* une chaine restreinte a 3 caracteres *) ;;


type case_coloree  = case * couleur ;;

type configuration = case_coloree list * couleur list * dimension ;; (* sans case libre *)
          
type coup = Du of case * case | Sm of case list ;;


(* Initisalisation de generation de entier aleatoire *)
Random.self_init () ;;