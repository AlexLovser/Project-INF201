(****************************************************************************)
(*                                                                          *)
(****************************************************************************)


(** 
  Dimension d'un plateau, note [dim] par la suite, est un parametre qui 
  encode la taille du plateau. Le plateau a [4 * dim + 1] lignes horizontales 
  que nous numerotons de bas en haut de [-2 * dim] a [2 * dim] et 
  similairement pour les lignes obliques.
*)
type dimension = int ;;


(** 
  La case est definie par trois coordonnees [(i, j, k)], la case au centre
  du plateau de jeu a pour coordonnees [(0, 0, 0)]. Les coordonnees
  representent:

  - i le numero de la ligne horizontale ;
  - j le numero de la ligne horizontale lorsqu'on a tourne le plateau d'un 
    tiers de tour dans le sens anti-horaire ;
  - k le numero de la ligne horizontale lorsqu'on a tourne le plateau d'un
    tiers de tour dans le sens horaire.
*)
type case = int * int * int ;;


(** 
  Les couleurs des joueurs. Le constructeur [Code] permet d'entrer les noms de
  joueur restreint a trois caracteres. La couleur [Libre] est une couleur en 
  plus pour coder l'absence de joueur.
*)
type couleur = 
  | Vert 
  | Jaune 
  | Rouge 
  | Noir 
  | Bleu 
  | Marron
  | Libre 
  | Code of string  
;;


(**
  Un pion d'une couleur [col] se situe sur une case [c] est code par un couple 
  [(c, col)] que l'on appelle une case coloree.
*)
type case_coloree  = case * couleur ;;


(**
  Le [configuration] du jeu est donnee par un triplet forme d'une liste de
  cases colorees, une liste de joueurs et une dimension. La liste de cases 
  colorees donne l'emplacement des pions et leurs couleurs. On veillera a ce 
  que pour chaque case [c] il y ait au plus un pion sur cette case, c'est-a-dire
  il y a au plus une couleur [col] tel que le couple [(c, col)] est dans la 
  liste; l'absence de pion sur la case [c] sera code par l'absence de couple 
  [(c, col)] dans la liste et non pas avec [(c, Libre)]. La liste de joueur 
  permet de savoir a qui est le tour (tete de liste) et quel sera le tour des 
  suivants (en suivant l'ordre de la liste). Enfin meme si elle ne change pas au
  cours de la partie la [dimension] est donnee dans la configuration car nous 
  devons pouvoir acceder facilement a celle-ci et pouvoir en changer si nous 
  souhaitons faire une partie sur un plateau de taille differente.
*)
type configuration = case_coloree list * couleur list * dimension ;;


(**
  Les coups seront decrits plus tard. Il en existe de deux sortes:
  - les deplacements unitaires (constructeur [Du])
  - les sauts multiples (constructeur [Sm])
*)
type coup = Du of case * case | Sm of case list ;;


(**
  Le type [vecteur] est synonyme du type [case] comme un vecteur permettant des 
  translation avec les meme propriet
*)
type vecteur = case ;; 