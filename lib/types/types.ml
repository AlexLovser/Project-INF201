(** 
  La [dimension] d'un plateau, noté [dim] par la suite, est un paramètre qui
  encode la taille du plateau. Le plateau a [4 * dim + 1] lignes horizontales
  que nous numérotons de bas en haut de [-2 * dim] à [2 * dim] et 
  similairement pour les lignes obliques.
*)
type dimension = int ;;


(** 
  Une [case] est définie par trois coordonnées [(i, j, k)], la case au centre
  du plateau de jeu a pour coordonnées [(0, 0, 0)]. Les coordonnées
  représentent:

  - i le numéro de la ligne horizontale ;
  - j le numéro de la ligne horizontale lorsqu'on a tourné le plateau d'un 
    tiers de tour dans le sens anti-horaire ;
  - k le numéro de la ligne horizontale lorsqu'on a tourné le plateau d'un
    tiers de tour dans le sens horaire.
*)
type case = int * int * int ;;


(** 
  Le type [couleur] représente les couleurs des joueurs. Le constructeur
  [Code] vous permet d'entrer vos propes noms de joueur restreint à trois
  caractères. La couleur [Libre] est une couleur en plus pour coder
  l'absence de joueur (dans une case ou pour le gagnant d'une partie).
*)
type couleur = Vert | Jaune | Rouge | Noir | Bleu | Marron
               | Libre 
               | Code of string  
;;


(**
  Le fait qu'un pion d'une couleur [col] se situe sur une case [c] est codé
  par un couple [(c, col)] que l'on appelle une case colorée.
*)
type case_coloree  = case * couleur ;;


(**
  Le [configuration] du jeu est donnée par un triplet formé d'une liste de
  cases colorées, une liste de joueurs et une dimension (un entier). La 
  liste decases colorées donne l'emplacement des pions et leurs couleurs. On
  veillera à ce que pour chaque case [c] il y ait au plus un pion sur cette
  case, c'est-à-dire il y a au plus une couleur [col] tel que le couple [(c, 
  col)] est dans la liste; l'absence de pion sur la case [c] sera codé par
  l'absence de couple [(c, col)] dans la liste et non pas avec [(c, Libre)].
  La liste de joueur permet de savoir à qui est le tour (tête de liste) et
  quel sera le tour des suivants (en suivant l'ordre de la liste). Enfin
  même si elle ne change pas au cours de la partie la [dimension] est donnée
  dans la configuration car nous devons pouvoir accéder facilement à celle-ci
  et pouvoir en changer si nous souhaitons faire une partie sur un plateau
  de taille différente.
*)
type configuration = case_coloree list * couleur list * dimension ;;


(**
  Les coups seront décrits plus tard. Il en existe de deux sortes:
  - les déplacements unitaires (constructeur [Du])
  - les sauts multiples (constructeur [Sm])
*)
type coup = Du of case * case | Sm of case list ;;


(**
  Nous définissons le type [vecteur] comme synonyme du type [case] car les
  triplets d'entiers [(i, j, k)] tels, que [i + j + k = 0] servent aussi
  bien comme case de la grille que comme vecteur permettant des translation.
*)
type vecteur = case ;; 



(* TYPES EXPERIMENTALES !!! *)
