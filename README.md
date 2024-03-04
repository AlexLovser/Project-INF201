
# Les dames chinoises

![Ocaml](https://img.shields.io/endpoint?url=https://ci.ocamllabs.io/badge/mirage/ocaml-github/master&logo=ocaml)

> **Version**: `0.1.9`
> 
> **Package manager**: `Dune` 



![GitHub commit activity](https://img.shields.io/github/commit-activity/t/AlexLovser/Project-INF201?authorFilter=Elkatra2&style=for-the-badge&link=https://github.com/Elkatra2)
![GitHub commit activity](https://img.shields.io/github/commit-activity/t/AlexLovser/Project-INF201?authorFilter=AlexLovser&style=for-the-badge&link=https://github.com/AlexLovser)
![GitHub commit activity](https://img.shields.io/github/commit-activity/t/AlexLovser/Project-INF201?authorFilter=dfox235&style=for-the-badge&link=https://github.com/dfox235)
![GitHub commit activity](https://img.shields.io/github/commit-activity/t/AlexLovser/Project-INF201?authorFilter=AkrBen&style=for-the-badge&link=https://github.com/AkrBen)


*Ce ficher README est fait pour le decrire en bref les fichiers et vous aider a touver les ceux dont vous aves besoin.*


### La stucture du projet:
```bash
@ROOT
│   .gitignore
│   dune-project
│   project_inf201.opam
│   README.md
│
├───.trash
│       fail.ml
│       projet.ml
│
├───bin
│       dune
│       main.ml
│
├───lib
│   │   dune
│   │   lib.ml
│   │
│   ├───constants
│   │       constants.ml
│   │       dune
│   │
│   ├───types
│   │       dune
│   │       types.ml
│   │
│   └───utils
│       │   dune
│       │   utils.ml 
│       │
│       ├───case
│       │       case.ml
│       │       caseString.ml
│       │       dune
│       │
│       ├───listes
│       │       sequences.ml
│       │
│       ├───math
│       │       dune
│       │       math.ml
│       │
│       ├───output
│       │       dune
│       │       output.ml
│       │
│       └───string
│               dune
│               stringUtils.ml
│
├───projet_04_03_2024
│   │   dames_chinoises_etd.pdf
│   │   execProjet.sh
│   │   inf201_ElKortbi_Tabolskii_Bendouha_Caille_Crelerot_Q1_Q9.ml
│   │   lien_dans_git.ml
│   │   specification_projet_de_q1_9.odt
│   │   specification_projet_de_q1_9.pdf
│   │
│   └───gimp
│           axes_couleur.eps
│           axes_couleur.xcf
│           est_case_intersection.png
│           losange_etoile.png
│           losange_north-south.png
│           losange_northeast-southwest.png
│           losange_northwest-southeast.png
│           tourner_case.png
│
└───test
        dune
        test.ml
```

> ***Si** vous avez besoin de projet dans un seul ficher on l'ai mis ici:*<br>[/projet_04_03_2024/inf201_ElKortbi_Tabolskii_Bendouha_Caille_Crelerot_Q1_Q9.ml](/projet_04_03_2024/inf201_ElKortbi_Tabolskii_Bendouha_Caille_Crelerot_Q1_Q9.ml)


> ***Sinon**, on a bien organise tous dans le dossier*:<br>[/lib/](/lib/)
> 
> **Comment l'utiliser?**
>
> ```bash
> ./ocaml_use_projet
> ```
> **ou avec dune**
> ```bash
> dune build     # pour construire le projet
> ```
> ```bash
> dune exec main # pour executer le projet
> ```
> ```bash
> dune test      # pour effectuer les tests
> ```

> On a ecrit une bonne sepcification des fonctions dans: 
> [specification.pdf](/projet_04_03_2024/specification_projet_de_q1_9.pdf)
 ou [specification.odt](/projet_04_03_2024/specification_projet_de_q1_9.odt)


Merci de lire ce ficher, si vous avez des questions:   
- Yassin El Kortbi    `elkortby@etu.univ-grenoble-alpes.fr`
- Aleksandr Tabolskii `aleksandr.tabolskii@etu.univ-grenoble-alpes.fr`
