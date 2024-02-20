#!/bin/sh

# ocaml commands
if [ "$1" = "" ] 
then 
    echo "#use \"projet.ml\" ;;" | ocaml
else
    echo "#use \"rendu_etd.ml\" ;;" | ocaml
fi