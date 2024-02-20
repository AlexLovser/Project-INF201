#!/bin/sh

# ocaml commands
if [ "$1" = "test" ] 
then 
    echo "#use \"test.ml\" ;;" | ocaml
else
    echo "#use \"rendu_etd.ml\" ;;" | ocaml
fi