#!/bin/sh

for FILE in $*
do
    echo "#use \"$FILE\" ;;" | ocaml
done