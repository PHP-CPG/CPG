#!/bin/bash
if [ $# -lt 2 ]
  then
    echo "./run.sh <project> <outfolder>"
    exit 1
fi

project=$1
outfolder=$2
version=$3
outname=${4:-"out.cpg"}
rem=$#
#remaining ${@:3}
docker run -v ${project}:/project/ \
  -v ${outfolder}:/out/ \
   multilayer-cpg-php
