#!/bin/bash

form="$1" #takes it from orchestrateOptim.sh

## change the paths to where you store the code

#Rscript --vanilla prepare/newMax.R #1
#Rscript --vanilla prepare/prepareData.R #2
#Rscript --vanilla optimisation/optim.R "$form" #3 #call here instead orchestrateOptim.sh
#Rscript --vanilla optimisation/applyOptim.R #4
#Rscript --vanilla analyse/comparePnv.R #5 
#Rscript --vanilla analyse/evalPnvShp.R #6 
#Rscript --vanilla analyse/sumDiff.R #7

