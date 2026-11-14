#!/usr/bin/env Rscrip


##change path to where the code is located
source("~/Workspace/pnvComp/sourceFunctions.R")
suppressPackageStartupMessages(library(DEoptim))

set.seed(42)

arg = commandArgs(trailingOnly=TRUE)
#hand over number or range of numbers for formations that shall be considered - 
i = as.numeric(arg[1])

###### change path ##########
path = "/path/to/in_and_output/"
## and please create following directory within: optimisation/

# polygon available under Federal Agency for Nature Conservation (BfN), see readme 
poly = "name/of/PNV_vector_data.shp"

classes = read.csv('formations.csv')
#############################


type = 'weighting'
shpPnv = as.data.frame(poly, xy=TRUE)
formation = as.data.frame(cbind(classes$F2.Formation, classes$Dominant.Tree.Species, classes$Side.Tree.species))
primSpecList = extractSpecList(formation)
sideSpecList = extractSpecList(formation, 'side')

#call the optimisation for each formation separately
optimDE(i)

