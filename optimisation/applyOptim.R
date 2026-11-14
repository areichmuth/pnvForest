#!/usr/bin/env Rscrip


##change path to where the code is located
source("~/Workspace/pnvComp/sourceFunctions.R")
suppressPackageStartupMessages(library(DEoptim))

###### change path ##########
path = "/path/to/in_and_output/"
## and please create following directory within: tif/

# polygon available under Federal Agency for Nature Conservation (BfN), see readme 
poly = "name/of/PNV_vector_data.shp"

classes = read.csv('formations.csv')
#############################


rcp = c('45', '85')
perc = c('50')
period = c('1971_1990', '2079_2098')
type = c('weighting', 'cutoff')

projText="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
shpPnv = poly
formation = as.data.frame(cbind(classes$F2.Formation, classes$Dominant.Tree.Species, classes$Side.Tree.species))
primSpecList = extractSpecList(formation)
sideSpecList = extractSpecList(formation, 'side')

paramList=expand.grid(period, rcp, perc, type, stringsAsFactors = FALSE)
paramList=setDT(paramList)[Var1 == "1971_1990", c('Var2','Var3'):="0"]
paramList = distinct(paramList, Var1, Var2, Var3, Var4, .keep_all= TRUE)
print(paramList)
#make nested list for calling in parallel computing
iterate_list <- split(paramList, seq(nrow(paramList)))

purrr::walk(.x=1:nrow(paramList), .f=applyOptimDE)