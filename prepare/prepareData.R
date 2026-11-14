#!/usr/bin/env Rscrip


##change path to where the code is located
source("~/Workspace/pnvComp/sourceFunctions.R")

###### change path ##########
path = "/path/to/in_and_output/"
classes = read.csv('formations.csv')
#############################

rcp = c('45', '85')
perc = c('50')
periods = c('1971_1990', '2079_2098')
type = 'weighting' 


formation = as.data.frame(cbind(classes$F2.Formation, classes$Dominant.Tree.Species, classes$Side.Tree.species))


primSpecList = extractSpecList(formation)
sideSpecList = extractSpecList(formation, 'side')

formationModel(namesSpec)

paramList=expand.grid(periods, rcp, perc, type, stringsAsFactors = FALSE)
#make nested list for calling in parallel computing
iterate_list <- split(paramList, seq(nrow(paramList)))

purrr::walk(.x=1:nrow(paramList), .f=prepareData)
