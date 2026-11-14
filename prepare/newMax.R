#!/usr/bin/env Rscrip


##change path to where the code is located
source("~/Workspace/pnvComp/sourceFunctions.R")

###### change path ##########
path = "/path/to/in_and_output/"
#############################

speciesNames=namesSpec
rcp = c('45','85')
perc = '50'
periods = c('1971_1990', '2079_2098')

paramList=expand.grid(speciesNames, rcp, perc, periods, stringsAsFactors = FALSE)
#make nested list for calling in parallel computing
iterate_list <- split(paramList, seq(nrow(paramList)))

purrr::walk(.x=1:nrow(paramList), .f=normalisePreds)

