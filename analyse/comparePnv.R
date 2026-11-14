#!/usr/bin/env Rscrip


##change path to where the code is located
source("~/Workspace/pnvComp/sourceFunctions.R")

###### change path ##########
path = "/path/to/in_and_output/"
## and please create following directory within: compare/
## and please create following directory within: plot/

# polygon available under Federal Agency for Nature Conservation (BfN), see readme 
poly = "name/of/PNV_vector_data.shp"
#############################

# might need to use only one at a time
rcp = c('45', '85')
perc = '50'
period = c('1971_1990', '2079_2098')
type='prob' 

baseRaster = rast(paste0(path,'Europe_1971.tif'))

##generate all combinations to iterate through
paramList=expand.grid(period, rcp, perc, type, stringsAsFactors = FALSE)
paramList=setDT(paramList)[Var1 == "1971_1990", c('Var2','Var3'):="0"]
paramList = distinct(paramList, Var1, Var2, Var3, Var4, .keep_all= TRUE)
#make nested list for calling in parallel computing
iterate_list <- split(paramList, seq(nrow(paramList)))

purrr::walk(.x=1:nrow(paramList), .f=comparePnv)