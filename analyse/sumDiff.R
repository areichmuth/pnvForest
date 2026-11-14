#!/usr/bin/env Rscrip


##change path to where the code is located
source("~/Workspace/pnvComp/sourceFunctions.R")

###### change path ##########
path = "/path/to/in_and_output/"
#############################

# might need to use only one at a time
rcp = c('45','85')
perc = c('50') 
period = c('1971_1990', '2079_2098')


data1 = rast(paste0(path,'tif/sum_1971_1990_cutoff.tif'))
data2 = rast(paste0(path,'tif/sum_2079_2098_rcp',rcp,'_perc50_cutoff.tif'))
dataDiff = data2-data1

plotDiff('sumDiff_', dataDiff, paste0('1971-2079_',rcp))

writeRaster(dataDiff, paste0(path, 'tif/sumDiff_cutoff_',rcp,'.tif'), overwrite=T)

#######
##generate statistics
#######

dataDiff45 = rast(paste0(path, 'tif/sumDiff_cutoff_45.tif'))
dataDiff85 = rast(paste0(path, 'tif/sumDiff_cutoff_85.tif'))


dataDiffPoints45 = as.data.frame(dataDiff45) 
dataDiffPoints85 = as.data.frame(dataDiff85) 

m145=mean(dataDiffPoints45[which(dataDiffPoints45>0),], na.rm=TRUE)
m245=mean(dataDiffPoints45[which(dataDiffPoints45<0),], na.rm=TRUE)
res45 = cbind(max=m145,min=m245)

m185=mean(dataDiffPoints85[which(dataDiffPoints85>0),], na.rm=TRUE)
m285=mean(dataDiffPoints85[which(dataDiffPoints85<0),], na.rm=TRUE)
res45 = cbind(max=m145,min=m245)
res85 = cbind(max=m185,min=m285)

print(paste0('min and max RCP 4.5: ',res45))
print(paste0('min and max RCP 8.5: ',res85))

