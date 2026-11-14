

source("~/Workspace/pnvComp/sourceFunctions.R")
options(digits=15)

forms = c("C2", "D1", "D3", "D4", "D5", "F1", "F2", "F3", "F5", "G1", "G2", "G3", "G4", "J1", "J2", "K1", "L1")

###### change path ##########
path = "/path/to/in_and_output/"

# polygon available under Federal Agency for Nature Conservation (BfN), see readme 
poly = "name/of/PNV_vector_data.shp"

classes = read.csv('formations.csv')
#############################


formation = as.data.frame(cbind(classes$F2.Formation, classes$Dominant.Tree.Species, classes$Side.Tree.species))
shpPnv = as.data.frame(poly, xy=TRUE)

ref = read.csv(paste0(path,'spec_1971_1990.csv'))

primSpecList = extractSpecList(formation)
sideSpecList = extractSpecList(formation, 'side')


##########
#TSS value and weights per species
##########

tssList = data.frame(Form=character(), cutoff_TSS=double(), threshold_TSS=double())
weights = data.frame(Form=character(), weights=character())

for (i in seq(1,length(forms),1)){
  specTrueDom = keep(primSpecList[[i]], function(x) x %in% names(ref))
  specTrueSide = keep(sideSpecList[[i]], function(x) x %in% names(ref))
  domPerc = round((length(specTrueDom)*100)/length(primSpecList[[i]]), digits=2)
  sidePerc = round((length(specTrueSide)*100)/length(sideSpecList[[i]]), digits=2)
  nameForm=formation[i,1]
  print(nameForm)
  ref.shp=shpPnv %>% mutate(F_2 = ifelse(F_2 == nameForm, 1, 0)) #0
  if (sum(ref.shp$F_2)!=0){
    if (domPerc>=50 && sidePerc >= 40){
      filesForm = list.files(path, paste0('_',nameForm,'_'), recursive=F, full.names = T)
      cutoff = readRDS(filesForm[1])
      tssThres = str_split(cutoff$optim$bestval, '000')
      tssVal = -1*as.numeric(tssThres[[1]][1])
      thresh = tssThres[[1]][2]
      thresh = str_remove(thresh, "^0+")
      nVal = nchar(thresh)
      mas = 1*10^nVal
      bestThres = as.numeric(thresh)/mas
      nVal = nchar(tssVal)
      mas = 1*10^nVal
      bestTSS = tssVal/mas
      weighting = as.character(round(cutoff$optim$bestmem/sum(cutoff$optim$bestmem),digits=4))
      tssList[nameForm,] = cbind(nameForm, bestTSS, bestThres)
      weights[nameForm,] = cbind(nameForm, paste(c(specTrueDom, specTrueSide), collapse=','))
      weights[paste0(nameForm,'w'),] = cbind(nameForm, paste(weighting, collapse=','))

    }
  }
}


write.csv(tssList, paste0(path,'optimValues.csv'), row.names = F)
write.csv(weights, paste0(path,'optimWeights.csv'), row.names = F)

##########
#summary stats
##########

stats85 = data.frame(Form=character(), gain=character(), loss=character())
stats45 = data.frame(Form=character(), gain=character(), loss=character())

for (i in seq(1,length(forms),1)){
    ######stats######
  nameForm = forms[i]
  raster = rast(paste0(path,'Formationen_',nameForm,'_2079_2098_rcp45_perc50_weighting_diff.tif'))
  raster=raster/10000
  raster[raster==-999]=NA
  raster = as.data.frame(raster)
  m1=round(mean(raster[nameForm][which(raster[nameForm]>0),], na.rm=TRUE)*100,2)
  m2=round(mean(raster[nameForm][which(raster[nameForm]<0),], na.rm=TRUE)*100,2)
  stats45[nameForm,] = cbind(nameForm,m1,m2)
  
  raster = rast(paste0(path,'Formationen_',nameForm,'_2079_2098_rcp85_perc50_weighting_diff.tif'))
  raster=raster/10000
  raster[raster==-999]=NA
  raster = as.data.frame(raster)
  m1=round(mean(raster[nameForm][which(raster[nameForm]>0),], na.rm=TRUE)*100, 2)
  m2=round(mean(raster[nameForm][(raster[nameForm]<0),], na.rm=TRUE)*100, 2)
  stats85[nameForm,] = cbind(nameForm,m1,m2)

}

write.csv(stats45, paste0(path,'stats45.csv'), row.names = F)
write.csv(stats85, paste0(path,'stats85.csv'), row.names = F)
