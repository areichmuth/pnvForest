#!/usr/bin/env Rscript


options(digits=15)

suppressPackageStartupMessages(library(terra))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(sp))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(tidyterra))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(viridis))
suppressPackageStartupMessages(library(khroma))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(parallel))
suppressPackageStartupMessages(library(stars)) 

namesSpec= c("Abies.alba",
         "Abies.sp",
         "Acer.campestre",
         "Acer.pseudoplatanus",
         "Alnus.glutinosa",
         "Alnus.incana",
         "Betula.sp",
         "Carpinus.betulus",
         "Castanea.sativa",
         "Corylus.avellana",
         "Fagus.sylvatica",
         "Fraxinus.excelsior",
         "Fraxinus.ornus",
         "Larix.decidua",
         "Picea.abies",
         "Picea.sitchensis",
         "Pinus.contorta",
         "Pinus.halepensis",
         "Pinus.mugo",
         "Pinus.nigra",
         "Pinus.pinaster",
         "Pinus.pinea",
         "Pinus.sylvestris",
         "Populus.nigra",
         "Populus.tremula",
         "Prunus.avium",
         "Pseudotsuga.menziesii",
         "Quercus.cerris",
         "Quercus.faginea",
         "Quercus.frainetto",
         "Quercus.ilex",
         "Quercus.petraea",
         "Quercus.pubescens",
         "Quercus.pyrenaica",
         "Quercus.robur",
         "Quercus.sp",
         "Quercus.suber",
         "Robinia.pseudoacacia",
         "Salix.caprea",
         "Sorbus.aucuparia",
         "Tilia.sp")


safe_colorblind_palette = c("#72190E","#D1BBD7", "#BA8DB4", "#AA6F9E", "#882E72", "#2e6a3c", "#4EB265", "#90C987", "#CAE0AB", "#F7F056", "#F6C141", "#F1932D", "#E8601C", "#1965B0", "#5289C7", "#7BAFDE", "#B50E67")

normalisePreds = function(iter){
  species=iterate_list[[iter]]$Var1
  rcp=iterate_list[[iter]]$Var2
  perc=iterate_list[[iter]]$Var3
  period=iterate_list[[iter]]$Var4
  
  if (str_detect(species, '.ssp')==TRUE){
    species=str_replace(species, '.ssp', '.sp')
  }

  if (period=='1971_1990'){
    folder='Ref/'
    prediction = rast(paste0(path, folder, 'Prediction_',species,'_1971_1990_20.tif'))
    filename=paste0('Prediction_',species,'_',period)
  }else{
    folder=paste0('rcp',rcp,'/')
    filename=paste0('Prediction_',species,'_',period,'_20_rcp',rcp,'_quantile',perc)
    print(filename)
    prediction = rast(paste0(path, folder, filename,'.tif'))
    
  }
  
  maxim = 1/minmax(prediction)[2]
  changePred = round(prediction * maxim, digits=4)
  writeRaster(changePred, paste0(path, folder,filename,'_normalised.tif'), overwrite=TRUE)
}


extractSpecList = function(input, type='dominant'){
  primSpecList = list()
  if (type=='dominant'){
    k=2
  }else{k=3}
  for (i in seq(1,nrow(input),1)){
    spec = unlist(str_split(input[i,k], ', '))
    spec = str_replace(spec, ' ', '.')
    spec = str_replace(spec, 'Betula.pubescens', 'Betula.sp')
    spec = str_replace(spec, 'Betula.pendula', 'Betula.sp')
    primSpecList[[i]] = unique(spec)
  }
  return(primSpecList)
}


prepareData = function(iter){
  period=iterate_list[[iter]]$Var1
  rcp=iterate_list[[iter]]$Var2
  perc=iterate_list[[iter]]$Var3
  type=iterate_list[[iter]]$Var4
  
  csvAdd = '_prob.csv'

  
  if (period=='1971_1990'){
    pattern=paste0(period,'_normalised')
    fileList = list.files(paste0(path,'Ref'), pattern, full.names=TRUE)
    specListNames=str_split(fileList, '_')
    csvName=paste0('spec_',period,csvAdd)
  }else{
    folder=paste0('rcp',rcp,'/')
    pattern=paste0(period,'_20_rcp',rcp, '_quantile',perc,'_normalised')
    fileList = list.files(paste0(path,'rcp',rcp), pattern, full.names=TRUE)
    specListNames=str_split(fileList, '_')
    csvName=paste0('spec_',period,'_rcp',rcp,'_perc',perc,csvAdd)
  }
  print(fileList)
  prediction = rast(fileList[1])
  ref=as.data.frame(prediction, xy=TRUE)
  colnames(ref)=c('x','y',specListNames[[1]][2])
  ref$x = round(ref$x, digits=4)
  ref$y = round(ref$y, digits=4)
  for (i in seq(1,length(specListNames),1)){
    species=specListNames[[i]][2]
    #filters out the correct names - no worries about various time intervals
    predFile=rast(fileList[i])
    raster_to_points = as.data.frame(predFile)
    ref[species] = raster_to_points[1]
  }
  write.csv(ref,paste0(path,csvName), row.names=FALSE)

}


formationModel = function(specNames){
  
  availSpecs = data.frame(Class=character(),
                          dominant_Formation = character(),
                          side_Formation = character(),
                          dominant_Model = character(),
                          side_Model = character(),
                          count_dominant_Formation=integer(),
                          count_side_Formation=integer(),
                          dominant_Percent=integer(),
                          side_Percent=integer(),
                               stringsAsFactors=FALSE)
  
  for (i in seq(1,nrow(formation),1)){#,nrow(formation),1)){
    print(i)
    specTrueDom = keep(primSpecList[[i]], function(x) x %in% specNames)
    domPerc = round((length(specTrueDom)*100)/length(primSpecList[[i]]), digits=2)
    
    specTrueSide = keep(sideSpecList[[i]], function(x) x %in% specNames)
    count_domForm = length(primSpecList[[i]])
    count_sideForm = length(sideSpecList[[i]])
    sidePerc = round((length(specTrueSide)*100)/length(sideSpecList[[i]]), digits=2)
    specTrue = c(specTrueDom, specTrueSide)
    
    availSpecs[i,] = cbind(formation[i,], paste(specTrueDom, collapse=", "), 
                          paste(specTrueSide, collapse=", "), 
                          count_domForm, count_sideForm, domPerc, sidePerc)
  }
  write.csv(availSpecs, paste0(path, 'formationModel.csv'), row.names = FALSE)
  
}

extractData=function(species, rcp, period){
  if (period == '2005'){
    return()
  }
  filenameDiff = paste0(species, '_', period, '_rcp', rcp, '_diff.tif')
  rasterTime = rast(paste0(path,'rasterDiff/rcp',rcp,'/',filenameDiff))
  rasterFrame = terra::as.data.frame(rasterTime, xy=T)
  colnames(rasterFrame)=c('x', 'y', 'value')
  ebrDat=vect(paste0(path,'BiogeoRegions2016_clip_4623.shp'))
  ebrDat = terra::as.data.frame(rasterize(ebrDat, rasterTime ,field="code"), xy=T)
  colnames(ebrDat)=c('x', 'y', 'code')
  ref = vect(paste0(path, 'extent.shp'))
  ref = terra::as.data.frame(rasterize(ref, rasterTime), xy=T)
  colnames(ref)=c('x', 'y', 'z')
  data=merge(x = ref, y = ebrDat, by = c("x","y"), all.x = TRUE)
  data=merge(x = data, y = rasterFrame, by = c("x","y"), all = TRUE)
  data$period = rep(period,times=nrow(data))
  data$rcp = rep(as.numeric(rcp)/10,times=nrow(data))
  data = select(data, -c('x', 'y', 'z'))
  return(cbind(data))
}


extractPoly = function(poly, rasterData, aggrStat, baseRaster){
  polySUM = terra::extract(rasterData, poly, exact=F, fun = aggrStat, na.rm=T, ID=FALSE)
  baseVal = terra::extract(baseRaster, poly, exact=F, fun = aggrStat, na.rm=T, ID=FALSE)
  #poly$mean[sapply(poly$mean, is.infinite)] <- NA
  newPoly = as.data.frame(cbind(F_2=poly$F_2, All=baseVal[,1], polySUM))
  pnvPoly = left_join(poly, newPoly, by='F_2')
  return(pnvPoly)
}


plotDiff = function(pathPlus, fileInput, filenames){
  rasterPoints = as.data.frame(fileInput, xy=TRUE)
  
  for (i in seq(3,ncol(rasterPoints),1)){
    dataToPlot = rasterPoints[,c(1,2,i)]
    
    colnames(dataToPlot)[3] = 'value'
    if (pathPlus == 'cutoffDiff_' | pathPlus == 'cutoff_'){dataToPlot[,3]=as.factor(dataToPlot[,3])}
    g=ggplot() +
      geom_raster(data=dataToPlot, aes(x = x, y = y, fill=value))+
      coord_sf(crs = 3035)+
      theme_bw()+
      theme(plot.margin=margin(t = 1, r = 0, b = 0, l = 0, 'mm'),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y = element_text(angle = 90, vjust=0.5, hjust=0.5, size=21, margin = margin(t = .3, unit = "cm")),
            axis.text.x = element_text(size=21, margin = margin(t = .3, unit = "cm")),
            legend.text = element_text(size=26, margin = margin(t = .3, unit = "cm")),
            legend.title=element_blank(),
            legend.position='bottom', legend.text.position='bottom',
            legend.direction='horizontal')

    if (pathPlus == 'sumDiff_'){
      low='#8E0152' #"#562456"
      high='royalblue' #"#325f2c"
      mid='grey'#'#FFFFBF' #"#FFFFD8"
      limits=c(-10,10)
      g=g+
        scale_fill_gradient2(low = low, mid=mid, high = high, na.value = NA, 
                             limits=limits, 
                             breaks=seq(limits[1], limits[2], 2), labels=seq(limits[1], limits[2], 2))+ 
        theme(legend.key.height = unit(1, 'cm'),
              legend.key.width = unit(4.5, 'cm'),
              legend.key.spacing = unit(0,'mm'))
      extra=''    }
    else{
      #low='#8E0152' #"#562456"
      high='royalblue' #"#325f2c"
      mid='lightblue'
      low='gray40'#'#FFFFBF' #"#FFFFD8"
      maxDat = max(dataToPlot[,3])
      minDat = min(dataToPlot[,3])
      midpoint=(maxDat-minDat)/2
      limits=c(0, maxDat)
      
      g=g+
        scale_fill_gradient2(low = low, mid=mid, high = high, na.value = NA, 
                             midpoint=midpoint, limits=limits, 
                             breaks=seq(minDat,maxDat,2), labels=seq(minDat,maxDat,2))+ 
        theme(legend.key.height = unit(1, 'cm'),
              legend.key.width = unit(4.5, 'cm'),
              legend.key.spacing = unit(0,'mm'))
      extra=''
    }
    pdf(paste0(path, 'plot/', pathPlus, filenames,  extra,'.pdf'), width = 10, height = 13)
    print(g)
    dev.off()
  }
}


comparePnv = function(r){
  time = iterate_list[[r]]$Var1
  rcp = iterate_list[[r]]$Var2
  perc = iterate_list[[r]]$Var3
  type = iterate_list[[r]]$Var4


  if (time=='1971_1990'){
    filenames = time
  }else{
    filenames = paste0(time,'_rcp',rcp,'_perc',perc)
  }
  filenames=paste0(filenames,'_cutoff')

  rasterClasses = rast(paste0(path, 'tif/Formationen_',filenames,'.tif'))  

  classSum = sum(rasterClasses)
  writeRaster(classSum, paste0(path, 'tif/sum_',filenames,'.tif'), datatype='INT2U', gdal=c("COMPRESS=DEFLATE"), overwrite=TRUE)
  plotDiff('sum_', classSum, filenames)
  polyRasterClass=extractPoly(poly, rasterClasses, 'sum', baseRaster)
  write.csv(as.data.frame(polyRasterClass), paste0(path,'compare/pnv_',filenames,'_extract_sum.csv'), row.names=F)
  rasterClasses=NULL
  polyRasterClass=NULL
  gc()
}

formNames = function(formations){
  nameForm = names(formations)
  str=list()
  for (i in seq(1,length(nameForm),1)){
    str[[i]] = paste0(nameForm[i],'=character(), ')
  }
  
  s=paste(str, collapse = '')
  sumDiff = paste0('sumDiff = data.frame(', s, ' stringsAsFactors=FALSE)')
  return(sumDiff)
}



evalShp = function(filenames, dataInput, extra, limits){
  nlen = length(unique(dataInput$F_2))
  g = ggplot(data = dataInput, aes(y=value, x=1, fill = variable))  + 
    geom_bar(stat = "identity",  position = "stack", alpha=0.7) +
    facet_grid(. ~F_2, scales = 'free') + 
    xlab('PNV formations') + ylab('Proportion of emulated formations')+ ylim(limits[1], limits[2])+ #Proportion of class
    theme(plot.margin=margin(t = 0, r = 2, b = 0, l = 0, 'mm'),
          axis.text.x = element_blank(), axis.ticks.x=element_blank(),
          axis.title.y = element_text(size=21, margin = margin(t = .3, unit = "cm")),
          axis.title.x = element_text(size=21, margin = margin(t = .3, unit = "cm")),
          axis.text.y = element_text(size=21, margin = margin(t = .3, unit = "cm")),
          axis.ticks.length=unit(.25, "cm"),
          panel.border = element_blank(), panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(), legend.title = element_blank(),
          legend.text = element_text(size=21, margin = margin(t = 0.3, unit = "cm")),
          legend.key.width = unit(1, 'cm'),
          legend.key.height = unit(1, 'cm'),
          legend.key.size = unit(2, 'cm'),
          legend.key.spacing = unit(2,'mm'),
          strip.text.x = element_text(size=21)) + 
    scale_fill_manual(values=safe_colorblind_palette)
  
  if (extra=='_selected'){
    extra=paste0('Extract_', filenames, extra)
  }else{
    extra=paste0('PeriodDiff_barplot_', extra)
    g=g+geom_hline(aes(yintercept=0), colour="darkgrey", lwd=1)
  }
  gg2 <- ggplot_gtable(ggplot_build(g))
  stripr <- which(grepl('strip-t', gg2$layout$name))
  k <- 1
  for (i in stripr) {
    j <- which(grepl('rect', gg2$grobs[[i]]$grobs[[1]]$childrenOrder))
    gg2$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- alpha(safe_colorblind_palette[k], .7)
     gg2$grobs[[i]]$grobs[[1]]$children[[j]]$gp$lwd <- 10  
    k <- k+1
  }
  plot(gg2)
  
  
  # save
  ggsave(paste0(path,'plot/',extra, '.pdf'),
         gg2,
         dpi = 300,
         width = 28,height= 20,units = "cm")
}
  
tssMaxim = function(data){
  #here the PNV poly data serve as refeence
  observations=data[,1]
  #each emulated iteration
  predictions=data[,2]
  ord <- order(predictions, decreasing = TRUE)
  pred_sorted <- predictions[ord]
  obs_sorted  <- observations[ord]  
  
  ##true positive
  tp <- cumsum(obs_sorted == 1)
  ##false positive
  fp <- cumsum(obs_sorted == 0)
  
  #The total number of presence 
  P <- sum(obs_sorted == 1)
  
  ##The total number of absences
  N <- sum(obs_sorted == 0)
  
  ##false negataive
  fn <- P - tp
  ##true negative
  tn <- N - fp
  
  ##sensitivity and specificity
  sens <- tp / P
  spec <- tn / N
  
  tss <- sens + spec - 1
  max_tss <- max(tss)
  best_threshold <- pred_sorted[which.max(tss)]
  
  trunc <- function(x, ..., prec = 0) base::trunc(x * 10^prec, ...) / 10^prec
  
  mintss = trunc((-1*max_tss),prec=4)
  ##here we concate the TSS and its threshold - not possible otherwise to handover two parameters
  finalCost = as.numeric(paste0(10000*mintss, '000', 10000*trunc(best_threshold,prec=4)))
  return(finalCost)

}




inv.optim <- function(ref.meas, ref, parm_min, parm_max, specTrueDom=NULL, specTrueSide=NULL, formName=NULL){
  
  ##pnv classes as points
  ref.meas <<- ref.meas
  ref <<- ref
  
  getMax = function(minMaxVal=F, rasterBin){
    quant = quantile(rasterBin, probs = c(0.95))

    minVal = min(rasterBin)
    maxVal = c(minVal, quant)
    if (minMaxVal==F){
      return(rasterBin)
    }else{
      return(maxVal)
    }
  }
  
  ## define cost function
  Cost.prosp <- function(input){
  
  
    afun = function(x){x %*% diag(perc)}
    if (length(specTrueDom)>1){
      perc=input[1:length(specTrueDom)]

      checkDom = rowSums(afun(as.matrix(ref[,c(specTrueDom)])))
    }else{
      checkDom = input[1]*ref[,c(specTrueDom)]
    }
    perc=input[(1+length(specTrueDom)):length(input)]
    
    if (length(specTrueSide)>1){
      checkSide = rowSums(afun(as.matrix(ref[,c(specTrueSide)])))
    }else{
      checkSide = input[1+length(specTrueDom)]*ref[,c(specTrueSide)]
    }
    sub = rowSums(cbind(checkDom, checkSide))/sum(input)
    valuesVal = as.data.frame(cbind(x=ref$x, y=ref$y, meanVal=round(sub, digits=4)))
  
    
    bothData <- inner_join(x = ref.meas, y = valuesVal, by = join_by(x, y) )

    ####TSS####
    dataV <<- bothData[,3:4] 
    cost=tssMaxim(dataV)
    ###TSS######

    
    return(cost)
    
  }
  
  NP = length(parm_min)*10
  ## define DEoptim control parameters


  DEctrl <- list(NP=NP,F=0.8,CR=0.5,trace=5,itermax=200, reltol=0.8)
  inv <- DEoptim(Cost.prosp, lower=parm_min, upper=parm_max, DEctrl)
  
  print('inv finish')
  return(inv)  
}
  

optimDE = function(i){
  print(paste(time, rcp, sep=' '))

  filenames = '1971_1990'
  s=formation[i,1]
  print(paste('formation name = ',s, sep=' '))
  #filter formations for the ones considered
  ref.shp=shpPnv %>% mutate(F_2 = ifelse(F_2 == s, 1, 0)) 
  if (sum(ref.shp$F_2)==0){return()}  
  
  print(type)
  
  ref = read.csv(paste0(path,'spec_', filenames,'_prob.csv'))
  specTrueDom = keep(primSpecList[[i]], function(x) x %in% names(ref))
  specTrueSide = keep(sideSpecList[[i]], function(x) x %in% names(ref))
  domPerc = round((length(specTrueDom)*100)/length(primSpecList[[i]]), digits=2)
  sidePerc = round((length(specTrueSide)*100)/length(sideSpecList[[i]]), digits=2)

  if (domPerc>=50 && sidePerc >= 40){
    ## Set ranges for weights
    len=length(specTrueDom)+length(specTrueSide)
    parm_min <- rep(0,len) #0.1
    parm_max <- rep(10,len)
    optimObject = inv.optim(ref.shp, ref, parm_min, parm_max, specTrueDom, specTrueSide)
    
    saveRDS(optimObject, file = paste0(path,'optimisation/',filenames,'_',s,'_optim.rds'))
  }else{print(formation[i,1])
    return()}
  
}      
   


applyOptimDE = function(r){
  time = iterate_list[[r]]$Var1
  rcp = iterate_list[[r]]$Var2
  perc = iterate_list[[r]]$Var2
  type = iterate_list[[r]]$Var4
  print(paste(time, rcp, sep=' '))
  if (time=='1971_1990'){
    filenames = time
  }else{
    filenames = paste0(time,'_rcp',rcp,'_perc50')
  }
  
  
  if(type=='weighting'){
    ref = read.csv(paste0(path,'spec_', filenames,'.csv'))
    meanFormat = data.frame(x=ref$x, y=ref$y)
    filesOptim = list.files(paste0(path,'optimisation/'), pattern=paste0('1971_1990','.+_optim.'))
      for (i in filesOptim){
        form=str_split(i, '1971_1990_')[[1]][2]
        formName=str_split(form, '_')[[1]][1]
        s=(formation[,1]==formName)*1
        form= match(1,s)
        print(form)
        specTrueDom = keep(primSpecList[[form]], function(x) x %in% names(ref))
        specTrueSide = keep(sideSpecList[[form]], function(x) x %in% names(ref))
        afun = function(x){x %*% diag(perc)}
        bestVal = readRDS(paste0(path,'optimisation/',i))
        bestVal = round(as.numeric(bestVal$optim$bestmem), digits=4)
        if (length(specTrueDom)>1){
          perc=bestVal[1:length(specTrueDom)]
          checkDom = rowSums(afun(as.matrix(ref[,c(specTrueDom)])))
        }else{
          checkDom = bestVal[1]*ref[,c(specTrueDom)]
        }
        ##now the sideSpecies
        perc=bestVal[(1+length(specTrueDom)):length(bestVal)]

        if (length(specTrueSide)>1){

          checkSide = rowSums(afun(as.matrix(ref[,c(specTrueSide)])))

        }else{
          checkSide = bestVal[1+length(specTrueDom)]*ref[,c(specTrueSide)]
        }
        sub = rowSums(cbind(checkDom, checkSide))/sum(bestVal)
        valuesVal = as.data.frame(cbind(x=ref$x, y=ref$y, meanVal=round(sub, digits=4)))
        colnames(valuesVal)[3] = formName
        meanFormat[[formName]] = valuesVal[,3]
      }
  }else if (type=='cutoff'){
    filesOptim = list.files(paste0(path,'optimisation/'), pattern=paste0('1971_1990','.+_optim.'))
    raster = rast(paste0(path,'tif/Formationen_',filenames,'_weighting.tif'))
    meanFormat = as.data.frame(raster, xy=T)
    meanFormat = data.frame(x=meanFormat$x, y=meanFormat$y)

    for (i in filesOptim){
        form=str_split(i, '1971_1990_')[[1]][2]
        formName=str_split(form, '_')[[1]][1]
        s=(formation[,1]==formName)*1
        form= match(1,s)
        print(form)
        bestVal = readRDS(paste0(path,'optimisation/',i))
        ######TSS########
        print(bestVal$optim$bestval)
        ###because we concatenated TSS value and threshold in one
        tssThres = str_split(bestVal$optim$bestval, '000')
        print(tssThres)
        tssVal = tssThres[[1]][1]
        thresh = tssThres[[1]][2]
        thresh = str_remove(thresh, "^0+")
        nVal = nchar(thresh)
        mas = 1*10^nVal
        bestVal = as.numeric(thresh)/mas
        

        #######TSS#######
        
        #######RMSE######
        #bestVal = round(as.numeric(bestVal$optim$bestmem), digits=4)
        #######RMSE######
        print(bestVal)
        rasterBin = raster[formName]
        rasterBin = ifel(rasterBin >= bestVal, 1, 0)
        sub = as.data.frame(rasterBin, xy=T)
        valuesVal = as.data.frame(cbind(x=round(sub$x,4), y=round(sub$y,4), meanVal=round(sub[,3], digits=4)))
        colnames(valuesVal)[3] = formName
        meanFormat[[formName]] = valuesVal[,3]
        print('------------------------')
    }
  }
meanFormat = meanFormat %>% select(where(function(x) any(!is.na(x))))
  toRaster = rast(meanFormat, crs=projText)
  writeRaster(round(toRaster, digits=4), paste0(path,'tif/Formationen_',filenames, '_',type,'.tif'), gdal=c("COMPRESS=DEFLATE"), overwrite=TRUE)

}

