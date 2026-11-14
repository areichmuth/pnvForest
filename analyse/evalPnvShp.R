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
type='prob' 


##generate all combinations to iterate through
paramList=expand.grid(period, rcp, perc, type, stringsAsFactors = FALSE)
paramList=setDT(paramList)[Var1 == "1971_1990", c('Var2','Var3'):="0"]
paramList = distinct(paramList, Var1, Var2, Var3, Var4, .keep_all= TRUE)
#make nested list for calling in parallel computing
iterate_list <- split(paramList, seq(nrow(paramList)))
print(paramList)

##
for (i in seq(1,nrow(paramList),1)){
  extra='_selected'
  if (paramList[i,1]=='1971_1990'){
    filenames = paramList[i,1]
  }else{
    filenames = paste0(paramList[i,1],'_rcp',rcp,'_perc',perc)
    print(filenames)
  }

  filenames=paste0(filenames, '_cutoff')
  
  data = read.csv(paste0(path, 'compare/pnv_', filenames,'_extract_sum.csv'))
  data = data[,c(1,8:ncol(data))]
  dataPercSelect <- data %>% filter(F_2 %in% names(data))
  write.csv(dataPercSelect[,-2], paste0(path, 'compare/Extract_', filenames,extra, '.csv'), row.names = F)
  dataPercSelect = cbind(F_2=dataPercSelect[,1],round((dataPercSelect[,3:ncol(dataPercSelect)]/dataPercSelect$All),digits=3))
  ## F_2 is ID within vector data
  melt_dataSelect <- melt(dataPercSelect, id = "F_2")
  evalShp(filenames, melt_dataSelect, '_selected', c(0,6.2))
  dataOut = transpose(dataPercSelect)
  colnames(dataOut) = dataOut[1,]
  dataOut = dataOut[-1,]
  dataOut = cbind(F_2=names(dataOut), dataOut)
  write.csv(dataOut, paste0(path, 'compare/Barplot_', filenames,extra, '.csv'), row.names = F)
  
}
extra=rcp
print(extra)
## we just generated that above
df1 = read.csv(paste0(path, 'compare/Extract_1971_1990_cutoff_selected.csv'))
df2 = read.csv(paste0(path, 'compare/Extract_2079_2098_rcp',extra,'_perc50_cutoff_selected.csv'))

dataPercDiff <- data.frame(matrix(ncol = ncol(df1), nrow = nrow(df1)))
colnames(dataPercDiff) = colnames(df1)
dataPercDiff$F_2 = df1$F_2
for (i in seq(1,nrow(df1),1)){
  for (j in seq(2,ncol(df2),1)){
    vec = max(c(df1[i,j], df2[i,j]))

    datap = (df2[i,j]-df1[i,j])/vec
    dataPercDiff[i,j] = round(datap, digits=3)

  }
}
write.csv(dataPercDiff, paste0(path, 'compare/PeriodDiff_', 'extract',extra, '.csv'), row.names = F)

melt_dataDiff <- melt(dataPercDiff, id = "F_2")
evalShp('', melt_dataDiff, extra, c(-16,10.5))
dataOut = transpose(dataPercDiff)
colnames(dataOut) = dataOut[1,]
dataOut = dataOut[-1,]
dataOut = cbind(F_2=names(dataOut), dataOut)
write.csv(dataOut, paste0(path, 'compare/PeriodDiff_', 'barplot',extra, '.csv'), row.names = F)

