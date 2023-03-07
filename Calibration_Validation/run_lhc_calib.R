### SolBeePop ####################################################
### Calibration and validation
### Script for the generation the Latin Hypercube (LHC) for model calibration 
### Author: Amelie Schmolke / Colleen Roy
### Date: December 2022
###################################################################

rm(list = ls()) # clean up workspace
library(lhs)

## Inputs ---------------------------------------------------------------------
ofp = 'C:\\SolBeePop\\Calibration_Validaton\\Calibration'  # output file path
lhcn = 2000  # LHC count

## Get Data  ------------------------------------------------------------------
setwd(ofp)

# Table with interface parameters listed and identifications of parameters included in LHC simulations
idf = read.csv('input_S15_01803_latemat.csv', stringsAsFactors = FALSE)
idf$type[idf$type == ''] = NA
lv = idf$param[idf$type %in% c('fraction', 'integer')]  # lhc params

## Create LHC -----------------------------------------------------------------
ldf = as.data.frame(randomLHS(lhcn, length(lv)))
colnames(ldf) = lv
ldf$name = formatC(seq(1, nrow(ldf)), width = 4, flag = '0')

# update values to correct ranges
for(i in lv){
  ridf = idf[idf$param == i,]
  
  if(ridf$type == 'integer'){
    # create "bins" for each integer
    tdf = data.frame('int' = ridf$min:ridf$max)
    tdf$bin = 1:nrow(tdf)
    tdf$mxbin = tdf$bin / nrow(tdf)
    
    for(j in 1:nrow(tdf)){
      rtdf = tdf[j,]
      ldf[ldf[ridf$param] < rtdf$mxbin, ridf$param] = rtdf$int
    }
    rm(tdf, j, rtdf)
  } else if(ridf$type == 'fraction'){
    ldf[i] = ridf$min + ldf[i] * (ridf$max - ridf$min)
  } else {
    print('ERROR!\n')
    break
  }
}
rm(i, ridf)

# add constant / dependent variables
for(i in 1:nrow(idf)){
  ridf = idf[i,]
  
  if(ridf$type %in% c('integer', 'fraction')){
    next
  } else if(is.na(ridf$type)){
    ldf[ridf$param] = ridf$base
  } else if(ridf$type %in% idf$param){
    ldf[ridf$param] = ldf[,ridf$type]
  }
}
rm(i, ridf)

# output file
write.csv(ldf[,c('name', idf$param)], 'run_S15_01803_latemat.csv', row.names = FALSE)
