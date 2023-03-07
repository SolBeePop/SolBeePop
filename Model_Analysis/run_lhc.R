### SolBeePop ####################################################
### Model analysis
### Script for the generation the Latin Hypercube (LHC) for model analysis (parameter uncertainty)
### Author: Amelie Schmolke / Colleen Roy
### Date: January 2022
###################################################################

rm(list = ls()) # clean up workspace
library(lhs)

## Inputs ---------------------------------------------------------------------
ofp = 'C:\\SolBeePop\\Model_analysis'  # output file path
lhcn = 3800  # LHC count

## Get Data  ------------------------------------------------------------------
setwd(ofp)

idf = read.csv('input_Nomia_3.csv', stringsAsFactors = FALSE)
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

write.csv(ldf[,c('name', idf$param)], 'run_Nomia_3.csv', row.names = FALSE)
