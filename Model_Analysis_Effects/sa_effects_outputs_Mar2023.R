### SolBeePop_ecotox ####################################################
### Model analysis
### Script for the combination of individual output files from the sensitivity and uncertainty analysis simulations 
###  generates "outputs_" file that is used by script "sa_effects_prcc_Mar2023.R"
### Author: Amelie Schmolke 
### Date: March 2023
###################################################################

library(sensitivity)
library(reshape2)
library(ggplot2)
library(plyr)

rm(list = ls()) # clean up workspace

## Inputs  --------------------------------------------------------------------
ofp = 'C:\\SolBeePop_ecotox\\Model_Analysis\\Sensitivity_analysis_effects\\Osmia\\sims_Osmia_SA_control_Mar2023\\'  # output file path
sim_set_name = 'run_Osmia_control_Mar2023'

# LHC vars
lv = c('var.emerge.f', 't.maturation', 'max.nesting.life',  
  'p.max.nesting.life', 'max.f.ratio', 'max.cells', 'max.survival.e.f',
  'emerged.survival', 'a.cell.age', 'a.sex.age', 'a.size.age',
  'a.cell.resource', 'a.sex.resource', 'a.size.resource',
  'ad.nectar.cons','ad.pollen.cons','ad.ET','TC_soil',
  'kd_IT','mw_IT','Fs_IT', # GUTS-IT parameters
#  'kd_SD','bw_SD','mw_SD', # GUTS-SD parameter
  'nectar_prop','weight.prov', 'SM', 'dr.intercept', 'dr.slope')
ov = list('bees.emerged.yr' = 730, 'f.emerged.yr' = 730, 'm.emerged.yr' = 730,  # out vars & end dates
  'bees.nesting' = 365, 'sum.cells' = 365, 'sum.f.cells' = 365, 'sum.m.cells' = 365,
  'deaths.exp.in' = 730, 'deaths.exp.ad' = 365)

## Get Data  ------------------------------------------------------------------
setwd(ofp)

ldf = read.csv(paste0(sim_set_name,'.csv'), stringsAsFactors = FALSE, check.names = FALSE)
ldf = ldf[c('name', lv)]

odf = data.frame(matrix(nrow = 0, ncol = length(ov) + 2))
colnames(odf) = c('name', 'RndSeed', names(ov))

for(i in 1:nrow(ldf)){
#for(i in 1:10){
  tdf = read.csv(paste0(ldf$name[i], '.csv'),
    stringsAsFactors = FALSE, skip = 6)
  tdf$name = ldf$name[i]
  for(j in names(ov)){  # get values for PRCC
    udf = tdf[tdf$X.step. == ov[[j]],c('name', 'RndSeed', j)]
    if(j == names(ov)[1]){
      vdf = udf
    } else {
      vdf = merge(vdf, udf)
    }
    rm(udf)
  }
  odf = rbind(odf, vdf[colnames(odf)])
  rm(j, vdf)
}
rm(i, tdf)

odf = merge(ldf, odf)

write.csv(odf, paste0('outputs_',sim_set_name,'.csv'), row.names = FALSE)
