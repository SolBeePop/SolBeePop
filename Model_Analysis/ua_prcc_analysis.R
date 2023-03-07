### SolBeePop ####################################################
### Model analysis
### Script for the calculation of PRCC from the model uncertainty analysis simulations 
### Author: Amelie Schmolke / Colleen Roy
### Date: February 2023
###################################################################

rm(list = ls()) # clean up workspace
library(sensitivity)
library(reshape2)
library(ggplot2)
library(plyr)

## Inputs  --------------------------------------------------------------------
ofp = 'C:\\SolBeePop\\Model_Analysis\\'  # output file path
sim_set_name = 'run_Osmia_1'

lv = c('var.emerge.f', 't.maturation', 'max.nesting.life',  # LHC vars
  'p.max.nesting.life', 'max.f.ratio', 'max.cells', 'max.survival.e.f',
  'emerged.survival', 'a.cell.age', 'a.sex.age', 'a.size.age',
  'a.cell.resource', 'a.sex.resource', 'a.size.resource')
ov = list('bees.emerged.yr' = 730, 'f.emerged.yr' = 730, 'm.emerged.yr' = 730,  # out vars & end dates
  'bees.nesting' = 365, 'sum.cells' = 365, 'sum.f.cells' = 365, 'sum.m.cells' = 365)

## Read simulation output data  ------------------------------------------------------------------
setwd(ofp)

ldf = read.csv(paste0(sim_set_name,'.csv'), stringsAsFactors = FALSE, check.names = FALSE)
ldf = ldf[c('name', lv)]

odf = data.frame(matrix(nrow = 0, ncol = length(ov) + 2))
colnames(odf) = c('name', 'RndSeed', names(ov))

for(i in 1:nrow(ldf)){
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
##
#odf = read.csv(paste0('outputs_',sim_set_name,'.csv')) # if output file already exists

# add sex ratio of emerged bees as output
ov <- c(ov, 'f.sex.ratio.emerged' = 730)
odf$f.sex.ratio.emerged = odf$f.emerged.yr/odf$bees.emerged.yr

## Calculate PRCC & Plot  -----------------------------------------------------
rdf = data.frame(var = lv)

for(j in names(ov)){  # per output measure
  tdf = pcc(X = odf[lv], y = odf[,j], rank = TRUE)
  tdf = tdf$PRCC
  colnames(tdf)[1] = j
  tdf$var = rownames(tdf)
  rdf = merge(rdf, tdf)
  rm(tdf)
}
rm(j)

write.csv(rdf, paste0('prcc_',sim_set_name,'.csv'), row.names = FALSE)

## Plot all PRCC (table created in previous section) ------------------------------
tdf = melt(rdf, id = 'var')

plt = ggplot(tdf) +
  geom_point(aes(x = value, y = var)) +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_y_discrete(limits = rev) +
  theme_bw() + facet_grid(variable~.) +
  theme(axis.title = element_blank())
ggsave(paste0('prcc_',sim_set_name,'.png'), plt, width = 6.5, height = 9)

