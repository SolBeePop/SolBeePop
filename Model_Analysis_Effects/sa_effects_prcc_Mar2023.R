### SolBeePop_ecotox ####################################################
### Model analysis
### Script for the calculation of PRCC from the model sensitivity and uncertainty analysis simulations 
###  script requires that files "outputs_" have been generated (using script "sa_effects_outputs_Mar2023.R")
###  PRCC calculated using the relative effect sizes
### Author: Amelie Schmolke 
### Date: March 2023
###################################################################

library(sensitivity)
library(reshape2)
library(ggplot2)
library(plyr)

rm(list = ls()) # clean up workspace

## Inputs  --------------------------------------------------------------------
ofp = 'C:\\SolBeePop_ecotox\\Model_Analysis\\Sensitivity_analysis_effects\\Nomia\\'  # output file path
folder_eff = 'sims_Nomia_SA_GUTS_IT_Mar2023\\' # folder name with simulation outputs from effect simulations
sim_set_name_eff = 'run_Nomia_GUTS_IT_Mar2023' # file name listing simulation setup of effect simulations
folder_contr = 'sims_Nomia_SA_control_Mar2023\\' # folder name with simulation outputs from control simulations
sim_set_name_contr = 'run_Nomia_control_Mar2023' # file name listing simulation setup of control simulations

# LHC vars
# For GUTS-SD, list GUTS parameters only in lv2
lv1 = c('var.emerge.f', 't.maturation', 'max.nesting.life',  
  'p.max.nesting.life', 'max.f.ratio', 'max.cells', 'max.survival.e.f',
  'emerged.survival', 'a.cell.age', 'a.sex.age', 'a.size.age',
  'a.cell.resource', 'a.sex.resource', 'a.size.resource',
  'ad.nectar.cons','ad.pollen.cons','ad.ET','TC_soil',
  'kd_IT','mw_IT','Fs_IT', # GUTS-IT parameters: do not include for GUTS-SD
  'nectar_prop','weight.prov', 'dr.intercept', 'dr.slope')#, 'SM') # only included for Osmia
# Only use for GUTS-SD:
lv2 = c('var.emerge.f', 't.maturation', 'max.nesting.life',  
        'p.max.nesting.life', 'max.f.ratio', 'max.cells', 'max.survival.e.f',
        'emerged.survival', 'a.cell.age', 'a.sex.age', 'a.size.age',
        'a.cell.resource', 'a.sex.resource', 'a.size.resource',
        'ad.nectar.cons','ad.pollen.cons','ad.ET','TC_soil',
        'kd_SD','bw_SD','mw_SD',
        'nectar_prop','weight.prov', 'dr.intercept', 'dr.slope', 'SM') # only included for Osmia

# Note: control simulations conducted with GUTS-SD and input files using '0' for all concentration time series

## Get Data  ------------------------------------------------------------------
setwd(ofp)

odf_eff = read.csv(paste0(folder_eff,'outputs_',sim_set_name_eff,'.csv'))
for(i in lv1){ # round values of input variables needed to merge data frames
  odf_eff[lv1] <- round(odf_eff[lv1],6) 
}
  
odf_c = read.csv(paste0(folder_contr,'outputs_',sim_set_name_contr,'.csv'))
for(i in lv1){ # round values of input variables needed to merge data frames
  odf_c[lv1] <- round(odf_c[lv1],6) 
}

odf <- merge(odf_c, odf_eff, by=c('name','RndSeed',lv1))

# Calculate relative effect sizes using control ('x') and effect ('y') simulation outputs ------
odf$bees.emerged.yr.R <- (odf$bees.emerged.yr.y - odf$bees.emerged.yr.x)/odf$bees.emerged.yr.x
odf$f.emerged.yr.R <- (odf$f.emerged.yr.y - odf$f.emerged.yr.x)/odf$f.emerged.yr.x
odf$m.emerged.yr.R <- (odf$m.emerged.yr.y - odf$m.emerged.yr.x)/odf$m.emerged.yr.x
odf$bees.nesting.R <- (odf$bees.nesting.y - odf$bees.nesting.x)/odf$bees.nesting.x
odf$sum.cells.R <- (odf$sum.cells.y - odf$sum.cells.x)/odf$sum.cells.x
odf$sum.f.cells.R <- (odf$sum.f.cells.y - odf$sum.f.cells.x)/odf$sum.f.cells.x
odf$sum.m.cells.R <- (odf$sum.m.cells.y - odf$sum.m.cells.x)/odf$sum.m.cells.x

ov_rel_eff = c('bees.emerged.yr.R', 'f.emerged.yr.R', 'm.emerged.yr.R',  
'bees.nesting.R', 'sum.cells.R', 'sum.f.cells.R', 'sum.m.cells.R',
'deaths.exp.in.y','deaths.exp.ad.y')
# GUTS-IT
#lv2 <- lv1
# GUTS-SD: keep SD-parameters only (discard IT parameters originating from control simulations)
odf2 <- odf[,c('name','RndSeed',lv2,ov_rel_eff)]

# Write relative effects to file 
write.csv(odf2, paste0('releffects_',sim_set_name_eff,'.csv'), row.names = FALSE)

## Calculate PRCC & Plot  -----------------------------------------------------
rdf = data.frame(var = lv2)

for(j in ov_rel_eff){  # per output measure
  tdf = pcc(X = odf2[lv2], y = odf2[,j], rank = TRUE)
  tdf = tdf$PRCC
  colnames(tdf)[1] = j
  tdf$var = rownames(tdf)
  rdf = merge(rdf, tdf)
  rm(tdf)
}
rm(j)

# Write PRCC values to file
write.csv(rdf, paste0('prcc_',sim_set_name_eff,'.csv'), row.names = FALSE)

## Script can be run from here if the prcc file had been generated previously
## Plot Data (table created in previous section) ------------------------------
##   Preliminary plots (not used in publication)
rdf <- read.csv(paste0('prcc_',sim_set_name_eff,'.csv'))
level_order <- c('var.emerge.f', 't.maturation', 'max.nesting.life', 'p.max.nesting.life',
                 'max.f.ratio', 'max.cells', 'max.survival.e.f', 'emerged.survival',
                 'a.cell.age', 'a.sex.age', 'a.size.age', 'a.cell.resource',
                 'a.sex.resource', 'a.size.resource', 
                 'ad.nectar.cons','ad.pollen.cons','ad.ET','TC_soil', 
                 'kd_IT','mw_IT','Fs_IT', # GUTS-IT parameters: change to GUTS-SD parameters
#                                  'kd_SD','mw_SD','bw_SD', # GUTS-IT parameters: change to GUTS-SD parameters
                 'nectar_prop','weight.prov', 'dr.intercept', 'dr.slope') # include SM only for Osmia

tdf = melt(rdf, id = 'var')

plt = ggplot(tdf) +
  geom_point(aes(x = value, y = factor(var, level=level_order))) +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_y_discrete(limits = rev) +
  theme_bw() + facet_grid(variable~.) +
  theme(axis.title = element_blank())
ggsave(paste0('prcc_',sim_set_name_eff,'.png'), plt, width = 6.5, height = 19.5)

## plots for single outputs
# nesting
nesting <- subset(tdf, tdf$variable == 'bees.nesting.R')
plt = ggplot(nesting) +
  geom_point(aes(x = value, y = factor(var, level=level_order), size = 8), show.legend = FALSE) +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_y_discrete(limits = rev) +
  theme_bw() + theme(text = element_text(size=16)) + 
  labs(x = "PRCC", y = "Model parameter") 
ggsave(paste0('nesting_prcc_',sim_set_name_eff,'.png'), plt, dpi=300, width = 6.5, height = 5)

# emergence
emerg <- subset(tdf, tdf$variable == 'bees.emerged.yr.R')
plt = ggplot(emerg) +
  geom_point(aes(x = value, y = factor(var, level=level_order), size = 8), show.legend = FALSE) +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_y_discrete(limits = rev) +
  theme_bw() + theme(text = element_text(size=16)) + 
  labs(x = "PRCC", y = "Model parameter") 
ggsave(paste0('emerged_prcc_',sim_set_name_eff,'.png'), plt, dpi=300, width = 6.5, height = 5)


