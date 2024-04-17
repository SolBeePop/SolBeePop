### SolBeePop_ecotox ####################################################
### Model analysis 
### Script for plots of PRCC results combined for Osmia and Nomia simulations
###  script requires that files "prcc_" have been generated (using script "sa_effects_prcc_Mar2023.R")
###  Plots included in TRACE documentation
###  PRCC calculated using the relative effect sizes
### Author: Amelie Schmolke 
### Date: March 2023
###################################################################

library(reshape2)
library(ggplot2)
library(plyr)
library(dplyr)

rm(list = ls()) # clean up workspace

## Inputs  --------------------------------------------------------------------
ofp = 'C:\\SolBeePop_ecotox\\Model_Analysis\\Sensitivity_analysis_effects\\'  # output file path
setwd(ofp)
folder_Osmia = 'Osmia\\'
sim_set_name_Osmia = 'run_Osmia_GUTS_Sd_Mar2023'
folder_Nomia = 'Nomia\\'
sim_set_name_Nomia = 'run_Nomia_GUTS_SD_Mar2023'

## read PRCC results  ---------------------------------------------------------------
# Osmia
rdf_Osmia = read.csv(paste0(folder_Osmia,'prcc_',sim_set_name_Osmia,'.csv'))
rdf_Osmia <- rdf_Osmia %>%
            select(var,bees.emerged.yr.R,bees.nesting.R)
tdf_Osmia = melt(rdf_Osmia, id = 'var')
tdf_Osmia$species = "Osmia"

# Nomia
rdf_Nomia = read.csv(paste0(folder_Nomia,'prcc_',sim_set_name_Nomia,'.csv'))
rdf_Nomia <- rdf_Nomia %>%
  select(var,bees.emerged.yr.R,bees.nesting.R)
tdf_Nomia = melt(rdf_Nomia, id = 'var')
tdf_Nomia$species = "Nomia"

TRACE_fig = rbind(tdf_Osmia,tdf_Nomia)

grid.names <- as_labeller(c('bees.emerged.yr.R' = "Rel. effect on bee emergence", 
                            'bees.nesting.R' = "Rel. effect on nesting bees",
                            'Osmia' = "O. bicornis", 'Nomia' = "N. melanderi"))

## Plot bees.emerged.yr separately
plt = ggplot(TRACE_fig) +
  geom_point(aes(x = value, y = var)) +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_y_discrete(limits = rev) +
  theme_bw() + facet_grid(cols = vars(variable), rows = vars(species), labeller = grid.names) +
  xlab("PRCC") + ylab(element_blank()) +
  theme(text = element_text(size = 16),
        strip.text.y = element_text(face = "italic"))     
#  theme(axis.title = element_blank())
#ggsave(paste0('UA_eff_prcc_GUTS_IT_Mar2023.pdf'), plt, device = "pdf", width = 9, height = 6, dpi=300)
ggsave(paste0('SA_eff_prcc_GUTS_SD_Mar2023.jpg'), plt, device = "jpeg", width = 9, height = 10, dpi=300)

