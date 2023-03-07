### SolBeePop ####################################################
### Model analysis
### Script for the plotting PRCC results from the model analysis for Osmia and Nomia (manuscript figure) 
### Author: Amelie Schmolke 
### Date: February 2023
###################################################################

rm(list = ls()) # clean up workspace
library(reshape2)
library(ggplot2)
library(plyr)
library(dplyr)

## Inputs  --------------------------------------------------------------------
ofp = 'C:\\SolBeePop\\Model_Analysis\\'  # output file path
setwd(ofp)
sim_set_name_Osmia = 'run_Osmia_1'
sim_set_name_Nomia = 'run_Nomia_1'

## read PRCC results  ---------------------------------------------------------------
# Osmia
rdf_Osmia = read.csv(paste0('prcc_',sim_set_name_Osmia,'.csv'))
rdf_Osmia <- rdf_Osmia %>%
            select(var,bees.emerged.yr,f.sex.ratio.emerged)
tdf_Osmia = melt(rdf_Osmia, id = 'var')
tdf_Osmia$species = "Osmia"

# Nomia
rdf_Nomia = read.csv(paste0('prcc_',sim_set_name_Nomia,'.csv'))
rdf_Nomia <- rdf_Nomia %>%
  select(var,bees.emerged.yr,f.sex.ratio.emerged)
tdf_Nomia = melt(rdf_Nomia, id = 'var')
tdf_Nomia$species = "Nomia"

MS1_fig = rbind(tdf_Osmia,tdf_Nomia)

grid.names <- as_labeller(c('bees.emerged.yr' = "# emerged bees", 
                            'f.sex.ratio.emerged' = "Sex ratio",
                            'Osmia' = "O. bicornis", 'Nomia' = "N. melanderi"))

## Plot 
plt = ggplot(MS1_fig) +
  geom_point(aes(x = value, y = var)) +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_y_discrete(limits = rev) +
  theme_bw() + facet_grid(cols = vars(variable), rows = vars(species), labeller = grid.names) +
  xlab("PRCC") + ylab(element_blank()) +
  theme(text = element_text(size = 16),
        strip.text.y = element_text(face = "italic"))     
ggsave(paste0('MS1_PRCC_28Feb2023.pdf'), plt, device = "pdf", width = 9, height = 6, dpi=300)
ggsave(paste0('MS1_PRCC_28Feb2023.jpg'), plt, device = "jpeg", width = 9, height = 6, dpi=300)

