## SolBeePop_ecotox
## Combine plots
## from scripts DIM_effects_Osmia2019_spraysce_absolute_Dec2023.R and
##   DIM_effects_Osmia2021_spraysce_absolute_Dec2023.R
### data from one script is overwritten by the other except the prepared plots
## This script generates a figure panel with 4 plots

ofp = 'C:/SolBeePop_ecotox/Calibration_Validation_Effects/'  # output file path

source("C:/SolBeePop_ecotox/Calibration_Validation_Effects/Effect_Simulations/DIM_effects_Osmia2021_spraysce_absolute_Dec2023.R")
source("C:/SolBeePop_ecotox/Calibration_Validation_Effects/Effect_Simulations/DIM_effects_Osmia2019_spraysce_absolute_Dec2023.R")

p_nest1 <- ggplotGrob(plot_nest1)
p_brood1 <- ggplotGrob(plot_brood1)
p_nest2 <- ggplotGrob(plot_nest2)
p_brood2 <- ggplotGrob(plot_brood2)
combi1 <- rbind(p_nest1,p_brood1, size = "last")
combi2 <- rbind(p_nest2,p_brood2, size = "last")
combi <- cbind(combi2, combi1)
ggsave("MS_fig_Eurofins_Osmia_spraysce.jpg",combi, dpi=300, width = 14, height = 11)
