## SolBeePop_ecotox
## Combine plots of control calibration or validation
## Uses scripts "Plot_calib_Eurofins2021.R" and "Plot_calib_Eurofins2019.R"
### data from one script is overwritten by the other except the prepared plots
## This script generates a figure panel with 4 plots
## Author: Amelie Schmolke
## last changed: 20.12.2023

ofp = 'C:/SolBeePop_ecotox/Calibration_Validation_Effects/Control_Calib_Valid/'  # output file path

source("C:/SolBeePop_ecotox/Calibration_Validation_Effects/Control_Calib_Valid/Plot_calib_Eurofins2021.R")
source("C:/SolBeePop_ecotox/Calibration_Validation_Effects/Control_Calib_Valid/Plot_calib_Eurofins2019.R")

p_nest1 <- ggplotGrob(study_nest1)
p_brood1 <- ggplotGrob(study_brood1)
p_nest2 <- ggplotGrob(study_nest2)
p_brood2 <- ggplotGrob(study_brood2)
combi1 <- rbind(p_nest1,p_brood1, size = "last")
combi2 <- rbind(p_nest2,p_brood2, size = "last")
combi <- cbind(combi2, combi1)
ggsave("MS_fig_Eurofins_Osmia_calib.jpg",combi, dpi=300, width = 14, height = 11)
