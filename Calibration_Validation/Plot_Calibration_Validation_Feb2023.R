## Project 781.02 / 796.166
## Calibration to Ruddle semi-field data (Osmia); plots for MS1
## Author: Brenna Kent (changed by Amelie Schmolke, Jennifer Crider)
## last changed: 16 Feb 2023 (MS1 figure)
rm(list = ls()) # clean up workspace

library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggsci)
library(stringr)
library(gridExtra)
library(cowplot)

setwd("C:\\SolBeePop\\Calibration_Validation\\Validation")
## Set correct 'type': validation or calibration; used in output file name
#type <- "Validation"
type <- "Calibration"
study03_sim_data <- "S15_01803_latemat_run1510_plotdata.csv"
study04_sim_data <- "S15_01804_latemat_run1189_plotdata.csv"
#study03_sim_data <- "S15-01803/Validation/S15_01803_earlymat_runEarlymat_01804E_1_plotdata.csv"
#study04_sim_data <- "S15-01804/Validation/S15_01804_latemat_runLatemat_01803E_1_plotdata.csv"

# Read simulation outputs prepared for plotting
sim_data_03<-read_csv(study03_sim_data)
sim_data_04<-read_csv(study04_sim_data)

# Read in semi-field data, study S15-01803 -------------------------------------------------
study_info_03<-read_csv("C:/SolBeePop/Calibration_Validation/Calibration_Validation/S15-01803-01_Tables/Study_Treatments_S15-01803-01.csv")
# getting the start and end date to limit the x-axis in plots
start_03<-mdy(study_info_03$Introduction[1])
end_03<-mdy(study_info_03$Last.day[1])
# reading in nest occupation summary table
NO_03<-read_csv("C:/SolBeePop/Calibration_Validation/Calibration_Validation/S15-01803-01_Tables/NestOccup_S15-01803-01.csv")
# summing data
NO_03_summarize<-NO_03 %>%
  group_by(date, treatment) %>% 
  summarise(female.sum = sum(female)) %>% 
  mutate(date =ymd(date))
# reading in cell production summary table
cellProd_03<-read_csv("C:/SolBeePop/Calibration_Validation/Calibration_Validation/S15-01803-01_Tables/CellProd_S15-01803-01.csv")
# summing cell production data for control groups by group and date
cellProd_03_summarize<-cellProd_03 %>% 
  group_by(date, treatment) %>% 
  summarise(brood.sum = sum(sum.cells)) %>% 
  mutate(date =ymd(date))
cellProd_03_summarize$brood.sum.cum <-cellProd_03_summarize %>% 
  group_by(treatment) %>% 
  mutate(brood.sum.cum=cumsum(brood.sum))
### Note: emergence data not included in MS plots

# Read in semi-field data, study S15-01804 -------------------------------------------------
study_info_04<-read_csv("C:/SolBeePop/Calibration_Validation/Calibration_Validation/S15-01804-01_Tables/Study_Treatments_S15-01804-01.csv")
# getting the start and end date to limit the x-axis in plots
start_04<-mdy(study_info_04$Introduction[1])
end_04<-mdy(study_info_04$Last.day[1])
# reading in nest occupation summary table
NO_04<-read_csv("C:/SolBeePop/Calibration_Validation/Calibration_Validation/S15-01804-01_Tables/NestOccup_S15-01804-01.csv")
# summing data
NO_04_summarize<-NO_04 %>%
  group_by(date, treatment) %>% 
  summarise(female.sum = sum(female)) %>% 
  mutate(date =ymd(date))
# reading in cell production summary table
cellProd_04<-read_csv("C:/SolBeePop/Calibration_Validation/Calibration_Validation/S15-01804-01_Tables/CellProd_S15-01804-01.csv")
# summing cell production data for control groups by group and date
cellProd_04_summarize<-cellProd_04 %>% 
  group_by(date, treatment) %>% 
  summarise(brood.sum = sum(sum.cells)) %>% 
  mutate(date =ymd(date))
cellProd_04_summarize$brood.sum.cum <-cellProd_04_summarize %>% 
  group_by(treatment) %>% 
  mutate(brood.sum.cum=cumsum(brood.sum))
### Note: emergence data not included in MS plots

# plotting data -----------------------------------------------------------
study03_nest<-ggplot()+
    geom_line(data=sim_data_03, aes(Date_mod, y=avg.bees.nesting.today),color ="black", linewidth=1)+
    geom_line(data=sim_data_03, aes(Date_mod, y=min.bees.nesting.today),color ="gray", linewidth=1)+
    geom_line(data=sim_data_03, aes(Date_mod, y=max.bees.nesting.today),color ="gray", linewidth=1)+
    geom_point(data=NO_03_summarize, aes(date, female.sum, color =treatment, shape =treatment ), size=3)+
    xlim(start_03, end_03)+
    ylim(0,50)+
    xlab("Date") + ylab("# Nesting females")+
    scale_shape_manual(values = c(15, 16, 17, 18))+
  theme_classic()+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),
          strip.text.x = element_text(size = 12, face = "bold"),
        axis.title.x = element_blank()) +
    theme(legend.position='none')+
    scale_color_npg()
  
study03_brood<-ggplot()+
  geom_line(data=sim_data_03, aes(x= Date_mod, y=avg.sum.cells, lty='Model simulations'), color ="black", linewidth=1)+
  geom_line(data=sim_data_03, aes(x= Date_mod, y=min.sum.cells), color ="gray", linewidth=1)+
  geom_line(data=sim_data_03, aes(x= Date_mod, y=max.sum.cells), color ="gray", linewidth=1)+
  geom_point(data=cellProd_03_summarize$brood.sum.cum, aes(date, brood.sum.cum, color =treatment, shape =treatment), size=3)+
  xlim(start_03, end_03)+ ylim(0,500)+
  xlab("Date") + ylab("# Brood cells")+
  scale_shape_manual(values = c(15, 16, 17, 18))+
  theme_classic()+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),
        strip.text.x = element_text(size = 12, face = "bold")) +
  theme(legend.position='bottom',
        legend.title=element_blank(),
        legend.margin = margin(0,-0.16,0,-0.2, unit="cm"))+
  scale_color_npg()
  
study04_nest<-ggplot()+
  geom_line(data=sim_data_04, aes(Date_mod, y=avg.bees.nesting.today),color ="black", linewidth=1)+
  geom_line(data=sim_data_04, aes(Date_mod, y=min.bees.nesting.today),color ="gray", linewidth=1)+
  geom_line(data=sim_data_04, aes(Date_mod, y=max.bees.nesting.today),color ="gray", linewidth=1)+
  geom_point(data=NO_04_summarize, aes(date, female.sum, color =treatment, shape =treatment ), size=3)+
  xlim(start_04, end_04)+ ylim(0,50)+
  xlab("Date") + ylab("# Nesting females")+
  scale_shape_manual(values = c(15, 16, 17, 18))+
  theme_classic()+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),
        strip.text.x = element_text(size = 12, face = "bold"),
        axis.title.x = element_blank()) +
  theme(legend.position='none')+
  scale_color_npg()

study04_brood<-ggplot()+
  geom_line(data=sim_data_04, aes(x= Date_mod, y=avg.sum.cells, lty='Model simulations'), color ="black", linewidth=1)+
  geom_line(data=sim_data_04, aes(x= Date_mod, y=min.sum.cells), color ="gray", linewidth=1)+
  geom_line(data=sim_data_04, aes(x= Date_mod, y=max.sum.cells), color ="gray", linewidth=1)+
  geom_point(data=cellProd_04_summarize$brood.sum.cum, aes(date, brood.sum.cum, color =treatment, shape =treatment), size=3)+
  xlim(start_04, end_04)+ ylim(0,500)+
  xlab("Date") + ylab("# Brood cells")+
  scale_shape_manual(values = c(15, 16, 17, 18))+
  theme_classic()+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),
    strip.text.x = element_text(size = 12, face = "bold"))+
  theme(legend.position='bottom',
        legend.title=element_blank(),
        legend.margin = margin(0,-0.16,0,-.2, unit="cm"))+
  scale_color_npg()

plt<- plot_grid(study03_nest,study04_nest,study03_brood,study04_brood,labels = "AUTO", align = 'vh', ncol=2, rel_heights = c(1, 1.15))
ggsave(paste0(type,"_Fig_Feb2023.jpg"), plt, device="jpeg", width=9,height=9, dpi=300)
ggsave(paste0(type,"_Fig_Feb2023.pdf"), plt, device="pdf", width=9, height=9, dpi=300)
