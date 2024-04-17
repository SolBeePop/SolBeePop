## SolBeePop_ecotox
## Simulation of Eurofins Osmia semi-field study 1 (2019)
## Author: Amelie Schmolke 
## last changed: 13.03.2024

#rm(list = ls()) # clean up workspace # cannot be used if figure panel is generated with script "Eurofins_calib_Osmia_MS_plot.R"

library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggsci)
library(stringr)
library(gridExtra)

setwd("C:/SolBeePop_ecotox/Calibration_Validation_Effects/Control_Calib_Valid/")
# reading in simulation information
run_num <- 1310 # run number, simulation number from LHC; 951: best fit to 2021, 1310: best fit to 2019
study_year <- 2019

study_ID <- 1 # additional study identifier: 2019 - 1, 2021 - 2, set automatically
if(study_year==2021){study_ID <- 2} 

# Read in semi-field data, study Eurofins 2019 -------------------------------------------------
study_info<-read_csv(paste0("C:/SolBeePop_ecotox/Osmia_semi_field_data/Study_Treatments_Eurofins_Study",study_ID,"_",study_year,".csv"))
# getting the start and end date to limit the x-axis in plots
start<-mdy(study_info$Introduction[1])
end<-mdy(study_info$Last.day[1])

# reading in nest occupation summary table
NestOcc<-read_csv(paste0("C:/SolBeePop_ecotox/Osmia_semi_field_data/NestOccup_Eurofins_Study",study_ID,"_",study_year,".csv"))
# summing data
NestOcc_summarize<-NestOcc %>%
  filter(treatment %in% c('Ca', 'Cb', 'Cc','Cd', 'Ce', 'Cf')) %>% # !!! controls only !!!
  group_by(date, treatment) %>% 
  summarise(female.sum = sum(female)) %>% 
  mutate(date =mdy(date))

# reading in cell production summary table
cellProd<-read_csv(paste0("C:/SolBeePop_ecotox/Osmia_semi_field_data/CellProd_Eurofins_Study",study_ID,"_",study_year,".csv"))
# summing cell production data for control groups by group and date
cellProd_summarize<-cellProd %>% 
  filter(treatment %in% c('Ca', 'Cb', 'Cc','Cd', 'Ce', 'Cf')) %>% # !!! controls only !!!
  group_by(date, treatment) %>% 
  mutate(date =mdy(date))

#----------- read in simulation outputs
best_run<-read_csv(paste0("Eurofins",study_year,"_",run_num,".csv"), skip=6)

#fixing date
cal_mod <- best_run %>% 
  mutate(day = str_split_fixed(DateREP, " ",4)[,1],
         month = str_split_fixed(DateREP, " ",4)[,3],
         year=as.numeric(str_split_fixed(DateREP, " ",4)[,4])+(study_year-1),
         Date=dmy(paste0(day,"/",month,"/", year)))

colnames(cal_mod)[colnames(cal_mod) == 'count turtles with [ life.stage = "emerged" AND sex = "female"]'] <- 'f.prenesting'
#cal_mod$f.sex.ratio <- cal_mod$sum.f.cells.today/cal_mod$sum.cells.today

# average value of 10 repetitions 
cal_summarize<-cal_mod %>% 
  #  left_join(cal_emergence[,c("[run number]", "female.sum", "male.sum", "sex.ratio")], by ="[run number]") %>% 
  group_by(var.emerge.f, t.maturation, p.max.nesting.life, max.f.ratio, max.cells, max.survival.e.f, Date) %>% 
  summarize(avg.bees.nesting.today=mean(bees.nesting.today),
            min.bees.nesting.today=min(bees.nesting.today),
            max.bees.nesting.today=max(bees.nesting.today),
            avg.postemerg.f.today=mean(bees.nesting.today+f.prenesting),
            min.postemerg.f.today=min(bees.nesting.today+f.prenesting),
            max.postemerg.f.today=max(bees.nesting.today+f.prenesting),
            avg.sum.cells=mean(sum.cells),
            min.sum.cells=min(sum.cells),
            max.sum.cells=max(sum.cells),
            avg.sum.cells.today=mean(sum.cells.today),
            min.sum.cells.today=min(sum.cells.today),
            max.sum.cells.today=max(sum.cells.today),
            avg.sum.f.cells.today=mean(sum.f.cells.today),
            min.sum.f.cells.today=min(sum.f.cells.today),
            max.sum.f.cells.today=max(sum.f.cells.today))

# adjust year to study year
#cal_summarize$Date_mod<-cal_summarize$Date+years(study_year-2001)

#write_csv(cal_summarize,paste0(study_name1,"/Calibration/",study_name2,"_",sim_set_name,"_run",sim_analysis_best_ID,"_plotdata.csv" ))

sim_data <- cal_summarize


# plotting data -----------------------------------------------------------
study_nest2<-ggplot()+
  #geom_line(data=K612_sim1_NO, aes(x= mdy(`DateREP`)+years(16), y=bees.nesting.today),color ="black", size=1)+
  geom_line(data=sim_data, aes(Date, y=avg.postemerg.f.today),color ="black", linewidth=1)+
  geom_line(data=sim_data, aes(Date, y=min.postemerg.f.today),color ="gray", linewidth=1)+
  geom_line(data=sim_data, aes(Date, y=max.postemerg.f.today),color ="gray", linewidth=1)+
  geom_point(data=NestOcc_summarize, aes(date, female.sum, shape =treatment ), size=3, color ="black")+
  xlim(start, end)+
  ylim(0,65)+ # adjust y-axis limits for each sim set and study
  xlab("Date") + ylab("Number of females in nest")+
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 11))+
  theme_classic()+
  theme(text=element_text(size=24),axis.text=element_text(size=20),
        axis.title=element_text(size=24,face="bold"),
        axis.title.x=element_blank(),
        strip.text.x = element_text(size=24, face = "bold"),
        plot.tag = element_text(size=24, face = "bold")) +
  theme(legend.position="none") +
  labs(tag = "A")
#ggsave(paste0('NestOcc2_Eurofins',study_year,'_',run_num,'_Oct2023.jpg'), study_nest2, dpi=300, width = 7, height = 5)

study_brood2<-ggplot()+
  geom_line(data=sim_data, aes(x= Date, y=avg.sum.cells), color ="black", linewidth=1)+
  geom_line(data=sim_data, aes(x= Date, y=min.sum.cells), color ="gray", linewidth=1)+
  geom_line(data=sim_data, aes(x= Date, y=max.sum.cells), color ="gray", linewidth=1)+
  geom_point(data=cellProd_summarize, aes(date, cum.cells, shape =treatment), size=3, color ="black")+
  xlim(start, end)+ ylim(0,350)+ # adjust y-axis limits for each sim set and study
  xlab("Date") + ylab("Number of brood cells")+
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 11))+
  theme_classic()+
  theme(text=element_text(size=24),axis.text=element_text(size=20),
        axis.title=element_text(size=24,face="bold"),
        #        axis.title.x=element_blank(),
        strip.text.x = element_text(size=24, face = "bold"),
        plot.tag = element_text(size=24, face = "bold")) +
  theme(legend.position="none") +
  labs(tag = "B")
#ggsave(paste0('Brood_Eurofins',study_year,'_',run_num,'_Oct2023.jpg'), study_brood, dpi=300, width = 7, height = 5)

