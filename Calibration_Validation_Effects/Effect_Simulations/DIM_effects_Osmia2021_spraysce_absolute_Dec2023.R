## SolBeePop_ecotox
## Plot controls and treatments from simulations and study data
## Plot two (or multiple) treatment simulations 
## Offspring sex ratio by date NOT calculated or plotted
## Author: Amelie Schmolke 
## Last Edited: 19 Dec 2023

## Overview of script
# 1. Definitions of analysis (paths, files etc.)
# 2. Read in model outputs (controls and 2 treatments) 
# 3. Read in study data 
# 4. Plots

library(reshape2)
library(ggplot2)
library(plyr)
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
library(ggsci)
library(stringr)
library(gridExtra)
library(gtable)

#rm(list = ls()) # clean up workspace

source("C:/SolBeePop_ecotox/Calibration_Validation_Effects/Effect_Simulations/df_avg_function.R")

## 1. Definitions of analysis  --------------------------------------------------------------------
ofp = 'C:/SolBeePop_ecotox/Calibration_Validation_Effects/'  # output file path
folder_eff = 'Effect_Simulations/' # outputs from treatment simulations
sim_set_name_eff1 = '951_2021_SD_med'
sim_set_name_eff2 = '951_2021_SD_med_75p'
sim_set_name_eff3 = '951_2021_SD_med_50p'
sim_set_name_eff4 = '951_2021_SD_med_25p'
sim_set_name_eff5 = '951_2021_SD_med_0p'
folder_contr = 'Control_Calib_Valid/' # outputs from control simulations
sim_set_name_contr = '951'

# specifications of study data
folder_data = 'C:/SolBeePop_ecotox/Osmia_semi_field_data/'
study = 'Eurofins_Study2_2021'
study_year = 2021

setwd(ofp)

## read in and process simulation outputs
# LHC vars
# For GUTS-SD, list GUTS parameters only in lv2
lv1 = c('day.emerge.f', 'var.emerge.f', 't.maturation', 'max.nesting.life',  
  'p.max.nesting.life', 'max.f.ratio', 'max.cells', 'max.survival.e.f',
  'emerged.survival', 'a.sex.age') 

## 2. Read in model outputs  ------------------------------------------------------------------

# treatment simulations (simulation 1)
odf_eff1 = read.csv(paste0(folder_eff,sim_set_name_eff1,'.csv'), stringsAsFactors = FALSE, skip=6)
for(i in lv1){ # round values of input variables needed to merge data frames
  odf_eff1[lv1] <- round(odf_eff1[lv1],6) 
}
colnames(odf_eff1)[colnames(odf_eff1) == 'count.turtles.with...life.stage....emerged..AND.sex....female..'] <- 'f.prenesting'
odf_eff1 <- odf_eff1 %>% 
  mutate(day = str_split_fixed(DateREP, " ",4)[,1],
         month = str_split_fixed(DateREP, " ",4)[,3],
         year=as.numeric(str_split_fixed(DateREP, " ",4)[,4])+(study_year-1),
         date=dmy(paste0(day,"/",month,"/", year)))
# correct date in simulations: measures in the study taken during the day (and prior to application on DOA);
#  simulated numbers reflect end of the day 
odf_eff1$date_corr <- as.Date(odf_eff1$date) + 1

# calculate mean, min and max and write to new data frame (using function from script "df_avg_function.R")
odf_eff1_avg<-f_df_avg(odf_eff1)

# treatment simulations (simulation 2)
odf_eff2 = read.csv(paste0(folder_eff,sim_set_name_eff2,'.csv'), stringsAsFactors = FALSE, skip=6)
for(i in lv1){ # round values of input variables needed to merge data frames
  odf_eff2[lv1] <- round(odf_eff2[lv1],6) 
}
colnames(odf_eff2)[colnames(odf_eff2) == 'count.turtles.with...life.stage....emerged..AND.sex....female..'] <- 'f.prenesting'
odf_eff2 <- odf_eff2 %>% 
  mutate(day = str_split_fixed(DateREP, " ",4)[,1],
         month = str_split_fixed(DateREP, " ",4)[,3],
         year=as.numeric(str_split_fixed(DateREP, " ",4)[,4])+(study_year-1),
         date=dmy(paste0(day,"/",month,"/", year)))
# correct date in simulations: measures in the study taken during the day (and prior to application on DOA);
#  simulated numbers reflect end of the day 
odf_eff2$date_corr <- as.Date(odf_eff2$date) + 1

# calculate mean, min and max and write to new data frame (using function from script "df_avg_function.R")
odf_eff2_avg<-f_df_avg(odf_eff2)

# treatment simulations (simulation 3)
odf_eff3 = read.csv(paste0(folder_eff,sim_set_name_eff3,'.csv'), stringsAsFactors = FALSE, skip=6)
for(i in lv1){ # round values of input variables needed to merge data frames
  odf_eff3[lv1] <- round(odf_eff3[lv1],6) 
}
colnames(odf_eff3)[colnames(odf_eff3) == 'count.turtles.with...life.stage....emerged..AND.sex....female..'] <- 'f.prenesting'
odf_eff3 <- odf_eff3 %>% 
  mutate(day = str_split_fixed(DateREP, " ",4)[,1],
         month = str_split_fixed(DateREP, " ",4)[,3],
         year=as.numeric(str_split_fixed(DateREP, " ",4)[,4])+(study_year-1),
         date=dmy(paste0(day,"/",month,"/", year)))
# correct date in simulations: measures in the study taken during the day (and prior to application on DOA);
#  simulated numbers reflect end of the day 
odf_eff3$date_corr <- as.Date(odf_eff3$date) + 1

# calculate mean, min and max and write to new data frame (using function from script "df_avg_function.R")
odf_eff3_avg<-f_df_avg(odf_eff3)

# treatment simulations (simulation 4)
odf_eff4 = read.csv(paste0(folder_eff,sim_set_name_eff4,'.csv'), stringsAsFactors = FALSE, skip=6)
for(i in lv1){ # round values of input variables needed to merge data frames
  odf_eff4[lv1] <- round(odf_eff4[lv1],6) 
}
colnames(odf_eff4)[colnames(odf_eff4) == 'count.turtles.with...life.stage....emerged..AND.sex....female..'] <- 'f.prenesting'
odf_eff4 <- odf_eff4 %>% 
  mutate(day = str_split_fixed(DateREP, " ",4)[,1],
         month = str_split_fixed(DateREP, " ",4)[,3],
         year=as.numeric(str_split_fixed(DateREP, " ",4)[,4])+(study_year-1),
         date=dmy(paste0(day,"/",month,"/", year)))
# correct date in simulations: measures in the study taken during the day (and prior to application on DOA);
#  simulated numbers reflect end of the day 
odf_eff4$date_corr <- as.Date(odf_eff4$date) + 1

# calculate mean, min and max and write to new data frame (using function from script "df_avg_function.R")
odf_eff4_avg<-f_df_avg(odf_eff4)

# treatment simulations (simulation 5)
odf_eff5 = read.csv(paste0(folder_eff,sim_set_name_eff5,'.csv'), stringsAsFactors = FALSE, skip=6)
for(i in lv1){ # round values of input variables needed to merge data frames
  odf_eff5[lv1] <- round(odf_eff5[lv1],6) 
}
colnames(odf_eff5)[colnames(odf_eff5) == 'count.turtles.with...life.stage....emerged..AND.sex....female..'] <- 'f.prenesting'
odf_eff5 <- odf_eff5 %>% 
  mutate(day = str_split_fixed(DateREP, " ",4)[,1],
         month = str_split_fixed(DateREP, " ",4)[,3],
         year=as.numeric(str_split_fixed(DateREP, " ",4)[,4])+(study_year-1),
         date=dmy(paste0(day,"/",month,"/", year)))
# correct date in simulations: measures in the study taken during the day (and prior to application on DOA);
#  simulated numbers reflect end of the day 
odf_eff5$date_corr <- as.Date(odf_eff5$date) + 1

# calculate mean, min and max and write to new data frame (using function from script "df_avg_function.R")
odf_eff5_avg<-f_df_avg(odf_eff5)


# control simulations
odf_c = read.csv(paste0(folder_contr,sim_set_name_contr,'.csv'), stringsAsFactors = FALSE, skip=6)
for(i in lv1){ # round values of input variables needed to merge data frames
  odf_c[lv1] <- round(odf_c[lv1],6) 
}
colnames(odf_c)[colnames(odf_c) == 'count.turtles.with...life.stage....emerged..AND.sex....female..'] <- 'f.prenesting'
#colnames(odf_c)[colnames(odf_c) == 'count.turtles.with...life.stage....emerged..AND.sex....female...'] <- 'f.prenesting'

odf_c <- odf_c %>% 
  mutate(day = str_split_fixed(DateREP, " ",4)[,1],
         month = str_split_fixed(DateREP, " ",4)[,3],
         year=as.numeric(str_split_fixed(DateREP, " ",4)[,4])+(study_year-1),
         date=dmy(paste0(day,"/",month,"/", year)))
# correct date in simulations: measures in the study taken during the day (and prior to application on DOA);
#  simulated numbers reflect end of the day 
odf_c$date_corr <- as.Date(odf_c$date) + 1

odf_c_avg <- f_df_avg(odf_c) 


#### 3. Read in study data and calculate relative effects ---------------------------
# getting the start and end date to limit the x-axis in plots
study_info<-read.csv(paste0(folder_data,"Study_Treatments_",study,".csv"))
study_start<-mdy(study_info$Introduction[1])
study_end<-mdy(study_info$Last.day[1])
study_appl<-mdy(study_info$Application.date[1])

## Nest occupation
study_nestocc <- read.csv(paste0(folder_data,"NestOccup_",study,".csv"), stringsAsFactors = FALSE)
# adjust date format
study_nestocc <- study_nestocc %>%
  group_by(date, treatment) %>% 
  mutate(date = mdy(date))

study_nestocc <- study_nestocc %>%
  group_by(date, treatment) %>% 
  summarise(female.sum = sum(female)) 

# calculating average between replicates
study_nestocc_DIM_avg<-study_nestocc %>% 
  filter(treatment %in% c("Ra","Rb","Rc","Rd")) %>% # !!! treatments only !!! mind that there is Rd in the 2021 study
  #  mutate(overall.avg = mean(female.sum, na.rm=TRUE)) %>% 
  group_by(date) %>% 
  summarize(female.sum.avg = mean(female.sum))

## Brood cell production
# reading in cell production summary table
study_cellProd <- read_csv(paste0(folder_data,"CellProd_",study,".csv"))
# summing cell production data for control groups by group and date
study_cellProd <- study_cellProd %>% 
  group_by(date, treatment) %>% 
  mutate(date = mdy(date))

# calculating average between replicates
study_cellProd_DIM_avg <- study_cellProd %>%
  filter(treatment %in% c("Ra","Rb","Rc","Rd")) %>% # !!! treatments only !!! mind that there is Rd in the 2021 study
  group_by(date) %>%
  summarise(cum.cells.avg = mean(cum.cells)) 

## plot control and treatment sims and data 
study_nestocc$treatment_group <- "control"
study_nestocc$treatment_group[study_nestocc$treatment == "Ra"] <- "dimethoate"
study_nestocc$treatment_group[study_nestocc$treatment == "Rb"] <- "dimethoate"
study_nestocc$treatment_group[study_nestocc$treatment == "Rc"] <- "dimethoate"
study_nestocc$treatment_group[study_nestocc$treatment == "Rd"] <- "dimethoate"

study_cellProd$treatment_group <- "control"
study_cellProd$treatment_group[study_cellProd$treatment == "Ra"] <- "dimethoate"
study_cellProd$treatment_group[study_cellProd$treatment == "Rb"] <- "dimethoate"
study_cellProd$treatment_group[study_cellProd$treatment == "Rc"] <- "dimethoate"
study_cellProd$treatment_group[study_cellProd$treatment == "Rd"] <- "dimethoate"

## plots combined to one figure, title removed
plot_nest1<-ggplot()+
  geom_point(data=study_nestocc, aes(date, female.sum, group=treatment_group,color=factor(treatment_group),
                                     shape=factor(treatment)), size=2)+
  geom_line(data=odf_c_avg, aes(date_corr, y=avg.f.postem.today), linewidth=1.25,color ="black")+
  geom_line(data=odf_c_avg, aes(date_corr, y=min.f.postem.today), linewidth=0.75,color ="gray")+
  geom_line(data=odf_c_avg, aes(date_corr, y=max.f.postem.today), linewidth=0.75,color ="gray")+
  geom_line(data=odf_eff5_avg, aes(date_corr, y=avg.f.postem.today), linewidth=1.25,color ="green3", linetype = "longdash")+
  #  geom_line(data=odf_eff5_avg, aes(date_corr, y=min.f.postem.today), linewidth=0.5,color ="paleturquoise3", linetype = "dashed")+
  #  geom_line(data=odf_eff5_avg, aes(date_corr, y=max.f.postem.today), linewidth=0.5,color ="paleturquoise3", linetype = "dashed")+
  geom_line(data=odf_eff4_avg, aes(date_corr, y=avg.f.postem.today), linewidth=1.25,color ="goldenrod3", linetype = "dotdash")+
  #  geom_line(data=odf_eff4_avg, aes(date_corr, y=min.f.postem.today), linewidth=0.5,color ="palegreen1", linetype = "dashed")+
  #  geom_line(data=odf_eff4_avg, aes(date_corr, y=max.f.postem.today), linewidth=0.5,color ="palegreen1", linetype = "dashed")+
  geom_line(data=odf_eff3_avg, aes(date_corr, y=avg.f.postem.today), linewidth=1.25,color ="magenta3", linetype = "longdash")+
  #  geom_line(data=odf_eff3_avg, aes(date_corr, y=min.f.postem.today), linewidth=0.5,color ="orchid2", linetype = "dashed")+
  #  geom_line(data=odf_eff3_avg, aes(date_corr, y=max.f.postem.today), linewidth=0.5,color ="orchid2", linetype = "dashed")+
  geom_line(data=odf_eff2_avg, aes(date_corr, y=avg.f.postem.today), linewidth=1.25,color ="orangered", linetype = "dotdash")+
  #  geom_line(data=odf_eff2_avg, aes(date_corr, y=min.f.postem.today), linewidth=0.5,color ="#E69F00", linetype = "dashed")+
  #  geom_line(data=odf_eff2_avg, aes(date_corr, y=max.f.postem.today), linewidth=0.5,color ="#E69F00", linetype = "dashed")+
  geom_line(data=odf_eff1_avg, aes(date_corr, y=avg.f.postem.today), linewidth=1.25,color ="blue")+
  geom_line(data=odf_eff1_avg, aes(date_corr, y=min.f.postem.today), linewidth=0.75,color ="lightblue")+
  geom_line(data=odf_eff1_avg, aes(date_corr, y=max.f.postem.today), linewidth=0.75,color ="lightblue")+
#  geom_vline(xintercept = odf_c_avg$date_corr[12]+0.5, linewidth = 0.6, linetype = "dotted", color = "black")+ # for 2019
  geom_vline(xintercept = odf_c_avg$date_corr[16]+0.5, linewidth = 0.75, linetype = "dotted", color = "black")+ # for 2021
  #  xlim(study_start, study_end)+  ylim(0,65)+ # for 2019
  xlim(study_start, study_end)+  ylim(0,90)+ # for 2021
  xlab("Date") + ylab("# adult females in nest")+
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 11, 15, 16, 17,18), guide="none")+
  scale_color_manual(name=element_blank(), 
                     values = c("black","blue"))+
  theme_classic()+
  theme(text=element_text(size=24),axis.text=element_text(size=20),
        axis.title=element_text(size=24,face="bold"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        strip.text.x = element_text(size=24, face = "bold"),
        plot.tag = element_text(size=24, face = "bold")) +
  theme(legend.position="none") +
  labs(tag = "C")

## plot brood cell production data
plot_brood1<-ggplot()+
  geom_point(data=study_cellProd, aes(date, cum.cells, group=treatment_group,color=factor(treatment_group),
                                        shape=factor(treatment)), size=2)+
  geom_line(data=odf_c_avg, aes(x= date_corr, y=avg.sum.cells), color ="black", linewidth=1.25)+
  geom_line(data=odf_c_avg, aes(x= date_corr, y=min.sum.cells), color ="gray", linewidth=0.75)+
  geom_line(data=odf_c_avg, aes(x= date_corr, y=max.sum.cells), color ="gray", linewidth=1)+
  geom_line(data=odf_eff5_avg, aes(x= date_corr, y=avg.sum.cells), color ="green3", linetype = "longdash",linewidth=1.25)+
  geom_line(data=odf_eff4_avg, aes(x= date_corr, y=avg.sum.cells), color ="goldenrod3", linetype = "dotdash",linewidth=1.25)+
  geom_line(data=odf_eff3_avg, aes(x= date_corr, y=avg.sum.cells), color ="magenta3", linetype = "longdash",linewidth=1.25)+
  geom_line(data=odf_eff2_avg, aes(x= date_corr, y=avg.sum.cells), color ="orangered", linetype = "dotdash",linewidth=1.25)+
  #  geom_line(data=odf_eff2_avg, aes(x= date_corr, y=min.sum.cells), color ="#E69F00", linewidth=1)+
  #  geom_line(data=odf_eff2_avg, aes(x= date_corr, y=max.sum.cells), color ="#E69F00", linewidth=1)+
  geom_line(data=odf_eff1_avg, aes(x= date_corr, y=avg.sum.cells), color ="blue", linewidth=1.25)+
  geom_line(data=odf_eff1_avg, aes(x= date_corr, y=min.sum.cells), color ="lightblue", linewidth=0.75)+
  geom_line(data=odf_eff1_avg, aes(x= date_corr, y=max.sum.cells), color ="lightblue", linewidth=0.75)+
#  geom_vline(xintercept = odf_c_avg$date_corr[12]+0.5, linewidth = 0.5, linetype = "dotted", color = "black")+ # for 2019
  geom_vline(xintercept = odf_c_avg$date_corr[16]+0.5, linewidth = 0.75, linetype = "dotted", color = "black")+ # for 2021
  #  xlim(study_start, study_end)+ ylim(0,350)+ # for 2019
  xlim(study_start, study_end)+ ylim(0,900)+ # for 2021
  xlab("Date") + ylab("# brood cells")+
  scale_shape_manual(values = c(0, 1, 2, 5, 6, 11, 15, 16, 17,18), guide="none")+
  scale_color_manual(name=element_blank(), 
                     values = c("black","blue"))+
  theme_classic()+
  theme(text=element_text(size=24),axis.text=element_text(size=20),
        axis.title=element_text(size=24,face="bold"),
        axis.title.y=element_blank(),
        strip.text.x = element_text(size=24, face = "bold"),
        plot.tag = element_text(size=24, face = "bold")) +
  theme(legend.position='none') +
  labs(tag = "D")
  #theme(legend.position='bottom', legend.text=element_text(size=24))
