## Plot controls and treatments from simulations of hypothetical semi-field studies (case study)
## plot multiple direct spray scenarios and species
##  left column: Nomia
##  middle column: Eucera
##  right column: Osmia
## plot of control (black), 100% exposure to direct spray (blue). 50% (magenta), 0% (orange)
## offspring emergence not included
## Project: SolBeePop_ecotox
## Author: Amelie Schmolke 
## Last Edited: 19 Dec 2023

## Overview of script
# 1. Simulation outputs species 1 (Nomia)
# 2. Simulation outputs species 2 (Eucera)
# 3. Simulation outputs species 3 (Osmia)
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

rm(list = ls()) # clean up workspace

source("C:/SolBeePop_ecotox/Hypothetical_Semi_Field/df_avg_function.R")

ofp = 'C:/SolBeePop_ecotox/Hypothetical_Semi_Field/'
setwd(ofp)

## read in and process simulation outputs
lv1 = c('day.emerge.f', 'var.emerge.f', 't.maturation', 'max.nesting.life',  
        'p.max.nesting.life', 'max.f.ratio', 'max.cells', 'max.survival.e.f',
        'emerged.survival', 'a.sex.age') 

## 1. Simulation outputs species 1 (Nomia)  --------------------------------------------------------------------
folder_name1 <- "Nomia/"
sim_set_name_eff1 = 'Nomia_semifield_SD_med'
sim_set_name_eff1b = 'Nomia_semifield_SD_med_50p'
sim_set_name_eff1c = 'Nomia_semifield_SD_med_0p'
sim_set_name_contr1 = 'Nomia_semifield_control'

study_year1 = 2003
DAA1 = ymd("2003-06-18")

# simulation settings
sim_settings1 <- read.csv(paste0(folder_name1,'run_semifield_Nomia_DIM.csv'))
sim_settings1 <- subset(sim_settings1, sim_settings1$name == sim_set_name_eff1)
start1 = sim_settings1$Start.day
end1 = sim_settings1$latest.emerge

# treatment simulations (100% direct spray)
odf_eff1 = read.csv(paste0(folder_name1,sim_set_name_eff1,'.csv'), stringsAsFactors = FALSE, skip=6)
for(i in lv1){ # round values of input variables needed to merge data frames
  odf_eff1[lv1] <- round(odf_eff1[lv1],6) 
}
colnames(odf_eff1)[colnames(odf_eff1) == 'count.turtles.with...life.stage....emerged..AND.sex....female..'] <- 'f.prenesting'
odf_eff1 <- odf_eff1 %>% 
  mutate(day = str_split_fixed(DateREP, " ",4)[,1],
         month = str_split_fixed(DateREP, " ",4)[,3],
         year=as.numeric(str_split_fixed(DateREP, " ",4)[,4])+(study_year1-1),
         date=dmy(paste0(day,"/",month,"/", year)))

odf_eff1_avg<-f_df_avg(odf_eff1) 
odf_eff1_avg$treatment <- "100p"

# treatment simulations (50% direct spray)
odf_eff1b = read.csv(paste0(folder_name1,sim_set_name_eff1b,'.csv'), stringsAsFactors = FALSE, skip=6)
for(i in lv1){ # round values of input variables needed to merge data frames
  odf_eff1b[lv1] <- round(odf_eff1b[lv1],6) 
}
colnames(odf_eff1b)[colnames(odf_eff1b) == 'count.turtles.with...life.stage....emerged..AND.sex....female..'] <- 'f.prenesting'
odf_eff1b <- odf_eff1b %>% 
  mutate(day = str_split_fixed(DateREP, " ",4)[,1],
         month = str_split_fixed(DateREP, " ",4)[,3],
         year=as.numeric(str_split_fixed(DateREP, " ",4)[,4])+(study_year1-1),
         date=dmy(paste0(day,"/",month,"/", year)))

odf_eff1b_avg<-f_df_avg(odf_eff1b) 
odf_eff1b_avg$treatment <- "50p"

# treatment simulations (0% direct spray)
odf_eff1c = read.csv(paste0(folder_name1,sim_set_name_eff1c,'.csv'), stringsAsFactors = FALSE, skip=6)
for(i in lv1){ # round values of input variables needed to merge data frames
  odf_eff1c[lv1] <- round(odf_eff1c[lv1],6) 
}
colnames(odf_eff1c)[colnames(odf_eff1c) == 'count.turtles.with...life.stage....emerged..AND.sex....female..'] <- 'f.prenesting'
odf_eff1c <- odf_eff1c %>% 
  mutate(day = str_split_fixed(DateREP, " ",4)[,1],
         month = str_split_fixed(DateREP, " ",4)[,3],
         year=as.numeric(str_split_fixed(DateREP, " ",4)[,4])+(study_year1-1),
         date=dmy(paste0(day,"/",month,"/", year)))

odf_eff1c_avg<-f_df_avg(odf_eff1c) 
odf_eff1c_avg$treatment <- "0p"

# control simulations
odf_c1 = read.csv(paste0(folder_name1,sim_set_name_contr1,'.csv'), stringsAsFactors = FALSE, skip=6)
for(i in lv1){ # round values of input variables needed to merge data frames
  odf_c1[lv1] <- round(odf_c1[lv1],6) 
}
colnames(odf_c1)[colnames(odf_c1) == 'count.turtles.with...life.stage....emerged..AND.sex....female..'] <- 'f.prenesting'

odf_c1 <- odf_c1 %>% 
  mutate(day = str_split_fixed(DateREP, " ",4)[,1],
         month = str_split_fixed(DateREP, " ",4)[,3],
         year=as.numeric(str_split_fixed(DateREP, " ",4)[,4])+(study_year1-1),
         date=dmy(paste0(day,"/",month,"/", year)))

odf_c1_avg <- f_df_avg(odf_c1) 
odf_c1_avg$treatment <- "control"

# limit to 'study duration', i.e. start to end date
odf_c1_avg$doy <- seq(from = start1, to = start1+length(odf_c1_avg$date)-1, by = 1)
odf_c1_avg <- subset(odf_c1_avg,odf_c1_avg$doy >= start1 & odf_c1_avg$doy <= end1)
odf_eff1_avg$doy <- seq(from = start1, to = start1+length(odf_eff1_avg$date)-1, by = 1)
odf_eff1_avg <- subset(odf_eff1_avg,odf_eff1_avg$doy >= start1 & odf_eff1_avg$doy <= end1)
odf_eff1b_avg$doy <- seq(from = start1, to = start1+length(odf_eff1b_avg$date)-1, by = 1)
odf_eff1b_avg <- subset(odf_eff1b_avg,odf_eff1b_avg$doy >= start1 & odf_eff1b_avg$doy <= end1)
odf_eff1c_avg$doy <- seq(from = start1, to = start1+length(odf_eff1c_avg$date)-1, by = 1)
odf_eff1c_avg <- subset(odf_eff1c_avg,odf_eff1c_avg$doy >= start1 & odf_eff1c_avg$doy <= end1)

odf1_avg <- rbind(odf_eff1_avg,odf_eff1b_avg,odf_eff1c_avg,odf_c1_avg)

## 1. Simulation outputs species 2 (Eucera)  --------------------------------------------------------------------
folder_name2 <- "Eucera/"
sim_set_name_eff2 = 'Eucera_semifield_SD_med'
sim_set_name_eff2b = 'Eucera_semifield_SD_med_50p'
sim_set_name_eff2c = 'Eucera_semifield_SD_med_0p'
sim_set_name_contr2 = 'Eucera_semifield_control'

study_year2 = 2003
DAA2 = ymd("2003-07-28")

# simulation settings
sim_settings2 <- read.csv(paste0(folder_name2,'run_semifield_Eucera_DIM.csv'))
sim_settings2 <- subset(sim_settings2, sim_settings2$name == sim_set_name_eff2)
start2 = sim_settings2$Start.day
end2 = sim_settings2$latest.emerge

# treatment simulations
odf_eff2 = read.csv(paste0(folder_name2,sim_set_name_eff2,'.csv'), stringsAsFactors = FALSE, skip=6)
for(i in lv1){ # round values of input variables needed to merge data frames
  odf_eff2[lv1] <- round(odf_eff2[lv1],6) 
}
colnames(odf_eff2)[colnames(odf_eff2) == 'count.turtles.with...life.stage....emerged..AND.sex....female..'] <- 'f.prenesting'
odf_eff2 <- odf_eff2 %>% 
  mutate(day = str_split_fixed(DateREP, " ",4)[,1],
         month = str_split_fixed(DateREP, " ",4)[,3],
         year=as.numeric(str_split_fixed(DateREP, " ",4)[,4])+(study_year2-1),
         date=dmy(paste0(day,"/",month,"/", year)))

odf_eff2_avg<-f_df_avg(odf_eff2)
odf_eff2_avg$treatment <- "100p"

# treatment simulations (50% direct spray)
odf_eff2b = read.csv(paste0(folder_name2,sim_set_name_eff2b,'.csv'), stringsAsFactors = FALSE, skip=6)
for(i in lv1){ # round values of input variables needed to merge data frames
  odf_eff2b[lv1] <- round(odf_eff2b[lv1],6) 
}
colnames(odf_eff2b)[colnames(odf_eff2b) == 'count.turtles.with...life.stage....emerged..AND.sex....female..'] <- 'f.prenesting'
odf_eff2b <- odf_eff2b %>% 
  mutate(day = str_split_fixed(DateREP, " ",4)[,1],
         month = str_split_fixed(DateREP, " ",4)[,3],
         year=as.numeric(str_split_fixed(DateREP, " ",4)[,4])+(study_year2-1),
         date=dmy(paste0(day,"/",month,"/", year)))

odf_eff2b_avg<-f_df_avg(odf_eff2b) 
odf_eff2b_avg$treatment <- "50p"

# treatment simulations (0% direct spray)
odf_eff2c = read.csv(paste0(folder_name2,sim_set_name_eff2c,'.csv'), stringsAsFactors = FALSE, skip=6)
for(i in lv1){ # round values of input variables needed to merge data frames
  odf_eff2c[lv1] <- round(odf_eff2c[lv1],6) 
}
colnames(odf_eff2c)[colnames(odf_eff2c) == 'count.turtles.with...life.stage....emerged..AND.sex....female..'] <- 'f.prenesting'
odf_eff2c <- odf_eff2c %>% 
  mutate(day = str_split_fixed(DateREP, " ",4)[,1],
         month = str_split_fixed(DateREP, " ",4)[,3],
         year=as.numeric(str_split_fixed(DateREP, " ",4)[,4])+(study_year2-1),
         date=dmy(paste0(day,"/",month,"/", year)))

odf_eff2c_avg<-f_df_avg(odf_eff2c) 
odf_eff2c_avg$treatment <- "0p"

# control simulations
odf_c2 = read.csv(paste0(folder_name2,sim_set_name_contr2,'.csv'), stringsAsFactors = FALSE, skip=6)
for(i in lv1){ # round values of input variables needed to merge data frames
  odf_c2[lv1] <- round(odf_c2[lv1],6) 
}
colnames(odf_c2)[colnames(odf_c2) == 'count.turtles.with...life.stage....emerged..AND.sex....female..'] <- 'f.prenesting'

odf_c2 <- odf_c2 %>% 
  mutate(day = str_split_fixed(DateREP, " ",4)[,1],
         month = str_split_fixed(DateREP, " ",4)[,3],
         year=as.numeric(str_split_fixed(DateREP, " ",4)[,4])+(study_year2-1),
         date=dmy(paste0(day,"/",month,"/", year)))

odf_c2_avg <- f_df_avg(odf_c2) 
odf_c2_avg$treatment <- "control"

# limit to 'study duration', i.e. start to end date
odf_c2_avg$doy <- seq(from = start2, to = start2+length(odf_c2_avg$date)-1, by = 1)
odf_c2_avg <- subset(odf_c2_avg,odf_c2_avg$doy >= start2 & odf_c2_avg$doy <= end2)
odf_eff2_avg$doy <- seq(from = start2, to = start2+length(odf_eff2_avg$date)-1, by = 1)
odf_eff2_avg <- subset(odf_eff2_avg,odf_eff2_avg$doy >= start2 & odf_eff2_avg$doy <= end2)
odf_eff2b_avg$doy <- seq(from = start2, to = start2+length(odf_eff2b_avg$date)-1, by = 1)
odf_eff2b_avg <- subset(odf_eff2b_avg,odf_eff2b_avg$doy >= start2 & odf_eff2b_avg$doy <= end2)
odf_eff2c_avg$doy <- seq(from = start2, to = start2+length(odf_eff2c_avg$date)-1, by = 1)
odf_eff2c_avg <- subset(odf_eff2c_avg,odf_eff2c_avg$doy >= start2 & odf_eff2c_avg$doy <= end2)

odf2_avg <- rbind(odf_eff2_avg,odf_eff2b_avg,odf_eff2c_avg,odf_c2_avg)

## 1. Simulation outputs species 3 (Osmia)  --------------------------------------------------------------------
folder_name3 = "Osmia/"
sim_set_name_eff3 = '951_2021_SD_med'
sim_set_name_eff3b = '951_2021_SD_med_50p'
sim_set_name_eff3c = '951_2021_SD_med_0p'
sim_set_name_contr3 = '951_2021_control'

study_year3 = 2021
DAA3 = ymd("2021-05-09")

# simulation settings
sim_settings3 <- read.csv(paste0(folder_name3,'run_semifield2021_Osmia_p_m_7days.csv'))
sim_settings3 <- subset(sim_settings3, sim_settings3$name == sim_set_name_eff3)
start3 = sim_settings3$Start.day
end3 = sim_settings3$latest.emerge

# treatment simulations with 100% exposure to direct spray
odf_eff3 = read.csv(paste0(folder_name3,sim_set_name_eff3,'.csv'), stringsAsFactors = FALSE, skip=6)
for(i in lv1){ # round values of input variables needed to merge data frames
  odf_eff3[lv1] <- round(odf_eff3[lv1],6) 
}
colnames(odf_eff3)[colnames(odf_eff3) == 'count.turtles.with...life.stage....emerged..AND.sex....female..'] <- 'f.prenesting'
odf_eff3 <- odf_eff3 %>% 
  mutate(day = str_split_fixed(DateREP, " ",4)[,1],
         month = str_split_fixed(DateREP, " ",4)[,3],
         year=as.numeric(str_split_fixed(DateREP, " ",4)[,4])+(study_year3-1),
         date=dmy(paste0(day,"/",month,"/", year)))

odf_eff3_avg<-f_df_avg(odf_eff3) 
odf_eff3_avg$treatment <- "100p"

# treatment simulations with 50% exposure to direct spray
odf_eff3b = read.csv(paste0(folder_name3,sim_set_name_eff3b,'.csv'), stringsAsFactors = FALSE, skip=6)
for(i in lv1){ # round values of input variables needed to merge data frames
  odf_eff3b[lv1] <- round(odf_eff3b[lv1],6) 
}
colnames(odf_eff3b)[colnames(odf_eff3b) == 'count.turtles.with...life.stage....emerged..AND.sex....female..'] <- 'f.prenesting'
odf_eff3b <- odf_eff3b %>% 
  mutate(day = str_split_fixed(DateREP, " ",4)[,1],
         month = str_split_fixed(DateREP, " ",4)[,3],
         year=as.numeric(str_split_fixed(DateREP, " ",4)[,4])+(study_year3-1),
         date=dmy(paste0(day,"/",month,"/", year)))

odf_eff3b_avg<-f_df_avg(odf_eff3b) 
odf_eff3b_avg$treatment <- "50p"

# treatment simulations with 0% exposure to direct spray
odf_eff3c = read.csv(paste0(folder_name3,sim_set_name_eff3c,'.csv'), stringsAsFactors = FALSE, skip=6)
for(i in lv1){ # round values of input variables needed to merge data frames
  odf_eff3c[lv1] <- round(odf_eff3c[lv1],6) 
}
colnames(odf_eff3c)[colnames(odf_eff3c) == 'count.turtles.with...life.stage....emerged..AND.sex....female..'] <- 'f.prenesting'
odf_eff3c <- odf_eff3c %>% 
  mutate(day = str_split_fixed(DateREP, " ",4)[,1],
         month = str_split_fixed(DateREP, " ",4)[,3],
         year=as.numeric(str_split_fixed(DateREP, " ",4)[,4])+(study_year3-1),
         date=dmy(paste0(day,"/",month,"/", year)))

odf_eff3c_avg<-f_df_avg(odf_eff3c) 
odf_eff3c_avg$treatment <- "0p"

# control simulations
odf_c3 = read.csv(paste0(folder_name3,sim_set_name_contr3,'.csv'), stringsAsFactors = FALSE, skip=6)
for(i in lv1){ # round values of input variables needed to merge data frames
  odf_c3[lv1] <- round(odf_c3[lv1],6) 
}
colnames(odf_c3)[colnames(odf_c3) == 'count.turtles.with...life.stage....emerged..AND.sex....female..'] <- 'f.prenesting'

odf_c3 <- odf_c3 %>% 
  mutate(day = str_split_fixed(DateREP, " ",4)[,1],
         month = str_split_fixed(DateREP, " ",4)[,3],
         year=as.numeric(str_split_fixed(DateREP, " ",4)[,4])+(study_year3-1),
         date=dmy(paste0(day,"/",month,"/", year)))

odf_c3_avg <- f_df_avg(odf_c3) 
odf_c3_avg$treatment <- "control"
# Offspring emergence not used for MS plots

# limit to 'study duration', i.e. start to end date
odf_c3_avg$doy <- seq(from = start3, to = start3+length(odf_c3_avg$date)-1, by = 1)
odf_c3_avg <- subset(odf_c3_avg,odf_c3_avg$doy >= start3 & odf_c3_avg$doy <= end3)
odf_eff3_avg$doy <- seq(from = start3, to = start3+length(odf_eff3_avg$date)-1, by = 1)
odf_eff3_avg <- subset(odf_eff3_avg,odf_eff3_avg$doy >= start3 & odf_eff3_avg$doy <= end3)
odf_eff3b_avg$doy <- seq(from = start3, to = start3+length(odf_eff3b_avg$date)-1, by = 1)
odf_eff3b_avg <- subset(odf_eff3b_avg,odf_eff3b_avg$doy >= start3 & odf_eff3b_avg$doy <= end3)
odf_eff3c_avg$doy <- seq(from = start3, to = start3+length(odf_eff3c_avg$date)-1, by = 1)
odf_eff3c_avg <- subset(odf_eff3c_avg,odf_eff3c_avg$doy >= start3 & odf_eff3c_avg$doy <= end3)

odf3_avg <- rbind(odf_eff3_avg,odf_eff3b_avg,odf_eff3c_avg,odf_c3_avg)

# 3. plot nesting data ----------------------------------------
tlines<-c("100p"="solid", "50p"="longdash", "0p"="longdash","control"="solid")
plot_nest1<-ggplot()+
  geom_line(data=odf1_avg, aes(date, y=avg.f.postem.today, group=treatment,color=factor(treatment),
                               linetype=factor(treatment)), linewidth=1.25)+
#  geom_line(data=odf_eff1_avg, aes(date, y=min.f.postem.today), linewidth=1,color ="lightblue")+
#  geom_line(data=odf_eff1_avg, aes(date, y=max.f.postem.today), linewidth=1,color ="lightblue")+
#  geom_line(data=odf_c1_avg, aes(date, y=min.f.postem.today), linewidth=1,color ="gray")+
#  geom_line(data=odf_c1_avg, aes(date, y=max.f.postem.today), linewidth=1,color ="gray")+
  geom_vline(xintercept = DAA1-0.5, linewidth = 0.75, linetype = "dotted", color = "black")+ 
  ylim(0,70)+
 # xlab("Date") + 
  ylab("Number of females in nest")+
  scale_linetype_manual(values=tlines)+
  scale_color_manual(values=c("green3","blue","magenta3","black"))+
#  scale_fill_discrete(name="treatment", breaks=levels(tlines))
  theme_classic()+
  theme(text=element_text(size=24),
        axis.text=element_text(size=20),
        axis.title=element_text(size=24,face="bold"),
        axis.title.x=element_blank(),
        strip.text.x = element_text(size=24, face = "bold"),
        plot.tag = element_text(size=24, face = "bold")) +
  theme(legend.position='none') +
  labs(tag = "A")

## plot brood cell production data
plot_brood1<-ggplot()+
  geom_line(data=odf1_avg, aes(x= date, y=avg.sum.cells, group=treatment,color=factor(treatment),
                               linetype=factor(treatment)), linewidth=1.25)+
#  geom_line(data=odf_eff1_avg, aes(x= date, y=min.sum.cells), color ="lightblue", linewidth=1)+
#  geom_line(data=odf_eff1_avg, aes(x= date, y=max.sum.cells), color ="lightblue", linewidth=1)+
#  geom_line(data=odf_c1_avg, aes(x= date, y=min.sum.cells), color ="gray", linewidth=1)+
#  geom_line(data=odf_c1_avg, aes(x= date, y=max.sum.cells), color ="gray", linewidth=1)+
  geom_vline(xintercept = DAA1-0.5, linewidth = 0.75, linetype = "dotted", color = "black")+
  ylim(0,850)+
  xlab("Date") + ylab("Number of brood cells")+
  scale_linetype_manual(values=tlines)+
  scale_color_manual(values=c("green3","blue","magenta3","black"))+
  theme_classic()+
  theme(text=element_text(size=24),axis.title=element_text(size=24,face="bold"),
        axis.text=element_text(size=20),
        legend.text=element_text(size=24),
        strip.text.x = element_text(size=24, face = "bold"),
        plot.tag = element_text(size=24, face = "bold")) +
  theme(legend.position='none') +
  labs(tag = "B")
#  theme(legend.position='bottom')+
#  labs(color="treatment", linetype=NULL)

plot_nest2<-ggplot()+
  geom_line(data=odf2_avg, aes(date, y=avg.f.postem.today, group=treatment,color=factor(treatment),
                               linetype=factor(treatment)), linewidth=1.25)+
#  geom_line(data=odf_eff2_avg, aes(date, y=min.f.postem.today), linewidth=1,color ="lightblue")+
#  geom_line(data=odf_eff2_avg, aes(date, y=max.f.postem.today), linewidth=1,color ="lightblue")+
#  geom_line(data=odf_c2_avg, aes(date, y=min.f.postem.today), linewidth=1,color ="gray")+
#  geom_line(data=odf_c2_avg, aes(date, y=max.f.postem.today), linewidth=1,color ="gray")+
  geom_vline(xintercept = DAA2-0.5, linewidth = 0.75, linetype = "dotted", color = "black")+ 
  ylim(0,70)+
#  xlab("Date") + ylab("# adult females in nest")+
  scale_linetype_manual(values=tlines)+
  scale_color_manual(values=c("green3","blue","magenta3","black"))+
  #  scale_fill_discrete(name="treatment", breaks=levels(tlines))
  theme_classic()+
  theme(text=element_text(size=24),axis.title=element_blank(),
        axis.text=element_text(size=20),
        strip.text.x = element_text(size=24, face = "bold"),
        plot.tag = element_text(size=24, face = "bold")) +
  theme(legend.position='none') +
  labs(tag = "C")

## plot brood cell production data
plot_brood2<-ggplot()+
  geom_line(data=odf2_avg, aes(x= date, y=avg.sum.cells, group=treatment,color=factor(treatment),
                               linetype=factor(treatment)), linewidth=1.25)+
#  geom_line(data=odf_eff2_avg, aes(x= date, y=min.sum.cells), color ="lightblue", linewidth=1)+
#  geom_line(data=odf_eff2_avg, aes(x= date, y=max.sum.cells), color ="lightblue", linewidth=1)+
#  geom_line(data=odf_c2_avg, aes(x= date, y=min.sum.cells), color ="gray", linewidth=1)+
#  geom_line(data=odf_c2_avg, aes(x= date, y=max.sum.cells), color ="gray", linewidth=1)+
  geom_vline(xintercept = DAA2-0.5, linewidth = 0.75, linetype = "dotted", color = "black")+
  ylim(0,850)+
  xlab("Date") + #ylab("# brood cells")+
  scale_linetype_manual(values=tlines)+
  scale_color_manual(values=c("green3","blue","magenta3","black"))+
  theme_classic()+
  theme(text=element_text(size=24),axis.title=element_text(size=24,face="bold"),
        axis.text=element_text(size=20),
        axis.title.y=element_blank(), legend.text=element_text(size=24),
        strip.text.x = element_text(size=24, face = "bold"),
        plot.tag = element_text(size=24, face = "bold")) +
  theme(legend.position='none') +
  labs(tag = "D")
#  theme(legend.position='bottom')+
#  labs(color="treatment", linetype=NULL)

plot_nest3<-ggplot()+
  geom_line(data=odf3_avg, aes(date, y=avg.f.postem.today, group=treatment,color=factor(treatment),
                               linetype=factor(treatment)), linewidth=1.25)+
#  geom_line(data=odf_eff3_avg, aes(date, y=min.f.postem.today), linewidth=1,color ="lightblue")+
#  geom_line(data=odf_eff3_avg, aes(date, y=max.f.postem.today), linewidth=1,color ="lightblue")+
#  geom_line(data=odf_c3_avg, aes(date, y=min.f.postem.today), linewidth=1,color ="gray")+
#  geom_line(data=odf_c3_avg, aes(date, y=max.f.postem.today), linewidth=1,color ="gray")+
  geom_vline(xintercept = DAA3-0.5, linewidth = 0.75, linetype = "dotted", color = "black")+ 
  ylim(0,70)+
#  xlab("Date") + ylab("# adult females in nest")+
  scale_linetype_manual(values=tlines)+
  scale_color_manual(values=c("green3","blue","magenta3","black"))+
  #  scale_fill_discrete(name="treatment", breaks=levels(tlines))
  theme_classic()+
  theme(text=element_text(size=24),axis.title=element_blank(),
        axis.text=element_text(size=20),
        strip.text.x = element_text(size=24, face = "bold"),
        plot.tag = element_text(size=24, face = "bold")) +
  theme(legend.position='none') +
  labs(tag = "E")

## plot brood cell production data
plot_brood3<-ggplot()+
  geom_line(data=odf3_avg, aes(x= date, y=avg.sum.cells, group=treatment,color=factor(treatment),
                               linetype=factor(treatment)), linewidth=1.25)+
#  geom_line(data=odf_eff3_avg, aes(x= date, y=min.sum.cells), color ="lightblue", linewidth=1)+
#  geom_line(data=odf_eff3_avg, aes(x= date, y=max.sum.cells), color ="lightblue", linewidth=1)+
#  geom_line(data=odf_c3_avg, aes(x= date, y=min.sum.cells), color ="gray", linewidth=1)+
#  geom_line(data=odf_c3_avg, aes(x= date, y=max.sum.cells), color ="gray", linewidth=1)+
  geom_vline(xintercept = DAA3-0.5, linewidth = 0.75, linetype = "dotted", color = "black")+
  ylim(0,850)+
  xlab("Date") + #ylab("# brood cells")+
  scale_linetype_manual(values=tlines)+
  scale_color_manual(values=c("green3","blue","magenta3","black"))+
  theme_classic()+
  theme(text=element_text(size=24),axis.title=element_text(face="bold"),
        axis.text=element_text(size=20),
        axis.title.y=element_blank(), legend.text=element_text(size=24),
        strip.text.x = element_text(size=24, face = "bold"),
        plot.tag = element_text(size=24, face = "bold")) +
  theme(legend.position='none') +
  labs(tag = "F")
#  theme(legend.position='bottom')+
#  labs(color="treatment", linetype=NULL)

#labs(title = paste0("Simulation: ", sim_set_name_eff1, ", ", sim_set_name_eff1b,", ", sim_set_name_eff1c))
#ggsave(paste0(folder_name1,'Brood_sim',sim_set_name_eff1,'_Dec2023.jpg'), plot_brood1, dpi=300, width = 7, height = 6)

p_nest1 <- ggplotGrob(plot_nest1)
p_brood1 <- ggplotGrob(plot_brood1)
p_nest2 <- ggplotGrob(plot_nest2)
p_brood2 <- ggplotGrob(plot_brood2)
p_nest3 <- ggplotGrob(plot_nest3)
p_brood3 <- ggplotGrob(plot_brood3)
combi1 <- rbind(p_nest1,p_brood1, size = "last")
combi2 <- rbind(p_nest2,p_brood2, size = "last")
combi3 <- rbind(p_nest3,p_brood3, size = "last")
combi <- cbind(combi1,combi2,combi3)
#ggsave(paste0("MS_fig_",sim_set_name_eff1,'_spraysce_20Dec2023.jpg'),combi, dpi=300, width = 21, height = 11)
ggsave("MS_fig_casestudy2_spraysce.jpg",combi, dpi=300, width = 21, height = 11)
