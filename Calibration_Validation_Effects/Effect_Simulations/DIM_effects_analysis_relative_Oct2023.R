## Calculate and plot relative effects of semi-field study simulations 
## O. bicornis field studies with dimethoate exposures
## Project: SolBeePop_ecotox
## Author: Amelie Schmolke 
## Last Edited: 12 Oct 2023

## Overview of script
# 1. Definitions of analysis (paths, files etc.)
# 2. Read in model outputs (controls and treatments) and calculate relative effects  
# 3. Read in study data and calculate relative effects
# 4. Calculate goodness of fit measures (comparison of simulated relative effects and measured relative effects)
# 5. Plot relative effects 
## NOTE: offspring sex ratio not included in this script: relative effect size does not make sense to compute

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

## 1. Definitions of analysis  --------------------------------------------------------------------
ofp = 'C:/SolBeePop_ecotox/Calibration_Validation_Effects/'  # output file path
folder_eff = 'Effect_Simulations/'
sim_set_name_eff = '951_2021_IT_med' #'951_2019_SD'
folder_contr = 'Control_Calib_Valid/'
sim_set_name_contr = '951'

# specifications of study data
folder_data = 'C:/SolBeePop_ecotox/Osmia_semi_field_data/'
study = 'Eurofins_Study2_2021'
study_year = 2021

setwd(ofp)

## read in and process simulation outputs
# model parameters that were varied in calibration to controls (kept in the analysis for reference)
lv1 = c('day.emerge.f', 'var.emerge.f', 't.maturation', 'p.max.nesting.life',  
  'max.f.ratio', 'max.cells', 'max.survival.e.f', 'emerged.survival', 'a.sex.age') 

## 2. Read in model outputs  ------------------------------------------------------------------
## simulation with exposure (dimethoate)
odf_eff = read.csv(paste0(folder_eff,sim_set_name_eff,'.csv'), stringsAsFactors = FALSE, skip=6)
for(i in lv1){ # round values of input variables needed to merge data frames
  odf_eff[lv1] <- round(odf_eff[lv1],6) 
}
colnames(odf_eff)[colnames(odf_eff) == 'count.turtles.with...life.stage....emerged..AND.sex....female..'] <- 'f.prenesting'
odf_eff <- odf_eff %>% 
  mutate(day = str_split_fixed(DateREP, " ",4)[,1],
         month = str_split_fixed(DateREP, " ",4)[,3],
         year=as.numeric(str_split_fixed(DateREP, " ",4)[,4])+(study_year-1),
         date=dmy(paste0(day,"/",month,"/", year)))
# correct date in simulations: measures in the study taken during the day (and prior to application on DOA);
#  simulated numbers reflect end of the day 
odf_eff$date_corr <- as.Date(odf_eff$date) + 1

## control simulation (no exposure)
odf_c = read.csv(paste0(folder_contr,sim_set_name_contr,'.csv'), stringsAsFactors = FALSE, skip=6)
for(i in lv1){ # round values of input variables needed to merge data frames
  odf_c[lv1] <- round(odf_c[lv1],6) 
}
#colnames(odf_c)[colnames(odf_c) == 'count.turtles.with...life.stage....emerged..AND.sex....female...'] <- 'f.prenesting'
colnames(odf_c)[colnames(odf_c) == 'count.turtles.with...life.stage....emerged..AND.sex....female..'] <- 'f.prenesting'
odf_c <- odf_c %>% 
  mutate(day = str_split_fixed(DateREP, " ",4)[,1],
         month = str_split_fixed(DateREP, " ",4)[,3],
         year=as.numeric(str_split_fixed(DateREP, " ",4)[,4])+(study_year-1),
         date=dmy(paste0(day,"/",month,"/", year)))
# correct date in simulations: measures in the study taken during the day (and prior to application on DOA);
#  simulated numbers reflect end of the day 
odf_c$date_corr <- as.Date(odf_c$date) + 1

## combine treatment and control simulation outputs
odf <- merge(odf_c, odf_eff, by=c('RndSeed','date_corr',lv1))

## calculate relative effects 
odf$bees.nesting.R <- (odf$bees.nesting.y - odf$bees.nesting.x)/odf$bees.nesting.x
odf$bees.nesting.today.R <- (odf$bees.nesting.today.y - odf$bees.nesting.today.x)/odf$bees.nesting.today.x
odf$f.prenesting.R <- (odf$f.prenesting.y - odf$f.prenesting.x)/odf$f.prenesting.x
odf$ad.females.R <- ((odf$bees.nesting.today.y + odf$f.prenesting.y) - (odf$bees.nesting.today.x + odf$f.prenesting.x))/(odf$bees.nesting.today.x + odf$f.prenesting.x)
odf$bees.emerged.yr.R <- (odf$bees.emerged.yr.y - odf$bees.emerged.yr.x)/odf$bees.emerged.yr.x
odf$f.emerged.yr.R <- (odf$f.emerged.yr.y - odf$f.emerged.yr.x)/odf$f.emerged.yr.x
odf$m.emerged.yr.R <- (odf$m.emerged.yr.y - odf$m.emerged.yr.x)/odf$m.emerged.yr.x
odf$sum.cells.R <- (odf$sum.cells.y - odf$sum.cells.x)/odf$sum.cells.x
odf$sum.f.cells.R <- (odf$sum.f.cells.y - odf$sum.f.cells.x)/odf$sum.f.cells.x
odf$sum.m.cells.R <- (odf$sum.m.cells.y - odf$sum.m.cells.x)/odf$sum.m.cells.x
odf$sum.cells.today.R <- (odf$sum.cells.today.y - odf$sum.cells.today.x)/odf$sum.cells.today.x
odf$sum.f.cells.today.R <- (odf$sum.f.cells.today.y - odf$sum.f.cells.today.x)/odf$sum.f.cells.today.x
odf$sum.m.cells.today.R <- (odf$sum.m.cells.today.y - odf$sum.m.cells.today.x)/odf$sum.m.cells.today.x

odf[sapply(odf, is.infinite)] <- NA

## define columns for further analysis (reduced data frame)
ov_rel_eff = c('bees.nesting.R', 'bees.nesting.today.R', 'f.prenesting.R','ad.females.R',
               'bees.emerged.yr.R', 'f.emerged.yr.R', 'm.emerged.yr.R',  
               'sum.cells.R', 'sum.f.cells.R', 'sum.m.cells.R',
               'sum.cells.today.R', 'sum.f.cells.today.R', 'sum.m.cells.today.R',
               'deaths.exp.in.y','deaths.exp.ad.y')
odf2 <- odf[,c('RndSeed','date_corr',lv1,ov_rel_eff)]

## save data frame 
#write.csv(odf2, paste0(folder_eff,'releffects_',sim_set_name_eff,'.csv'), row.names = FALSE)
odf2 <- read.csv(paste0(folder_eff,'releffects_',sim_set_name_eff,'.csv'))

odf2_summarize <- odf2 %>%
  group_by(date_corr) %>%
  summarize(avg.bees.nesting.R = mean(bees.nesting.R, na.rm = TRUE),
            min.bees.nesting.R = min(bees.nesting.R, na.rm = TRUE),
            max.bees.nesting.R = max(bees.nesting.R, na.rm = TRUE),
            avg.bees.nesting.today.R = mean(bees.nesting.today.R, na.rm = TRUE),
            min.bees.nesting.today.R = min(bees.nesting.today.R, na.rm = TRUE),
            max.bees.nesting.today.R = max(bees.nesting.today.R, na.rm = TRUE),
            avg.f.prenesting.R = mean(f.prenesting.R, na.rm = TRUE),
            min.f.prenesting.R = min(f.prenesting.R, na.rm = TRUE),
            max.f.prenesting.R = max(f.prenesting.R, na.rm = TRUE),
            avg.ad.females.R = mean(ad.females.R, na.rm = TRUE), 
            min.ad.females.R = min(ad.females.R, na.rm = TRUE), 
            max.ad.females.R = max(ad.females.R, na.rm = TRUE), 
            avg.bees.emerged.yr.R = mean(bees.emerged.yr.R, na.rm = TRUE), 
            min.bees.emerged.yr.R = min(bees.emerged.yr.R, na.rm = TRUE), 
            max.bees.emerged.yr.R = max(bees.emerged.yr.R, na.rm = TRUE), 
            avg.f.emerged.yr.R = mean(f.emerged.yr.R, na.rm =TRUE), 
            min.f.emerged.yr.R = min(f.emerged.yr.R, na.rm =TRUE), 
            max.f.emerged.yr.R = max(f.emerged.yr.R, na.rm =TRUE), 
            avg.m.emerged.yr.R = mean(m.emerged.yr.R, na.rm =TRUE),  
            min.m.emerged.yr.R = min(m.emerged.yr.R, na.rm =TRUE),  
            max.m.emerged.yr.R = max(m.emerged.yr.R, na.rm =TRUE),  
            avg.sum.cells.R = mean(sum.cells.R, na.rm = TRUE), 
            min.sum.cells.R = min(sum.cells.R, na.rm = TRUE), 
            max.sum.cells.R = max(sum.cells.R, na.rm = TRUE), 
            avg.sum.f.cells.R = mean(sum.f.cells.R, na.rm = TRUE), 
            min.sum.f.cells.R = min(sum.f.cells.R, na.rm = TRUE), 
            max.sum.f.cells.R = max(sum.f.cells.R, na.rm = TRUE), 
            avg.sum.m.cells.R = mean(sum.m.cells.R, na.rm = TRUE), 
            min.sum.m.cells.R = min(sum.m.cells.R, na.rm = TRUE), 
            max.sum.m.cells.R = max(sum.m.cells.R, na.rm = TRUE), 
            avg.sum.cells.today.R = mean(sum.cells.today.R, na.rm = TRUE), 
            min.sum.cells.today.R = min(sum.cells.today.R, na.rm = TRUE), 
            max.sum.cells.today.R = max(sum.cells.today.R, na.rm = TRUE), 
            avg.sum.f.cells.today.R = mean(sum.f.cells.today.R, na.rm = TRUE),
            min.sum.f.cells.today.R = min(sum.f.cells.today.R, na.rm = TRUE),
            max.sum.f.cells.today.R = max(sum.f.cells.today.R, na.rm = TRUE),
            avg.sum.m.cells.today.R = mean(sum.m.cells.today.R, na.rm = TRUE),
            min.sum.m.cells.today.R = min(sum.m.cells.today.R, na.rm = TRUE),
            max.sum.m.cells.today.R = max(sum.m.cells.today.R, na.rm = TRUE),
            avg.deaths.exp.in = mean(deaths.exp.in.y, na.rm = TRUE),
            min.deaths.exp.in = min(deaths.exp.in.y, na.rm = TRUE),
            max.deaths.exp.in = max(deaths.exp.in.y, na.rm = TRUE),
            avg.deaths.exp.ad = mean(deaths.exp.ad.y, na.rm = TRUE),
            min.deaths.exp.ad = min(deaths.exp.ad.y, na.rm = TRUE),
            max.deaths.exp.ad = max(deaths.exp.ad.y, na.rm = TRUE))

odf2_summarize[sapply(odf2_summarize, is.infinite)] <- NA

# 3. Read in study data and calculate relative effects ---------------------------
study_info<-read.csv(paste0(folder_data,"Study_Treatments_",study,".csv"))

# getting the start and end date to limit the x-axis in plots
study_start<-mdy(study_info$Introduction[1])
study_end<-mdy(study_info$Last.day[1])
study_appl<-mdy(study_info$Application.date[1])
study_afterappl <- as.Date(study_appl + 2) # first sample date after application: 2019: 3 day after application; 2021: 2 days after application

## Nest occupation
study_nestocc <- read.csv(paste0(folder_data,"NestOccup_",study,".csv"))
# adjust date format
study_nestocc <- study_nestocc %>%
  group_by(date, treatment) %>% 
  summarise(female.sum = sum(female)) %>% 
  mutate(date = mdy(date))

# calculate relative effect: compare each DIM treatment with average of controls
study_nestocc_controls <- study_nestocc[which(study_nestocc$treatment %in% c("Ca","Cb","Cc","Cd","Ce","Cf")),]
study_nestocc_avgcontrols <- study_nestocc_controls %>%
  group_by(date) %>%
  summarise(female.sum.control = mean(female.sum)) 

study_nestocc_DIM <- study_nestocc[which(study_nestocc$treatment %in% c("Ra","Rb","Rc","Rd")),] ## ! mind that there is Rd in the 2021 study
study_nestocc_DIM <- left_join(study_nestocc_DIM,study_nestocc_avgcontrols)
study_nestocc_DIM$releffect <- (study_nestocc_DIM$female.sum-study_nestocc_DIM$female.sum.control)/study_nestocc_DIM$female.sum.control

# calculating average between replicates
study_nestocc_DIM_avg<-study_nestocc_DIM %>% 
  mutate(overall.avg = mean(releffect, na.rm=TRUE)) %>% 
  group_by(date, overall.avg) %>% 
  summarize(female.sum.avg = mean(female.sum),
            female.sum.control.avg = mean(female.sum.control),
            releffect.avg = mean(releffect, na.rm=TRUE)) 

## Brood cell production
# reading in cell production summary table
study_cellProd <- read_csv(paste0(folder_data,"CellProd_",study,".csv"))
# summing cell production data for control groups by group and date
study_cellProd <- study_cellProd %>% 
  group_by(date, treatment) %>% 
  mutate(date = mdy(date))

# calculate relative effect: compare each DIM treatment with average of controls
study_cellProd_controls <- study_cellProd[which(study_cellProd$treatment %in% c("Ca","Cb","Cc","Cd","Ce","Cf")),]
study_cellProd_avgcontrols <- study_cellProd_controls %>%
  group_by(date) %>%
  summarise(cum.cells.control = mean(cum.cells)) 

study_cellProd_DIM <- study_cellProd[which(study_cellProd$treatment %in% c("Ra","Rb","Rc","Rd")),] ## ! mind that there is Rd in the 2021 study
study_cellProd_DIM <- left_join(study_cellProd_DIM,study_cellProd_avgcontrols)
study_cellProd_DIM$releffect <- (study_cellProd_DIM$cum.cells-study_cellProd_DIM$cum.cells.control)/study_cellProd_DIM$cum.cells.control
study_cellProd_DIM[sapply(study_cellProd_DIM, is.infinite)] <- NA

# calculating average between replicates
study_cellProd_DIM_avg<-study_cellProd_DIM %>% 
#  mutate(overall.avg = mean(releffect, na.rm=TRUE))%>% 
#  group_by(date, overall.avg) %>% 
  group_by(date) %>%
  summarize(sum.cells.avg = mean(sum.cells),
            cum.cells.avg = mean(cum.cells.control),
            cum.cells.control.avg = mean(cum.cells.control),
            releffect.avg = mean(releffect, na.rm=TRUE)) 

# output of relative effects sizes on the day after application (study_appl + 1) 
#  and the end of the tunnel phase (study_end)
rel_effect_out <- subset(odf2_summarize, odf2_summarize$date_corr == study_afterappl)
rel_effect_out <- rbind(rel_effect_out,subset(odf2_summarize, odf2_summarize$date_corr == study_end))
rel_effect_out$dataset <- c(sim_set_name_eff, sim_set_name_eff)
rel_effect_out$date_corr <- as.Date(rel_effect_out$date_corr)
rel_effect_out[nrow(rel_effect_out)+1,] <- NA
rel_effect_out$dataset[length(rel_effect_out$date_corr)] <- study
rel_effect_out$date_corr[length(rel_effect_out$date_corr)] <- study_afterappl
tmp <- subset(study_nestocc_DIM, study_nestocc_DIM$date == study_afterappl)
rel_effect_out$avg.ad.females.R[length(rel_effect_out$date_corr)] <- mean(tmp$releffect)
rel_effect_out$min.ad.females.R[length(rel_effect_out$date_corr)] <- min(tmp$releffect) 
rel_effect_out$max.ad.females.R[length(rel_effect_out$date_corr)] <- max(tmp$releffect) 
tmp <- subset(study_cellProd_DIM, study_cellProd_DIM$date == study_afterappl)
rel_effect_out$avg.sum.cells.R[length(rel_effect_out$date_corr)] <- mean(tmp$releffect)
rel_effect_out$min.sum.cells.R[length(rel_effect_out$date_corr)] <- min(tmp$releffect)
rel_effect_out$max.sum.cells.R[length(rel_effect_out$date_corr)] <- max(tmp$releffect)
rel_effect_out[nrow(rel_effect_out)+1,] <- NA
rel_effect_out$dataset[length(rel_effect_out$date_corr)] <- study
rel_effect_out$date_corr[length(rel_effect_out$date_corr)] <- study_end
tmp <- subset(study_nestocc_DIM, study_nestocc_DIM$date == study_end)
rel_effect_out$avg.ad.females.R[length(rel_effect_out$date_corr)] <- mean(tmp$releffect)
rel_effect_out$min.ad.females.R[length(rel_effect_out$date_corr)] <- min(tmp$releffect) 
rel_effect_out$max.ad.females.R[length(rel_effect_out$date_corr)] <- max(tmp$releffect) 
tmp <- subset(study_cellProd_DIM, study_cellProd_DIM$date == study_end)
rel_effect_out$avg.sum.cells.R[length(rel_effect_out$date_corr)] <- mean(tmp$releffect)
rel_effect_out$min.sum.cells.R[length(rel_effect_out$date_corr)] <- min(tmp$releffect)
rel_effect_out$max.sum.cells.R[length(rel_effect_out$date_corr)] <- max(tmp$releffect)
write.csv(rel_effect_out, paste0(folder_eff,'releffects_summary_',sim_set_name_eff,'.csv'), row.names = FALSE)
## 
# 4. Calculate goodness of fit measures ----------------------------------------
##
Nesting.RMSE2 <-odf2_summarize %>% 
  ungroup() %>% 
  inner_join(study_nestocc_DIM_avg, by = c('date_corr'= 'date')) %>% 
  summarize(Nesting.RMSE2 = sqrt(mean((releffect.avg - avg.ad.females.R)^2))) #%>% 
# nothing to normalize to for relative effects: NRMSE not calculated separately
  
Nesting.MAE2 <-odf2_summarize %>% 
  ungroup() %>% 
  inner_join(study_nestocc_DIM_avg, by = c('date_corr'= 'date')) %>% 
  summarize(Nesting.MAE2 = mean(abs(releffect.avg - avg.ad.females.R))) #%>% 

# unclear what RSR means for relative effect
mean_eff <- mean(study_nestocc_DIM_avg$releffect.avg)
Nesting.RSR2 <-odf2_summarize %>% 
  ungroup() %>% 
  inner_join(study_nestocc_DIM_avg, by = c('date_corr'= 'date')) %>% 
  summarize(Nesting.RSR2 = mean(sqrt((releffect.avg - avg.ad.females.R)^2)/sqrt((releffect.avg - mean_eff)^2))) 

BroodCell.RMSE <-odf2_summarize %>% 
  ungroup() %>% 
  inner_join(study_cellProd_DIM_avg, by = c('date_corr'= 'date')) %>% 
  summarize(BroodCell.RMSE = sqrt(mean((releffect.avg - avg.sum.cells.R)^2, na.rm = TRUE))) #%>% 
# nothing to normalize to for relative effects: NRMSE not calculated separately

BroodCell.MAE <-odf2_summarize %>% 
  ungroup() %>% 
  inner_join(study_cellProd_DIM_avg, by = c('date_corr'= 'date')) %>% 
  summarize(BroodCell.MAE2 = mean(abs(releffect.avg - avg.sum.cells.R), na.rm = TRUE)) #%>% 

# unclear what RSR means for relative effect
mean_eff <- mean(study_cellProd_DIM$releffect)
BroodCell.RSR <-odf2_summarize %>% 
  ungroup() %>% 
  inner_join(study_cellProd_DIM_avg, by = c('date_corr'= 'date')) %>% 
  summarize(BroodCell.RSR2 = mean(sqrt((releffect.avg - avg.sum.cells.R)^2)/sqrt((releffect.avg - mean_eff)^2), na.rm=TRUE)) 

GOFM<-data.frame(c(sim_set_name_eff, BroodCell.RMSE, BroodCell.MAE, BroodCell.RSR,
                   Nesting.RMSE2, Nesting.MAE2, Nesting.RSR2))
colnames(GOFM)<-c('name','BroodCell.RMSE', 'BroodCell.MAE', 'BroodCell.RSR',
                  'Nesting.RMSE2', 'Nesting.MAE2', 'Nesting.RSR2')
  
GOFM$Avg.RMSE<- rowMeans(GOFM[,c('Nesting.RMSE2','BroodCell.RMSE')])
GOFM$Avg.MAE<- rowMeans(GOFM[,c('Nesting.MAE2','BroodCell.MAE')])
GOFM$Avg.RSR<- rowMeans(GOFM[,c('Nesting.RSR2','BroodCell.RSR')])

# Write goodness of fit measures to file 
write_csv(GOFM, paste0(ofp,folder_eff,"\\GOFM_releffects_",sim_set_name_eff,".csv"))

## 5. Plot relative effects -------------------------------------------------------
# plot nesting data
plot_nest1<-ggplot()+
  geom_line(data=odf2_summarize, aes(date_corr, y=avg.ad.females.R),color ="black", linewidth=1)+
  geom_line(data=odf2_summarize, aes(date_corr, y=min.ad.females.R),color ="gray", linewidth=1)+
  geom_line(data=odf2_summarize, aes(date_corr, y=max.ad.females.R),color ="gray", linewidth=1)+
  geom_point(data=study_nestocc_DIM, aes(date, releffect, color =treatment, shape =treatment ), size=3)+
  geom_hline(yintercept = 0, linewidth = 0.5, linetype = "dotted")+
  geom_vline(xintercept = study_appl, linewidth = 0.2, linetype = "dashed", color = "blue")+
  xlim(study_start, study_end)+  ylim(-1,1)+
  xlab("Date") + ylab("Relative effect on # nesting females")+
  scale_shape_manual(values = c(15, 16, 17, 18, 19, 20))+
  theme_classic()+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),
        strip.text.x = element_text(size = 12, face = "bold")) +
  theme(legend.position='bottom')+
  scale_color_npg()+
  labs(title = paste("Eurofins",study_year,"data, simulation", sim_set_name_eff))
ggsave(paste0(folder_eff,'\\NestOcc_releffect',sim_set_name_eff,'_Oct2023.jpg'), plot_nest1, dpi=300, width = 7, height = 5)

## plot brood cell production data
plot_brood1<-ggplot()+
  geom_line(data=odf2_summarize, aes(x= date_corr, y=avg.sum.cells.R), color ="black", linewidth=1)+
  geom_line(data=odf2_summarize, aes(x= date_corr, y=min.sum.cells.R), color ="gray", linewidth=1)+
  geom_line(data=odf2_summarize, aes(x= date_corr, y=max.sum.cells.R), color ="gray", linewidth=1)+
  geom_point(data=study_cellProd_DIM, aes(date, releffect, color=treatment, shape =treatment), size=3)+
  geom_hline(yintercept = 0, linewidth = 0.5, linetype = "dotted")+
  geom_vline(xintercept = study_appl, linewidth = 0.2, linetype = "dashed", color = "blue")+
  xlim(study_start, study_end)+ ylim(-1,1)+
  xlab("Date") + ylab("Relative effect on # brood cells")+
  scale_shape_manual(values = c(15, 16, 17, 18, 19, 20))+
  theme_classic()+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),
        strip.text.x = element_text(size = 12, face = "bold")) +
  theme(legend.position='bottom')+
  scale_color_npg()+
  labs(title = paste("Eurofins",study_year,"data, simulation", sim_set_name_eff))
ggsave(paste0(folder_eff,'\\Brood_releffect',sim_set_name_eff,'_Oct2023.jpg'), plot_brood1, dpi=300, width = 7, height = 5)

