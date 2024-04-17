## Calculate and plot relative effects of hypthetical semi-field study simulations 
## Project: SolBeePop_ecotox
## Author: Amelie Schmolke 
## Last Edited: 21 Nov 2023

## Overview of script
# 1. Definitions of analysis (paths, files etc.)
# 2. Read in model outputs (controls and treatments) and calculate relative effects  
# 3. Plot relative effects 

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
ofp = 'C:/SolBeePop_ecotox/Hypothetical_Semi_Field/Eucera/'  # output file path
setwd(ofp)

sim_set_name_eff = 'Eucera_semifield_SD_med_m7days'
sim_set_name_contr = 'Eucera_semifield_control_m7days'

# study year: need to be set but is not relevant for simulations of hypothetical studies; using the year of the weather data here
study_year = 2003
# Day of application: this date need to be set according to the simulation set / species
DAA = ymd("2003-07-28")

# simulation settings
sim_settings <- read.csv('run_semifield_Eucera_DIM.csv')
sim_settings <- subset(sim_settings, sim_settings$name == sim_set_name_eff)
start = sim_settings$Start.day
end = sim_settings$latest.emerge

## read in and process simulation outputs
# model parameters that were varied in calibration to controls (kept in the analysis for reference)
lv1 = c('day.emerge.f', 'var.emerge.f', 't.maturation', 'p.max.nesting.life',  
  'max.f.ratio', 'max.cells', 'max.survival.e.f', 'emerged.survival', 'a.sex.age') 

## 2. Read in model outputs  ------------------------------------------------------------------
## simulation with exposure (dimethoate)
odf_eff = read.csv(paste0(sim_set_name_eff,'.csv'), stringsAsFactors = FALSE, skip=6)
for(i in lv1){ # round values of input variables needed to merge data frames
  odf_eff[lv1] <- round(odf_eff[lv1],6) 
}
colnames(odf_eff)[colnames(odf_eff) == 'count.turtles.with...life.stage....emerged..AND.sex....female..'] <- 'f.prenesting'
odf_eff <- odf_eff %>% 
  mutate(day = str_split_fixed(DateREP, " ",4)[,1],
         month = str_split_fixed(DateREP, " ",4)[,3],
         year=as.numeric(str_split_fixed(DateREP, " ",4)[,4])+(study_year-1),
         date=dmy(paste0(day,"/",month,"/", year)))

## control simulation (no exposure)
odf_c = read.csv(paste0(sim_set_name_contr,'.csv'), stringsAsFactors = FALSE, skip=6)
for(i in lv1){ # round values of input variables needed to merge data frames
  odf_c[lv1] <- round(odf_c[lv1],6) 
}
colnames(odf_c)[colnames(odf_c) == 'count.turtles.with...life.stage....emerged..AND.sex....female..'] <- 'f.prenesting'
odf_c <- odf_c %>% 
  mutate(day = str_split_fixed(DateREP, " ",4)[,1],
         month = str_split_fixed(DateREP, " ",4)[,3],
         year=as.numeric(str_split_fixed(DateREP, " ",4)[,4])+(study_year-1),
         date=dmy(paste0(day,"/",month,"/", year)))

## combine treatment and control simulation outputs
odf <- merge(odf_c, odf_eff, by=c('RndSeed','date',lv1))

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
odf2 <- odf[,c('RndSeed','date',lv1,ov_rel_eff)]

## save data frame 
#write.csv(odf2, paste0('releffects_',sim_set_name_eff,'.csv'), row.names = FALSE)
odf2 <- read.csv(paste0('releffects_',sim_set_name_eff,'.csv'))
start_date <- as.Date(odf2$date[1])
diff_date <- end - start
end_date <- as.Date(start_date) + diff_date

odf2_summarize <- odf2 %>%
  group_by(date) %>%
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

# output of relative effects sizes on the first day of sampling after application; 
#   2019: study_appl + 3; 2021: (study_appl + 2) 
#  and the end of the tunnel phase (study_end)
rel_effect_out <- subset(odf2_summarize, odf2_summarize$date == (as.Date(DAA)+2))
rel_effect_out <- rbind(rel_effect_out,subset(odf2_summarize, odf2_summarize$date == end_date))
rel_effect_out$dataset <- c(sim_set_name_eff, sim_set_name_eff)
rel_effect_out$date <- as.Date(rel_effect_out$date)
write.csv(rel_effect_out, paste0('releffects_summary_',sim_set_name_eff,'.csv'), row.names = FALSE)


# for offspring emergence: counts at the end of the 2nd year
offspring <- subset(odf, odf$date == odf$date[length(odf$date)])
offspring <- offspring %>%
  group_by(date) %>%
  summarize(avg.bees.emerged.yr.R = mean(bees.emerged.yr.R, na.rm = TRUE), 
            min.bees.emerged.yr.R = min(bees.emerged.yr.R, na.rm = TRUE), 
            max.bees.emerged.yr.R = max(bees.emerged.yr.R, na.rm = TRUE), 
            avg.f.emerged.yr.R = mean(f.emerged.yr.R, na.rm =TRUE), 
            min.f.emerged.yr.R = min(f.emerged.yr.R, na.rm =TRUE), 
            max.f.emerged.yr.R = max(f.emerged.yr.R, na.rm =TRUE), 
            avg.m.emerged.yr.R = mean(m.emerged.yr.R, na.rm =TRUE),  
            min.m.emerged.yr.R = min(m.emerged.yr.R, na.rm =TRUE),  
            max.m.emerged.yr.R = max(m.emerged.yr.R, na.rm =TRUE))  
            
offspring1 = melt(offspring, id.vars = c('date'),
                  measure.vars = c("avg.f.emerged.yr.R", "avg.m.emerged.yr.R"))
colnames(offspring1)[colnames(offspring1) == 'value'] <- 'avg'
offspring1$sex <- c("female", "male")
offspring1$min <- c(offspring$min.f.emerged.yr.R, offspring$min.m.emerged.yr.R)
offspring1$max <- c(offspring$max.f.emerged.yr.R, offspring$max.m.emerged.yr.R)

## plot control and treatment sims
# limit to 'study duration', i.e. start to end date
odf2_summarize$doy <- seq(from = start, to = start+length(odf2_summarize$date)-1, by = 1)
odf2_summarize <- subset(odf2_summarize,odf2_summarize$doy >= start & odf2_summarize$doy <= end)

## 3. Plot relative effects -------------------------------------------------------
# plot nesting data
plot_nest1<-ggplot()+
  geom_line(data=odf2_summarize, aes(date, y=avg.ad.females.R),color ="blue", linewidth=1)+
  geom_line(data=odf2_summarize, aes(date, y=min.ad.females.R),color ="lightblue", linewidth=1)+
  geom_line(data=odf2_summarize, aes(date, y=max.ad.females.R),color ="lightblue", linewidth=1)+
  geom_hline(yintercept = 0, linewidth = 0.5, linetype = "dotted")+
  geom_vline(xintercept = DAA-0.5, linewidth = 0.2, linetype = "dashed", color = "blue")+
  ylim(-1,1)+
  xlab("Date") + ylab("Relative effect on # nesting females")+
  theme_classic()+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),
        strip.text.x = element_text(size = 12, face = "bold")) +
  theme(legend.position='bottom')+
  scale_color_npg()+
  labs(title = paste("Simulation:", sim_set_name_eff))
ggsave(paste0('NestOcc_releffect_',sim_set_name_eff,'_Nov2023.jpg'), plot_nest1, dpi=300, width = 7, height = 5)

## plot brood cell production data
plot_brood1<-ggplot()+
  geom_line(data=odf2_summarize, aes(x= date, y=avg.sum.cells.R), color ="blue", linewidth=1)+
  geom_line(data=odf2_summarize, aes(x= date, y=min.sum.cells.R), color ="lightblue", linewidth=1)+
  geom_line(data=odf2_summarize, aes(x= date, y=max.sum.cells.R), color ="lightblue", linewidth=1)+
  geom_hline(yintercept = 0, linewidth = 0.5, linetype = "dotted")+
  geom_vline(xintercept = DAA-0.5, linewidth = 0.2, linetype = "dashed", color = "blue")+
  ylim(-1,1)+
  xlab("Date") + ylab("Relative effect on # brood cells")+
  theme_classic()+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),
        strip.text.x = element_text(size = 12, face = "bold")) +
  theme(legend.position='bottom')+
  scale_color_npg()+
  labs(title = paste("Simulation:", sim_set_name_eff))
ggsave(paste0('Brood_releffect_',sim_set_name_eff,'_Nov2023.jpg'), plot_brood1, dpi=300, width = 7, height = 5)

# plot offspring 
plot_offspring<-ggplot()+
  geom_pointrange(data=offspring1, aes(x= sex, y=avg, ymin=min,
                                      ymax=max), 
                  color = "blue", shape = 95, size = 2, linewidth=0.5)+ #color ="pink", 
  geom_hline(yintercept = 0, linewidth = 0.5, linetype = "dotted")+
  ylim(-1,0.5)+
  ylab("Relative effect on # of emerged bees")+ xlab("")+
  scale_color_manual(values = c("pink","skyblue"), labels=c("females", "males"))+
  theme_classic()+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),
        strip.text.x = element_text(size = 12, face = "bold")) +
  theme(legend.position = 'bottom')+
  labs(title = paste("Simulation:", sim_set_name_eff))
ggsave(paste0('Offspring_releffect_',sim_set_name_eff,'_Nov2023.jpg'), plot_offspring, dpi=300, width = 5, height = 5)

