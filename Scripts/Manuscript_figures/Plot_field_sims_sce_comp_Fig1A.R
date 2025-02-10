## SolBeePop_ecotox: landscape application
## Simulation of Ruddle et al. (2018) field studies
## Script for plotting simulation outputs along with study data 
##  Manuscript Fig. 1A: comparing low, medium and high detail scenarios
## Simulations of Tue2015 used for manuscript figure 1
## Author: Amelie Schmolke

Sys.setlocale("LC_ALL", "English") # to make sure dates appear in English in plots

library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggsci)
library(stringr)
library(gridExtra)

rm(list = ls()) # clean up workspace

### Note: 
##    SceA = sce (a) = low detail scenario
##    SceB = sce (b) = medium detail scenario
##    SceC = sce (c) = high detail scenario

##### Study specifications
# folder path
setwd("C:/SolBeePop/Field_study_simulations/")
# plot outputs from the three scenarios for comparison (note: "dist" with 1200m foraging radius used for sce (b) and (c))
# simulation and study name # needs to be set to each scenario / study
run_sce_a <- "Tue2015_SceA_IT" # run ID, from run*.csv file
run_sce_b <- "Tue2015_SceB_1200m_dist_IT" # run ID, from run*.csv file
run_sce_c <- "Tue2015_SceC_1200m_dist_IT" # run ID, from run*.csv file # incluced in output file name

studyID <- "S15-01803" # Ruddle study ID (applies to field studies from 2014; field and semi-field studies in 2015)
study_year <- 2015 # study year of field study 

##### Field study data (brood cell production)
# Study info: read for determination of study start and end 
field_study_info<-read_csv(paste0("C:/SolBeePop/Field_study_data/",studyID,"_field_Tables/Study_Treatments_",studyID,"_field.csv"))

# getting the start and end date to limit the x-axis in plots
field_study_start<-mdy(field_study_info$Introduction[1]) # 2014: Introduction.r1[1]; 2015: Introduction[1]
field_study_end<-mdy(field_study_info$Last.day[1])

# read in brood cell production data
field_cellProd<-read_csv(paste0("C:/SolBeePop/Field_study_data/",studyID,"_field_Tables/CellProd_",studyID,"_field.csv"))

# preparing data frames for plotting
field_cellProd<-field_cellProd %>% 
  select(Date, treatment, sum.cells) %>%
  mutate(Date =dmy(Date), day = as.Date(Date)-field_study_start, study =studyID, study.type = "field")

##### Semi-field study data (only available for 2015 studies); plotted for comparison

semi_study_info<-read_csv(paste0("C:/SolBeePop/Field_study_data/",studyID,"-01_Tables/Study_Treatments_",studyID,"-01.csv"))

# getting the start and end date to limit the x-axis in plots
semi_study_start<-mdy(semi_study_info$Introduction[1]) # Introduction.r1[1])
semi_study_end<-mdy(semi_study_info$Last.day[1])

# read in brood cell production data
semi_cellProd<-read_csv(paste0("C:/SolBeePop/Field_study_data/",studyID,"-01_Tables/CellProd_",studyID,"-01.csv"))

# preparing data frame for plotting
semi_cellProd <- semi_cellProd %>%
    group_by(date,treatment) %>%
    summarize(sum.cells = sum(sum.cells))
semi_cellProd <- semi_cellProd %>%
    group_by(treatment) %>%
    mutate(cum.cells = cumsum(sum.cells), Date =as.Date(date), day = as.Date(Date)-semi_study_start, 
           study =studyID, study.type = "semifield")
semi_cellProd<-semi_cellProd %>% 
    select(Date, treatment, cum.cells, day, study, study.type)
names(semi_cellProd)[names(semi_cellProd) == "cum.cells"] <- "sum.cells"

# combine field and semi-field study data
cell_Prod <- rbind(field_cellProd,semi_cellProd) # use only for 2015: combine study data from field and semi-field studies 

##### Simulation outputs (simulation outputs read in the same way irrespective of study year)
# Sce A: low detail scenario
sim_sce_a<-read_csv(paste0(run_sce_a,".csv"), skip=6)

#fixing date format
sim_sce_a <- sim_sce_a %>% 
  mutate(day = str_split_fixed(DateREP, " ",4)[,1],
         month = str_split_fixed(DateREP, " ",4)[,3],
         year=as.numeric(str_split_fixed(DateREP, " ",4)[,4])+(study_year-1),
         Date=dmy(paste0(day,"/",month,"/", year)))

# calculate average, min and max values of 10 repetitions 
sim_sce_a<-sim_sce_a %>% 
  group_by(var.emerge.f, t.maturation, p.max.nesting.life, max.f.ratio, max.cells, max.survival.e.f, Date) %>% 
  summarize(avg.bees.nesting.today=mean(bees.nesting.today),
            min.bees.nesting.today=min(bees.nesting.today),
            max.bees.nesting.today=max(bees.nesting.today),
            avg.postemerg.f.today=mean(f.postemergent.today),
            min.postemerg.f.today=min(f.postemergent.today),
            max.postemerg.f.today=max(f.postemergent.today),
            avg.sum.cells=mean(sum.cells),
            min.sum.cells=min(sum.cells),
            max.sum.cells=max(sum.cells),
            avg.sum.cells.today=mean(sum.cells.today),
            min.sum.cells.today=min(sum.cells.today),
            max.sum.cells.today=max(sum.cells.today),
            avg.sum.f.cells.today=mean(sum.f.cells.today),
            min.sum.f.cells.today=min(sum.f.cells.today),
            max.sum.f.cells.today=max(sum.f.cells.today))

sim_sce_a$scenario <- "Low detail scenario"

# Sce B: medium detail scenario
sim_sce_b<-read_csv(paste0(run_sce_b,".csv"), skip=6)

#fixing date format
sim_sce_b <- sim_sce_b %>% 
  mutate(day = str_split_fixed(DateREP, " ",4)[,1],
         month = str_split_fixed(DateREP, " ",4)[,3],
         year=as.numeric(str_split_fixed(DateREP, " ",4)[,4])+(study_year-1),
         Date=dmy(paste0(day,"/",month,"/", year)))

# average, min and max values of 10 repetitions 
sim_sce_b<-sim_sce_b %>% 
  group_by(var.emerge.f, t.maturation, p.max.nesting.life, max.f.ratio, max.cells, max.survival.e.f, Date) %>% 
  summarize(avg.bees.nesting.today=mean(bees.nesting.today),
            min.bees.nesting.today=min(bees.nesting.today),
            max.bees.nesting.today=max(bees.nesting.today),
            avg.postemerg.f.today=mean(f.postemergent.today),
            min.postemerg.f.today=min(f.postemergent.today),
            max.postemerg.f.today=max(f.postemergent.today),
            avg.sum.cells=mean(sum.cells),
            min.sum.cells=min(sum.cells),
            max.sum.cells=max(sum.cells),
            avg.sum.cells.today=mean(sum.cells.today),
            min.sum.cells.today=min(sum.cells.today),
            max.sum.cells.today=max(sum.cells.today),
            avg.sum.f.cells.today=mean(sum.f.cells.today),
            min.sum.f.cells.today=min(sum.f.cells.today),
            max.sum.f.cells.today=max(sum.f.cells.today))

sim_sce_b$scenario <- "Medium detail scenario"

# Sce C: high detail scenario
sim_sce_c<-read_csv(paste0(run_sce_c,".csv"), skip=6)

#fixing date format
sim_sce_c <- sim_sce_c %>% 
  mutate(day = str_split_fixed(DateREP, " ",4)[,1],
         month = str_split_fixed(DateREP, " ",4)[,3],
         year=as.numeric(str_split_fixed(DateREP, " ",4)[,4])+(study_year-1),
         Date=dmy(paste0(day,"/",month,"/", year)))

# average, min and max values of 10 repetitions 
sim_sce_c<-sim_sce_c %>% 
  group_by(var.emerge.f, t.maturation, p.max.nesting.life, max.f.ratio, max.cells, max.survival.e.f, Date) %>% 
  summarize(avg.bees.nesting.today=mean(bees.nesting.today),
            min.bees.nesting.today=min(bees.nesting.today),
            max.bees.nesting.today=max(bees.nesting.today),
            avg.postemerg.f.today=mean(f.postemergent.today),
            min.postemerg.f.today=min(f.postemergent.today),
            max.postemerg.f.today=max(f.postemergent.today),
            avg.sum.cells=mean(sum.cells),
            min.sum.cells=min(sum.cells),
            max.sum.cells=max(sum.cells),
            avg.sum.cells.today=mean(sum.cells.today),
            min.sum.cells.today=min(sum.cells.today),
            max.sum.cells.today=max(sum.cells.today),
            avg.sum.f.cells.today=mean(sum.f.cells.today),
            min.sum.f.cells.today=min(sum.f.cells.today),
            max.sum.f.cells.today=max(sum.f.cells.today))

sim_sce_c$scenario <- "High detail scenario"

# combine outputs from all three scenarios
sim_data <- rbind(sim_sce_a,sim_sce_b,sim_sce_c)

## code allowing to set the order of two legends in the same plot 
guides_merge <- function(gdefs) {
  gdefs <- lapply(gdefs, function(g) { g$hash <- paste(g$order, g$hash, sep = "z"); g})
  tapply(gdefs, sapply(gdefs, function(g)g$hash), function(gs)Reduce(guide_merge, gs))
}
environment(guides_merge) <- environment(ggplot)
assignInNamespace("guides_merge", guides_merge, pos = "package:ggplot2")

# plotting -----------------------------------------------------------
sim_data$scenario <- factor(sim_data$scenario, levels=c('Low detail scenario','Medium detail scenario','High detail scenario'))

# plot of brood production rate: study data and average, minimum and maximum of simulation outputs for the three scenarios 
brood<-ggplot()+
  geom_line(data=sim_data, aes(x= Date, avg.sum.cells, color =scenario), linewidth=1)+
  geom_line(data=sim_data, aes(x= Date, min.sum.cells, color =scenario), linetype="dotted",linewidth=0.5)+ #
  geom_line(data=sim_data, aes(x= Date, max.sum.cells, color =scenario), linetype="dotted",linewidth=0.5)+
  geom_ribbon(data=subset(sim_data,sim_data$scenario == "Low detail scenario"), 
              aes(x=Date, ymin=min.sum.cells,ymax=max.sum.cells), fill="#009E73", alpha = 0.1)+
  geom_ribbon(data=subset(sim_data,sim_data$scenario == "Medium detail scenario"), 
              aes(x=Date, ymin=min.sum.cells,ymax=max.sum.cells), fill="#E69F00", alpha = 0.1)+
  geom_ribbon(data=subset(sim_data,sim_data$scenario == "High detail scenario"), 
              aes(x=Date, ymin=min.sum.cells,ymax=max.sum.cells), fill="black", alpha = 0.1)+
  geom_point(data=cell_Prod, aes(Date, sum.cells, shape =study.type), size=1.2, color = 'grey45')+ #color = treatment, 
  xlim(field_study_start, field_study_end)+ 
  ylim(0,1000)+ # adjust y-axis limits: max. 4000 for 2014; max. 1200 for 2015 
  xlab("Date") + ylab("Number of brood cells")+
  scale_shape_manual(values = c(1, 2))+
  scale_color_manual(values = c("#009E73","#E69F00","black"))+
  scale_fill_discrete(breaks = c('Low detail scenario','Medium detail scenario','High detail scenario','field','semifield'))+
  guides(shape = guide_legend(order = 2), color = guide_legend(order = 1))+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=20,face="bold"))+
  theme_minimal(base_size = 15)+
  theme(legend.position = c(0.22,0.72), legend.title=element_blank())+
  theme(legend.background = element_rect(color="white",fill="white")) 
ggsave(paste0('Fig1A_brood_sce_comp_',run_sce_c,'.jpg'), brood, dpi=300, width = 7, height = 5)
