## SolBeePop_ecotox: landscape application
## Simulation of Ruddle et al. (2018) field studies
## Script called by "Plot_panel_field_sims_Fig2.R"
## Script generates individual plot panels for manuscript Fig. 2
## Author: Amelie Schmolke
## SolBeePop landscape

library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggsci)
library(stringr)
library(gridExtra)

##### NOTE: case differentiation necessary between study data from 2014 vs. 2015 trials
#####       because in 2014, no semi-field studies were conducted and the corresponding
#####       data are not available (and need to be taken out of plotting);
#####       In addition, study designs of field studies differed slightly between 2014 and 2015

##### Field study data
# Study info 
field_study_info<-read_csv(paste0("C:/SolBeePop/Field_study_data/",studyID,"_field_Tables/Study_Treatments_",studyID,"_field.csv"))

# getting the start and end date to limit the x-axis in plots
field_study_start <- 0
if(study_year == 2014){
  field_study_start <- mdy(field_study_info$Introduction.r1[1]) # 2014: Introduction.r1[1]; 2015: Introduction[1]
}
if(study_year == 2015){
  field_study_start <- mdy(field_study_info$Introduction[1]) # 2014: Introduction.r1[1]; 2015: Introduction[1]
}
field_study_end<-mdy(field_study_info$Last.day[1])

##### Semi-field study data (only available for 2015 studies); plotted for comparison
##### do not use following code for 2014 studies
if(study_year == 2015){
  # Study info 
semi_study_info<-read_csv(paste0("C:/SolBeePop/Field_study_data/",studyID,"-01_Tables/Study_Treatments_",studyID,"-01.csv"))

# getting the start and end date to limit the x-axis in plots
semi_study_start<-mdy(semi_study_info$Introduction[1]) # Introduction.r1[1])
semi_study_end<-mdy(semi_study_info$Last.day[1])
#####
}

# Brood cells (Ruddle data) -----------------------------------------
# reading in cell production summary table; note that cell production is listed as cumulative numbers
field_cellProd<-read_csv(paste0("C:/SolBeePop/Field_study_data/",studyID,"_field_Tables/CellProd_",studyID,"_field.csv"))
if(study_year == 2015){
  ##### Semi-field study data (only available for 2015 studies); plotted for comparison
  semi_cellProd<-read_csv(paste0("C:/SolBeePop/Field_study_data/",studyID,"-01_Tables/CellProd_",studyID,"-01.csv"))
#####
}

# preparing data frames for plotting
field_cellProd<-field_cellProd %>% 
  select(Date, treatment, sum.cells) %>%
  mutate(Date =dmy(Date), day = as.Date(Date)-field_study_start, study =studyID, study.type = "field")
if(study_year == 2015){
  ##### Semi-field study data (only available for 2015 studies); plotted for comparison
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
  cell_Prod <- rbind(field_cellProd,semi_cellProd) # combine study data from field and semi-field studies 
}
if(study_year == 2014){
  cell_Prod <- field_cellProd # no combination of data 
}

# Nesting females (Ruddle data) -----------------------------------------------
# Note: nesting data not shown as figure in manuscript
# reading in nest occupation summary table 
field_nesting<-read_csv(paste0("C:/SolBeePop/Field_study_data/",studyID,"_field_Tables/NestOccup_",studyID,"_field.csv"))
if(study_year == 2015){
  ##### Semi-field study data (only available for 2015 studies); plotted for comparison
  semi_nesting<-read_csv(paste0("C:/SolBeePop/Field_study_data/",studyID,"-01_Tables/NestOccup_",studyID,"-01.csv"))
}

# preparing data frames for plotting
field_nesting<-field_nesting %>% 
  select(Date, treatment, female) %>%
  mutate(Date =dmy(Date), day = as.Date(Date)-field_study_start, study =studyID, study.type = "field")
if(study_year == 2015){
  ##### Semi-field study data (only available for 2015 studies); plotted for comparison
  semi_nesting <- semi_nesting %>%
    mutate(Date =as.Date(date), day = as.Date(date)-semi_study_start, 
           study =studyID, study.type = "semifield")
  semi_nesting <- semi_nesting %>%
    group_by(Date,treatment, day, study, study.type) %>%
    summarize(female = sum(female))
  nesting <- rbind(field_nesting,semi_nesting) # combine study data from field and semi-field studies
}
if(study_year == 2014){
  nesting <- field_nesting # no combination of data 
}


#----------- read in simulation outputs (simulation outputs read in the same way irrespective of study year)
sim<-read_csv(paste0(run_num,".csv"), skip=6)

#fixing date format
sim <- sim %>% 
  mutate(day = str_split_fixed(DateREP, " ",4)[,1],
         month = str_split_fixed(DateREP, " ",4)[,3],
         year=as.numeric(str_split_fixed(DateREP, " ",4)[,4])+(study_year-1),
         Date=dmy(paste0(day,"/",month,"/", year)))

# average value of 10 repetitions 
sim_data<-sim %>% 
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

# plotting data -----------------------------------------------------------
# plots: data sets do not need to be adjusted to study year but y-axes limits 
#        because more bees (150 females) were released in 2014 compared to 2015 (60 females)  
max_nesting <- 0
max_brood <- 0
if(study_year == 2014){
  max_nesting <- 150
  max_brood <- 4000
}
if(study_year == 2015){
  max_nesting <- 60
  max_brood <- 1200
}

legend_pos <- 0
if(studyID == "S14-01501" || studyID == "S14-01570" || 
   studyID == "S15-01802" || studyID == "S15-01803"){
  legend_pos <- "none"
}
if(studyID == "S14-01571" || studyID == "S15-01804"){
  legend_pos <- "bottom"
}

# nesting data not included in manuscript figure
study_nest<-ggplot()+
  geom_point(data=nesting, aes(Date, female, color = treatment, shape = study.type), size=3)+
  geom_line(data=sim_data, aes(Date, y=avg.bees.nesting.today),color ="black", linewidth=1)+
  geom_line(data=sim_data, aes(Date, y=min.bees.nesting.today),color ="gray", linewidth=1)+
  geom_line(data=sim_data, aes(Date, y=max.bees.nesting.today),color ="gray", linewidth=1)+
  xlim(field_study_start, field_study_end)+
  ylim(0,max_nesting)+ # adjust y-axis limits: max. 150 for 2014; max. 60 for 2015
  xlab("Date") + ylab("Number of adult females in nest")+
  scale_shape_manual(values = c(16, 2))+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=20,face="bold") )+
  theme(legend.position = legend_pos) +
  theme_minimal(base_size = 15)
#ggsave(paste0('NestOcc_Ruddle_',study_year,'_',run_num, '_Mar2024.jpg'), study_nest, dpi=300, width = 7, height = 5)

# x-axis titles only included in bottom row, y-axis titles only in left column
xtext <- "Date"
ytext <- "Number of brood cells"
if(xlabel == 0){xtext <- NULL}
if(ylabel == 0){ytext <- NULL}

study_brood<-ggplot()+
  geom_line(data=sim_data, aes(x= Date, avg.sum.cells), color ="black", linewidth=1)+
  geom_line(data=sim_data, aes(x= Date, min.sum.cells), color ="gray", linetype="dotted",linewidth=0.5)+ #
  geom_line(data=sim_data, aes(x= Date, max.sum.cells), color ="gray", linetype="dotted",linewidth=0.5)+
  geom_ribbon(data=sim_data,
              aes(x=Date, ymin=min.sum.cells,ymax=max.sum.cells), fill="black", alpha = 0.1)+
  geom_point(data=cell_Prod, aes(Date, sum.cells, shape =study.type), size=1.2, color = 'grey45')+ #color = treatment, 
  xlim(field_study_start, field_study_end)+ 
  ylim(0,max_brood)+ # adjust y-axis limits dependent on study year
  xlab(xtext) + ylab(ytext) + labs(title = plot_title)+
  scale_shape_manual(values = c(1, 2))+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=20,face="bold"), 
        title=element_text(size=20,face="bold"))+
  theme_minimal(base_size = 15)+
  theme(legend.position = c(0.15,0.85), legend.title=element_blank())+
  theme(legend.background = element_rect(color="white",fill="white"))


