### SolBeePop ####################################################
### Calibration and validation
### Script for the analysis of simulation outputs (calibration, validation);
###  Calculation of NRMSE, NMAE, RSR
### Author: Amelie Schmolke / Brenna Kent
### Date: January 2022
###################################################################

library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggsci)
library(stringr)

rm(list = ls()) # clean up workspace

# Read in calibration or validation data ------------------------------------------------
sim_set_name = 'run_S15_01802_validation'
study_name = "S15-01802"

ofp = 'C:\\SolBeePop\\Calibration_Validation\\Validation'  # output file path

lv = c('var.emerge.f', 't.maturation',   # LHC vars
       'p.max.nesting.life', 'max.f.ratio', 'max.cells', 'max.survival.e.f',
       'a.sex.age')

ov = list('bees.emerged.yr', 'f.emerged.yr', 'm.emerged.yr',  # out vars 
          'bees.nesting.today', 'sum.cells', 'sum.f.cells', 'sum.m.cells')

## Get Data  ------------------------------------------------------------------
setwd(ofp)

ldf = read.csv(paste0(sim_set_name,'.csv'), stringsAsFactors = FALSE, check.names = FALSE)
ldf = ldf[c('name', lv)]

odf = data.frame(matrix(nrow = 0, ncol = length(ov) + 6))
colnames(odf) = c('name', 'RndSeed', 'X.step.','doy','year','DateREP', ov)

for(i in 1:nrow(ldf)){
    tdf = read.csv(paste0(ldf$name[i], '.csv'),
                 stringsAsFactors = FALSE, skip = 6)
  tdf$name = ldf$name[i]
  odf = rbind(odf, tdf[colnames(odf)])
}
rm(i, tdf)

odf = merge(ldf, odf)

write.csv(odf, paste0('outputs_',sim_set_name,'.csv'), row.names = FALSE)

# group and analyze simulation outputs  ----------------------------------------------------------

#odf <-read_csv(paste0('outputs_',sim_set_name,'.csv')) # only if previous section of this script had been run before (and output from 'odf' generate)

#fixing date
odf<-odf %>% 
  mutate(day = str_split_fixed(DateREP, " ",4)[,1],
         month = str_split_fixed(DateREP, " ",4)[,3],
         year=as.numeric(str_split_fixed(DateREP, " ",4)[,4])+2000,
         Date=dmy(paste0(day,"/",month,"/", year)))

# average value of 10 repetitions 
odf_summarize<-odf %>% 
  group_by(name,var.emerge.f, t.maturation,   # LHC vars
           p.max.nesting.life, max.f.ratio, max.cells, max.survival.e.f,
           a.sex.age, Date) %>% 
#  group_by(name,bees.emerged.yr, f.emerged.yr, m.emerged.yr,  
#           bees.nesting.today, sum.cells, sum.f.cells, sum.m.cells, Date) %>% 
  summarize(avg.bees.nesting.today = mean(bees.nesting.today), avg.sum.cells = mean(sum.cells), 
            avg.sum.f.cells = mean(sum.f.cells), avg.sum.m.cells = mean(sum.m.cells),
            avg.bees.emerged.yr = mean(bees.emerged.yr),
            avg.f.emerged.yr = mean(f.emerged.yr), avg.m.emerged.yr = mean(m.emerged.yr)) 
# year 1 (2001) becomes 2015 and year 1 (2002) becomes 2016
odf_summarize$Date_mod<-odf_summarize$Date+years(14) 


# Nesting females ---------------------------------------------------------

# Read in semi-field data
field_NO<-read_csv(paste0("C:/SolBeePop/Calibration_Validation/",study_name,"-01_Tables/NestOccup_",study_name,"-01.csv"))

# summing data
field_NO_summarize<-field_NO %>%
  group_by(date, treatment) %>% 
  summarise(female.sum = sum(female)) %>% 
  mutate(date =ymd(date))

# calculating average between replicates
field_NO_avg<-field_NO_summarize %>% 
  ungroup() %>% 
  mutate(overall.avg = mean(female.sum)) %>% 
  group_by(date, overall.avg) %>% 
  summarize(female.sum.avg = mean(female.sum)) 

Nesting.NRMSE <-odf_summarize %>% 
  ungroup() %>% 
  inner_join(field_NO_avg, by = c('Date_mod'= 'date')) %>% 
  group_by(name, var.emerge.f, t.maturation, p.max.nesting.life, max.f.ratio, 
           max.cells, max.survival.e.f, a.sex.age, overall.avg) %>% 
  summarize(Nesting.RMSE = sqrt(mean((female.sum.avg - avg.bees.nesting.today)^2))) %>% 
  ungroup() %>% 
  mutate(Nesting.NRMSE = Nesting.RMSE/overall.avg)

Nesting.MAE <-odf_summarize %>% 
  ungroup() %>% 
  inner_join(field_NO_avg, by = c('Date_mod'= 'date')) %>% 
  group_by(name,var.emerge.f, t.maturation, p.max.nesting.life, max.f.ratio, 
           max.cells, max.survival.e.f, a.sex.age, overall.avg) %>% 
  summarize(Nesting.MAE = mean(abs(female.sum.avg - avg.bees.nesting.today))) %>% 
  ungroup() %>% 
  mutate(Nesting.NMAE = Nesting.MAE/overall.avg)

Nesting.RSR <-odf_summarize %>% 
  ungroup() %>% 
  inner_join(field_NO_avg, by = c('Date_mod'= 'date')) %>% 
  group_by(name,var.emerge.f, t.maturation, p.max.nesting.life, max.f.ratio, 
           max.cells, max.survival.e.f, a.sex.age, overall.avg) %>% 
  summarize(Nesting.RSR = mean(sqrt((female.sum.avg - avg.bees.nesting.today)^2)/sqrt((female.sum.avg - overall.avg)^2))) 


# Brood cells -------------------------------------------------------------

# reading in cell production summary table
field_cellProd<-read_csv(paste0("C:/SolBeePop/Calibration_Validation/",study_name,"-01_Tables/CellProd_",study_name,"-01.csv"))

# summing cell production data for control groups by group and date
field_cellProd_summarize<-field_cellProd %>% 
  #filter(`treatment/replicate` %in% c('1B', '1C','1D','1E')) %>% 
  group_by(date, treatment) %>% 
  summarise(brood.sum = sum(sum.cells)) %>% 
  mutate(date =ymd(date))


field_cellProd_summarize$brood.sum.cum <-field_cellProd_summarize %>% 
  group_by(treatment) %>% 
  mutate(brood.sum.cum=cumsum(brood.sum))


field_cellProd_summarize <-field_cellProd_summarize %>% 
  group_by(treatment) %>% 
  mutate(brood.sum.cum=cumsum(brood.sum))

# calculating average between replicates
field_cellProd_avg<-field_cellProd_summarize %>% 
  ungroup() %>% 
  mutate(overall.avg = mean(brood.sum.cum)) %>% 
  group_by(date, overall.avg) %>% 
  summarize(brood.sum.cum.avg = mean(brood.sum.cum)) 

BroodCell.NRMSE <-odf_summarize %>% 
  ungroup() %>% 
  inner_join(field_cellProd_avg, by = c('Date_mod'= 'date')) %>% 
  group_by(name, var.emerge.f, t.maturation, p.max.nesting.life, max.f.ratio, 
           max.cells, max.survival.e.f, a.sex.age, overall.avg) %>% 
  summarize(BroodCells.RMSE = sqrt(mean((brood.sum.cum.avg - avg.sum.cells)^2))) %>% 
  ungroup() %>% 
  mutate(BroodCells.NRMSE = BroodCells.RMSE/overall.avg)

BroodCell.MAE <-odf_summarize %>% 
  ungroup() %>% 
  inner_join(field_cellProd_avg, by = c('Date_mod'= 'date')) %>% 
  group_by(name, var.emerge.f, t.maturation, p.max.nesting.life, max.f.ratio, 
           max.cells, max.survival.e.f, a.sex.age, overall.avg) %>% 
  summarize(BroodCells.MAE = mean(abs(brood.sum.cum.avg - avg.sum.cells))) %>% 
  ungroup() %>% 
  mutate(BroodCells.NMAE = BroodCells.MAE/overall.avg)

BroodCell.RSR <-odf_summarize %>% 
  ungroup() %>% 
  inner_join(field_cellProd_avg, by = c('Date_mod'= 'date')) %>% 
  group_by(name, var.emerge.f, t.maturation, p.max.nesting.life, max.f.ratio, 
           max.cells, max.survival.e.f, a.sex.age, overall.avg) %>% 
  summarize(BroodCells.RSR = mean(sqrt((brood.sum.cum.avg - avg.sum.cells)^2)/sqrt((brood.sum.cum.avg - overall.avg)^2))) 


# Offspring emergence -------------------------------------------------------------

# reading in emergence summary table
field_offspring<-read_csv(paste0("C:/SolBeePop/Calibration_Validation/",study_name,"-01_Tables/OffspringOverview_",study_name,"-01.csv"))
# note that offspring emergence numbers are totals per treatment

# calculating average between replicates
field_offspring_avg<-data.frame(mean(field_offspring$female.num),mean(field_offspring$male.num))
colnames(field_offspring_avg)<- c("f.offspring","m.offspring")
# calculate sex ratio of offspring emerged in the study
field_offspring_avg$sex.ratio <- field_offspring_avg$f.offspring/(field_offspring_avg$f.offspring+field_offspring_avg$m.offspring)

# simulation output: sum of emerged individuals corresponds to reported model output on the last date of the 2nd simulated year
odf_offspring <- subset(odf_summarize, odf_summarize$Date_mod == "2016-12-31")
odf_offspring$sex.ratio.emerged <- odf_offspring$avg.f.emerged.yr/(odf_offspring$avg.f.emerged.yr+odf_offspring$avg.m.emerged.yr)

Offspring.NRMSE <-odf_offspring %>% 
  ungroup() %>% 
  group_by(name, var.emerge.f, t.maturation, p.max.nesting.life, max.f.ratio, 
           max.cells, max.survival.e.f, a.sex.age) %>% 
  summarize(Offspring.RMSE = sqrt(mean(((field_offspring_avg$sex.ratio - sex.ratio.emerged)^2)))) %>% 
  ungroup() %>% 
  mutate(Offspring.NRMSE = Offspring.RMSE/(field_offspring_avg$sex.ratio))

Offspring.MAE <-odf_offspring %>% 
  ungroup() %>% 
  group_by(name, var.emerge.f, t.maturation, p.max.nesting.life, max.f.ratio, 
           max.cells, max.survival.e.f, a.sex.age) %>% 
  summarize(Offspring.MAE = mean(abs(field_offspring_avg$sex.ratio - sex.ratio.emerged))) %>% 
  ungroup() %>% 
  mutate(Offspring.NMAE = Offspring.MAE/(field_offspring_avg$sex.ratio))

# RSR cannot be calculated for single data points (and does not make sense for two dependent data points)

# Join data ---------------------------------------------------------------

GOFM<-BroodCell.NRMSE %>% 
  ungroup() %>% 
  left_join(BroodCell.MAE, by = c(lv,'name')) %>% 
  left_join(BroodCell.RSR, by = c(lv,'name')) %>% 
  left_join(Nesting.NRMSE, by = c(lv,'name')) %>% 
  left_join(Nesting.MAE, by = c(lv,'name')) %>% 
  left_join(Nesting.RSR, by = c(lv,'name')) %>%
  left_join(Offspring.NRMSE, by = c(lv,'name')) %>%
  left_join(Offspring.MAE, by = c(lv,'name'))
  
  
GOFM$Avg.NRMSE<- rowMeans(GOFM[,c('Nesting.NRMSE','BroodCells.NRMSE',"Offspring.NRMSE")])
GOFM$Avg.NMAE<- rowMeans(GOFM[,c('Nesting.NMAE','BroodCells.NMAE',"Offspring.NMAE")])
GOFM$Avg.RSR<- rowMeans(GOFM[,c('Nesting.RSR','BroodCells.RSR')])

# Output data -------------------------------------------------------------

write_csv(GOFM, paste0(ofp,"\\GOFM_",sim_set_name,".csv"))

# identify best runs

#GOFM <- read.csv(paste0(ofp,"_",sim_set_name,".csv"))
min_NRMSE <- min(GOFM$Avg.NRMSE)
best_NRMSE <- subset(GOFM, GOFM$Avg.NRMSE == min_NRMSE)
good_NRMSE <- subset(GOFM, GOFM$Avg.NRMSE < 0.25)          
