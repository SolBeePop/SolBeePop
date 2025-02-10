### SolBeePop ####################################################
### Script for the generation of a yearly input file for the model
### Specific for Ruddle et a. (2018) field studies:
###  Scenario (c)
###  - use site-specific weather data
###  - use study-specific dates and OSR flowering characterizations
###  - use site-specific land cover data (area or distance to relevant land covers and resource phenology)
###     Note: script generates two input files, one based on distance and one on proportion area 
###  - use study-specific proportion of Brassica (OSR) pollen in provision samples for Prop_foraging_crop
### Author: Amelie Schmolke
### Last changed: 04 Jan 2024
###################################################################

rm(list = ls()) # clean up workspace

library("plyr")
library("lubridate")
library("dplyr")

####### Scenario definitions ######################################
## Specific for species, weather, landscape, crop and exposure ####

# set working directory - adjust as applicable
setwd("X:/Population Modelling Work/P2327120_2327121_SolBeePop/Landscape_application/Ruddle_data_analyzed/")
# Output file name: note that the "meta" output includes the file name of this script. 
sce.file <- "Floral_S15_01803_Tue2105_SceC_1200m_W8" 
# name of file containing the study characterizations (used for dates and study ID)
study.info.file <- "S15-01803_field_Tables/Study_Treatments_S15-01803_field.csv"
# radius of landscape quantified around study site (location of nest boxes); radii analyzed: 400, 800, 1200m
foraging.radius <- 1200 # m
OSR.file <- "OSR_floral_function/OSR_BBCH_S15_01803_Tue2015.csv"
# first date in OSR flowering file as doy (manual input!)
osr.start.doy <- 113

pollen.composition.file <- "Pollen_data.csv"

# weather data; csv file needs to list weather data from a single year 
#weather.file1 <- "Weather_S15_01803.csv" # from study fields, used for precipitation
weather.file1 <- NA
weather.file2 <- "S15-01803_field_Tables/Weather_Unterjesingen_2015.csv" # closest weather station
wind.data <- FALSE # set to TRUE if the weather data includes wind speed, set to FALSE if not
use.sunshine <- 0 # set to 0 if unused or if the weather data does not include sunshine hours

# land cover data 
land_cover_prop.file <- "Results_landuse_proportions.csv"
land_cover_dist.file <- "Results_landuse_distances.csv"
# land cover quality factor
Q_factor.file <- "Landcover_factor_table.csv"
Q_factor <- read.csv(Q_factor.file, stringsAsFactors = FALSE)

## Species-specific characterizations of weather-related foraging ##
# Proportion of daily foraging time available due to weather is based on threshold assumptions
# Refined scenarios (B) assumes sunshine needed for foraging
# NOT considered: continuous relationships between temperature, precipitation, humidity, wind and foraging time

## parameter values in this block have to be set for simplest scenario generation
# minimum temperature (Celsius) for foraging
#  Default for Osmia: 10C; 4.4C identified as absolute minimum across species in blueberries
#  (Drummond 2016, Drummond et al. 2017)
min.temp <- 18
# maximum wind speed (m/s) for foraging
#  set to 8.9 if unknown (Drummond 2016, Drummond et al. 2017)
max.wind <- 7.2
# maximum daily precipitation (mm) 
#  set to 25.4mm if unknown: general high value from Drummond 2016, Drummond et al. 2017
max.precip <- 25.4
# maximum humidity (%) (set to 100 if unknown)
max.humid <- 60

## if additional data are available:
# maximum foraging duration per day: using sunshine hours to compare to this value
max.forag.hrs <- 10 # set use.sunshine = 0 if unknown

### End of scenario definitions ###############################################
### Note that some column names etc. are specific to study / weather file and 
###   have to be adjusted in the block below

### (1) Weather-related proportional foraging per day (Prop_forag_day) #########################
# reading study information: used to extract study-specific dates
study.info <- read.csv(study.info.file)

#start <- as.Date(study.info$Introduction.r1[1],"%m/%d/%Y") # for 2014 files (with 2 cocoon introduction dates)
start <- as.Date(study.info$Introduction[1],"%m/%d/%Y") # for 2015 files (with 1 cocoon introduction date)
end <- as.Date(study.info$Last.day[1],"%m/%d/%Y")
studyID <- study.info$Study.ID[1]
studyID <- substr(studyID, start = 1, stop = 9)
#d.weather1 <- read.csv(file=weather.file1, stringsAsFactors = FALSE, sep=";")#, skip = 5) # skipping the first 4 rows in Ruddle et al. weather data files
#d.weather1$date <- as.Date(paste0(d.weather1$Day,"/",d.weather1$Month,"/",d.weather1$Year), "%d/%m/%Y")
#d.weather1 <- subset(d.weather1, d.weather1$date >= start & d.weather1$date <= end)

d.weather2 <- read.csv(file=weather.file2, sep=";", dec=",") # for 2014 files / weather data from BW weather stations
d.weather2$date <- as.Date(d.weather2$Tag, "%d.%m.%Y") # 
d.weather2$DOY <- seq(from=1, to=length(d.weather2$date), by=1) # only for 2014 files (2015 weather files already include a column with DOY)
#d.weather2 <- read.csv(file=weather.file2, skip = 5) # for 2015 files
#d.weather2$date <- as.Date(d.weather2$Date, "%Y-%m-%d") #
d.weather2 <- subset(d.weather2, d.weather2$date >= start & d.weather2$date <= end)
d.weather <- d.weather2
# renaming columns in Ruddle et al. weather files: remove or adjust for other weather files as applicable
names(d.weather)[names(d.weather) == 'MAX_TA200'] <- 'T.max' # from weather station # for 2014 files
names(d.weather)[names(d.weather) == 'SUM_NN050'] <- 'Precipitation' # from field measurement
#names(d.weather)[names(d.weather) == 'AVG_RH200'] <- 'rel.humidity'
names(d.weather)[names(d.weather) == 'MIN_RH200'] <- 'rel.humidity'
#names(d.weather)[names(d.weather) == 'Max..air.temp.'] <- 'T.max' # from weather station # for 2015 files
#names(d.weather)[names(d.weather) == 'Pre.cipitation'] <- 'Precipitation' # from field measurement
#names(d.weather)[names(d.weather) == 'Relative.air.humidity'] <- 'rel.humidity'

# Scenario data frame with 365 rows = days
sce.out <- data.frame(matrix(NA, nrow = 365, ncol = 5))
colnames(sce.out) <- c("doy",	"Prop_foraging_day",	"Quality_crop",	"Quality_nat", "Prop_foraging_crop")
sce.out$doy <- seq.int(from=1,to=365,by=1)

### Weather-related foraging scenario (column 2 = Forag_prop_day)
sce.out$Prop_foraging_day <- 0

# if weather data is not defined for the whole year, assume no foraging 
# on days without weather data
min.doy <- d.weather$DOY[1]
max.doy <- d.weather$DOY[length(d.weather$DOY)]

## simplest scenario: days assigned to foraging (1) or no foraging (0) based on thresholds defined in block (A)
if(wind.data == TRUE){
  for(i in 1:(max.doy-min.doy+1)){
    if(d.weather$T.max[i] > min.temp){
      if(d.weather$Wind.mean[i] < max.wind){
        if(d.weather$Precipitation[i] < max.precip){
          if(d.weather$rel.humidity[i] < max.humid){
            sce.out$Prop_foraging_day[d.weather$DOY[i]] <- 1
          }
        }
      }
    }
  }
}

if(wind.data == FALSE){
  for(i in 1:(max.doy-min.doy+1)){
    if(d.weather$T.max[i] > min.temp){       
      if(d.weather$Precipitation[i] < max.precip){
        if(d.weather$rel.humidity[i] < max.humid){
          sce.out$Prop_foraging_day[d.weather$DOY[i]] <- 1
        }
      }
    }
  }
}

## use of sunshine hours based on maximum foraging hours per day (B)
if(use.sunshine == 1){
  for(i in 1:(max.doy-min.doy+1)){
    if(sce.out$Prop_foraging_day[d.weather$DOY[i]] == 1){
      if(d.weather$Sun.hrs[i] < max.forag.hrs){
        sce.out$Prop_foraging_day[d.weather$DOY[i]] <- d.weather$Sun.hrs[i]/max.forag.hrs
      }
    }
  }
}

### (2) Crop flowering characterizations: from OSR input (Quality_crop) ########
## Floral resource availability from crop 
# end date (day of year) of crop flowering
study.end <- study.info$Last.day.doy[1]
# Oilseed rape (OSR) flowering 
osr.in <- read.csv(OSR.file, skip = 3)

# interpolation of OSR proportional flowering
osr <- data.frame(Date = seq(as.Date(osr.in$Date[1]), as.Date(osr.in$Date[length(osr.in$Date)]), by = "days"))
osr$DOY = seq(from = osr.start.doy, to = osr.start.doy+length(osr$Date)-1, by = 1)
osr$prop_floral <- NA
for(i in 1:length(osr$Date)){
  for(j in 1:length(osr.in$Date)){
    if(osr$Date[i] == osr.in$Date[j]){
      osr$prop_floral[i] <- osr.in$Proportional.resource.availability[j]
    }
  }
}
for(i in 1:length(osr$Date)){
  if(is.na(osr$prop_floral[i])){
    osr$prop_floral[i] <- approx(osr$DOY, osr$prop_floral, xout=osr$DOY[i])[2]
  }
}

## OSR flowering time series within study field phase dates
### Crop daily flowering quality (column 3 = Quality_crop)
sce.out$Quality_crop <- 0
# linear increase from start to peak and decrease from peak to end assumed
for(i in 1:(length(sce.out$Quality_crop))){
  for(j in 1:length(osr$DOY)){
    if(sce.out$doy[i] == osr$DOY[j]){ 
      sce.out$Quality_crop[i] <- osr$prop_floral[j]
    }
  }
}
sce.out$Quality_crop <- round(as.numeric(sce.out$Quality_crop), digits=2)

### (3) Non-crop flowering characterizations (Quality_nat) ########
## Floral resource availability from identified land covers in the landscape 
##    (deciduous trees, permanent herbaceous land covers)
# Quality_nat generated in two ways:
#  1) based on proportion of land cover types within foraging radius
prop_sce.out <- sce.out

land_cover_prop <- read.csv(land_cover_prop.file)
land_cover_prop <- subset(land_cover_prop, land_cover_prop$Buffer.radius == foraging.radius)
land_cover_prop <- subset(land_cover_prop, land_cover_prop$ID == studyID)

# proportion of the relevant land cover types; convert from % to proportion
lcp_Q_trees <- land_cover_prop$Woody.broadleaved.decidious.trees/100
lcp_Q_herb <- land_cover_prop$Permanent.herbaceous/100

nat.Q <- data.frame(Date = Q_factor$Date, DOY = Q_factor$doy)
nat.Q$Date <- dmy(paste0(Q_factor$Date,"-",study.info$Study.year[1]))
nat.Q$prop_floral <- Q_factor$factor_trees * lcp_Q_trees + Q_factor$factor_herbs * lcp_Q_herb
nat.Q$prop_floral <- round(as.numeric(nat.Q$prop_floral), digits=2)

prop_sce.out$Quality_nat <- 0
# linear increase from start to peak and decrease from peak to end assumed
for(i in 1:(length(prop_sce.out$Quality_nat))){
  for(j in 1:length(nat.Q$DOY)){
    if(prop_sce.out$doy[i] == nat.Q$DOY[j]){ 
      prop_sce.out$Quality_nat[i] <- nat.Q$prop_floral[j]
    }
  }
}

#  2) based on distance to nearest land cover
dist_sce.out <- sce.out

land_cover_dist <- read.csv(land_cover_dist.file)
land_cover_dist <- subset(land_cover_dist, land_cover_dist$ID == studyID)
# proportion of the relevant land cover types; convert from % to proportion
lcd_Q_trees <- 1-(land_cover_dist$Distance.to.forest/foraging.radius)
lcd_Q_herb <- 1-(land_cover_dist$Distance.to.grass.land/foraging.radius)

nat.Q <- data.frame(Date = Q_factor$Date, DOY = Q_factor$doy)
nat.Q$Date <- dmy(paste0(Q_factor$Date,"-",study.info$Study.year[1]))
#nat.Q$prop_floral <- apply(max((Q_factor$factor_trees * lcd_Q_trees), (Q_factor$factor_herbs * lcd_Q_herb))
tmp_df <- data.frame(trees = Q_factor$factor_trees * lcd_Q_trees, herbs = Q_factor$factor_herbs * lcd_Q_herb)
nat.Q$prop_floral <- apply(tmp_df,1,max)
nat.Q$prop_floral <- round(as.numeric(nat.Q$prop_floral), digits=2)

dist_sce.out$Quality_nat <- 0
# linear increase from start to peak and decrease from peak to end assumed
for(i in 1:(length(dist_sce.out$Quality_nat))){
  for(j in 1:length(nat.Q$DOY)){
    if(dist_sce.out$doy[i] == nat.Q$DOY[j]){ 
      dist_sce.out$Quality_nat[i] <- nat.Q$prop_floral[j]
    }
  }
}

### (4) Proportion foraging on crop (Prop_forag_crop) ####################
# prop.crop is the proportion of foraging on crop, the remainder of foraging (if any) 
#    assumed to occur in non-crop areas
# Scenario C: directly derived from reported proportion of Brassica (OSR) pollen in provisions 
#    (from Ruddle et al. 2018, site-specific data)

# pollen composition reported by Ruddle et al. (2018) (proportion of pollen by plant species, date, and study)
pollen_OSR <- read.csv(pollen.composition.file, stringsAsFactors = FALSE)
pollen_OSR <- pollen_OSR %>%
  mutate(Date =dmy(Date))
# use only the site-specific data for Brassica (OSR) pollen proportion
pollen_OSR <- subset(pollen_OSR, pollen_OSR$Pollen_species == "Brassica")
pollen_OSR <- subset(pollen_OSR, pollen_OSR$Study_ID == studyID)
# new data frame used in the linear interpolation of reported site-specific pollen proportion data
posr <- data.frame(Date = seq(start, end, by = "days"))
posr$DOY = seq(from = min.doy, to = max.doy, by = 1)
posr$prop_floral <- NA
# filling in the data from the reported dates (3-4 per study location)
for(i in 1:length(posr$Date)){
  for(j in 1:length(as.Date(pollen_OSR$Date))){
    if(posr$Date[i] == pollen_OSR$Date[j]){
      posr$prop_floral[i] <- pollen_OSR$Proportion[j]
    }
  }
}
# for the interpolation of dates in-between reported proportions, the proportion OSR on the first and last date of the field phase are required 
# applying generic assumption of first day: 0.5 * first sample proportion; last day: 0.5 * last sample proportion
posr$prop_floral[1] <- pollen_OSR$Proportion[1] / 2
posr$prop_floral[length(posr$prop_floral)] <- pollen_OSR$Proportion[length(pollen_OSR$Proportion)] / 2
for(i in 1:length(posr$Date)){
  if(is.na(posr$prop_floral[i])){
    posr$prop_floral[i] <- approx(posr$DOY, posr$prop_floral, xout=posr$DOY[i])[2]
  }
}

prop_sce.out$Prop_foraging_crop <- 0
for(i in 1:(length(prop_sce.out$Prop_foraging_crop))){
  for(j in 1:length(posr$DOY)){
    if(prop_sce.out$doy[i] == posr$DOY[j]){ 
      prop_sce.out$Prop_foraging_crop[i] <- posr$prop_floral[j]
    }
  }
}
prop_sce.out$Prop_foraging_crop <- round(as.numeric(prop_sce.out$Prop_foraging_crop), digits=2)

dist_sce.out$Prop_foraging_crop <- 0
for(i in 1:(length(dist_sce.out$Prop_foraging_crop))){
  for(j in 1:length(posr$DOY)){
    if(dist_sce.out$doy[i] == posr$DOY[j]){ 
      dist_sce.out$Prop_foraging_crop[i] <- posr$prop_floral[j]
    }
  }
}
dist_sce.out$Prop_foraging_crop <- round(as.numeric(dist_sce.out$Prop_foraging_crop), digits=2)

################# write SolBeePop input files ####################
## generate file that is the input file to the population model
# land cover proportion 
write.csv(prop_sce.out, file=paste0("../landscape_inputs/",sce.file,"_prop.csv"), row.names=FALSE)
## meta file: store characterizations set in this script
#### NOTE: update script name if needed! ####
meta.out <- data.frame(c("Script_version","Date_created", "Study info file", "Weather_file1","Weather_file2",
                         "OSR_flowering_file", "Landcover_prop_file", "Landcover_factor_file",
                         "Pollen_composition_file", 
                         "wind.data", "min.temp","max.wind", 
                         "max.precip","max.humid","use.sunshine","max.forag.hrs", 
                         "osr.start.doy","study.end", "foraging.radius"), 
                       c("Ruddle_field_InputSceC.R", as.character(Sys.Date()), 
                         study.info.file, weather.file1, weather.file2, 
                         OSR.file, land_cover_prop.file, Q_factor.file,
                         pollen.composition.file,
                         wind.data, min.temp, max.wind, 
                         max.precip, max.humid, use.sunshine, max.forag.hrs, 
                         osr.start.doy, study.end, foraging.radius))
colnames(meta.out) <- c("Parameter", "Value")
write.csv(meta.out, file=paste0("../landscape_inputs/",sce.file,"_prop_meta.csv"), row.names=FALSE, quote=FALSE)

# distance to land covers
write.csv(dist_sce.out, file=paste0("../landscape_inputs/",sce.file,"_dist.csv"), row.names=FALSE)
## meta file: store characterizations set in this script
#### NOTE: update script name if needed! ####
meta.out <- data.frame(c("Script_version","Date_created", "Study info file", "Weather_file1","Weather_file2",
                         "OSR_flowering_file", "Landcover_dist_file", "Landcover_factor_file",
                         "Pollen_composition_file", 
                         "wind.data", "min.temp","max.wind", 
                         "max.precip","max.humid","use.sunshine","max.forag.hrs", 
                         "osr.start.doy","study.end", "foraging.radius"), 
                       c("Ruddle_field_InputSceC.R", as.character(Sys.Date()), 
                         study.info.file, weather.file1, weather.file2, 
                         OSR.file, land_cover_dist.file, Q_factor.file,
                         pollen.composition.file,
                         wind.data, min.temp, max.wind, 
                         max.precip, max.humid, use.sunshine, max.forag.hrs, 
                         osr.start.doy, study.end, foraging.radius))
colnames(meta.out) <- c("Parameter", "Value")
write.csv(meta.out, file=paste0("../landscape_inputs/",sce.file,"_dist_meta.csv"), row.names=FALSE, quote=FALSE)


