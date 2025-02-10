### SolBeePop ####################################################
### Script for the generation of a yearly input file for the model
### Specific for Ruddle et a. (2018) field studies:
###  Scenario (a)
###  - use site-specific weather data
###  - use study-specific dates and OSR flowering characterizations
### Author: Amelie Schmolke
### Last changed: 8 January 2023
###################################################################

rm(list = ls()) # clean up workspace

library("plyr")
library("lubridate")

####### Scenario definitions ######################################
## Specific for species, weather, landscape, crop and exposure ####

# set working directory - adjust as applicable
setwd("X:/Population Modelling Work/P2327120_2327121_SolBeePop/Landscape_application/Ruddle_data_analyzed/")
# Output file name: note that the "meta" output includes the file name of this script. 
sce.file <- "Floral_S15_01803_Tue2105_SceA_W8" 
# name of file containing the study characterizations (used for dates and study ID)
study.info.file <- "S15-01803_field_Tables/Study_Treatments_S15-01803_field.csv"
# File listing the OSR flowering by date, specific to study (site and year)
OSR.file <- "OSR_floral_function/OSR_BBCH_S15_01803_Tue2015.csv"
# first date in OSR flowering file as doy (manual input! comes from the OSR_BBCH file: start date does not correspond to study start date in all cases!)
osr.start.doy <- 113

# weather data; csv file needs to list weather data from a single year 
#weather.file1 <- "" # from study fields, used for precipitation
weather.file1 <- NA
weather.file2 <- "S15-01803_field_Tables/Weather_Unterjesingen_2015.csv" # closest weather station
wind.data <- FALSE # set to TRUE if the weather data includes wind speed, set to FALSE if not
use.sunshine <- 0 # set to 0 if unused or if the weather data does not include sunshine hours

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
##

## if additional data are available:
# maximum foraging duration per day: using sunshine hours to compare to this value
max.forag.hrs <- 10 # set use.sunshine = 0 if unknown

nat.Q <- 0 # resource availability from non-crop floral resources: set to generic value for scenario (a)
prop.crop <- 1 # proportion foraging on crop: set to generic value for scenario (a)

### End of scenario definitions ###############################################
### Note that some column names etc. are specific to study / weather file and 
###   have to be adjusted in the block below

### End of scenario definitions ###############################################
### Note that some column names etc. are specific to study / weather file and 
###   have to be adjusted in the block below

### (1) Weather-related proportional foraging per day (Prop_forag_day) #########################
# reading study information: used to extract study-specific dates
study.info <- read.csv(study.info.file)

#start <- as.Date(study.info$Introduction.r1[1],"%m/%d/%Y") # for 2014 files (with 2 cocoon introduction dates)
start <- as.Date(study.info$Introduction[1],"%m/%d/%Y") # for 2014 files (with 1 cocoon introduction date)
end <- as.Date(study.info$Last.day[1],"%m/%d/%Y")
studyID <- study.info$Study.ID[1]
studyID <- substr(studyID, start = 1, stop = 9)
#d.weather1 <- read.csv(file=weather.file1, stringsAsFactors = FALSE, sep=";")#, skip = 5) # skipping the first 4 rows in Ruddle et al. weather data files
#d.weather1$date <- as.Date(paste0(d.weather1$Day,"/",d.weather1$Month,"/",d.weather1$Year), "%d/%m/%Y")
#d.weather1 <- subset(d.weather1, d.weather1$date >= start & d.weather1$date <= end)

d.weather2 <- read.csv(file=weather.file2, sep=";", dec=",") # for 2014 files
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
names(d.weather)[names(d.weather) == 'MIN_RH200'] <- 'rel.humidity' # using minimum daily humidity for comparison with threshold
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
# resource availability from non-crop floral resources: set to generic value for scenario (a)
sce.out$Quality_nat <- nat.Q

### (4) Proportion foraging on crop (Prop_forag_crop) ####################
# generic time series in scenario (a), set by prop.crop in scenario definitions
# prop.crop is the proportion of foraging on crop, the remainder of foraging (if any) 
#    assumed to occur in non-crop areas
sce.out$Prop_foraging_crop <- 0
for(i in 1:(length(sce.out$Prop_foraging_crop))){
  if(i >= min.doy){
    if(i <= study.end){
      sce.out$Prop_foraging_crop[i] <- prop.crop
    }
  }
}

################ write file ####################
## generate file that is the input file to the population model
write.csv(sce.out, file=paste0("../landscape_inputs/",sce.file,".csv"), row.names=FALSE)
## meta file: store characterizations set in this script
#### NOTE: update script name if needed! ####
meta.out <- data.frame(c("Script_version","Date_created", "Study info file", "Weather_file1","Weather_file2",
                         "OSR_flowering_file",
                         "wind.data", "min.temp","max.wind", 
                         "max.precip","max.humid","use.sunshine","max.forag.hrs", 
                         "osr.start.doy","study.end", "nat.Q", "prop.crop"), 
                       c("Ruddle_field_InputSceA.R", as.character(Sys.Date()), study.info.file,
                         weather.file1, weather.file2, OSR.file, wind.data, 
                         min.temp, max.wind, max.precip, max.humid, use.sunshine, 
                         max.forag.hrs, osr.start.doy, study.end, nat.Q, prop.crop))
colnames(meta.out) <- c("Parameter", "Value")
write.csv(meta.out, file=paste0("../landscape_inputs/",sce.file,"_meta.csv"), row.names=FALSE, quote=FALSE)


