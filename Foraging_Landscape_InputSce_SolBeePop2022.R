### SolBeePop ####################################################
### Script for the generation of a yearly input file for the model 
### Author: Amelie Schmolke
### Date: January 2022
###################################################################

####### Scenario definitions ######################################
## Specific for species, weather, landscape, crop and exposure ####

### (1) Weather-related proportional foraging per day ###
# set working directory - adjust as applicable
setwd("C:/SolBeePop/Calibration_Validation/S15-01804-01_Tables")
# Output file name: note that the "meta" output includes the file name of this script. 
sce.file <- "Floral_S15_01804_Sce3_Jan2022"
# reading weather data; csv file needs to list weather data from a single year 
weather.file <- "Weather_Ruddle_et_al_study_2015_Cel.csv"
d.weather <- read.csv(file=weather.file, stringsAsFactors = FALSE, skip = 5) # skipping the first 4 rows in Ruddle et al. weather data files
# renaming columns in Ruddle et al. weather files: remove or adjust for other weather files as applicable
names(d.weather)[names(d.weather) == 'Max..air.temp.'] <- 'T.max'
names(d.weather)[names(d.weather) == 'Pre.cipitation'] <- 'Precipitation'
names(d.weather)[names(d.weather) == 'Relative.air.humidity'] <- 'rel.humidity'

wind.data <- FALSE # set to TRUE if the weather data includes wind speed, set to FALSE if not

## Species-specific characterizations of weather-related foraging ##
# Proportion of daily foraging time available due to weather is based on threshold assumptions
# Refined scenarios (B) assumes sunshine needed for foraging
# NOT considered: continuous relationships between temperature, precipitation, humidity, wind and foraging time

## (A) parameter values in this block have to be set for simplest scenario generation
# minimum temperature (Celsius) for foraging
#  Default for Osmia: 10C; 4.4C identified as absolute minimum across species in blueberries
#  (Drummond 2016, Drummond et al. 2017)
min.temp <- 10
# maximum wind speed (m/s) for foraging
#  set to 8.9 if unknown (Drummond 2016, Drummond et al. 2017)
max.wind <- 7.2
# maximum daily precipitation (mm) 
#  set to 25.4mm if unknown: general high value from Drummond 2016, Drummond et al. 2017
max.precip <- 25.4
# maximum humidity (%) (set to 100 if unknown)
max.humid <- 78
##

## if additional data are available:
## (B) scenario refinement: sunshine hours
use.sunshine <- 0 # set to 0 if unused or if the weather data does not include sunshine hours
# maximum foraging duration per day: using sunshine hours to compare to this value
max.forag.hrs <- 10 # set use.sunshine = 0 if unknown

### (2) Crop flowering characterizations ###
## minimum parameters needed 
# without refinements: uniform flowering between start and end date; 
# resource quality from crop set to 1
# start date (day of year) of crop flowering
crop.start <- 119
# end date (day of year) of crop flowering
crop.end <- 155
# crop flowering follows triangular distribution
# date (day of year) of peak flowering 
# set to 0 if unknown
crop.peak <- 0
# crop resource quality (at peak flowering):
# value required between 0: crop provides no bee resources, 
# and 1: crop provides optimal bee resources (at peak flowering)
# set to 1 if unknown
crop.Q <- 1
# 10% of peak crop flowering quality assumed on first and last day of flowering (change if data are available)
min.crop.Q <- 0.1*crop.Q 
# non-crop resource quality (simplest scenario: constant, set to 1)
# possible alternatives/additions to reflect landscape composition:
#   e.g., calculate from distance of nearest (or average) resource patch to assumed nest site: within a few meters of nest site: 1, beyond foraging range: 0
# possible alternatives/additions to reflect temporal variability:
#   a) community-level flowering phenology of semi-natural areas (provide time series)
#   b) flowering phenology of preferred flowers (e.g., oak for Osmia, based on provision pollen composition analysis, provide time series)
# Note that for E. pruinosa, nat.Q = 0 because they are specialized on cucurbit crops only
nat.Q <- 0
# proportion foraging on crop (limited to period of crop flowering)
# assumption: constant during crop flowering, can be based on landscape composition within foraging range
# possible alternatives:
#   time series based on provision pollen composition
prop.crop <- 1

####### end scenario definitions ###########################

####### SolBeePop input file generation ##############################
# Scenario data frame with 365 rows = days
sce.out <- data.frame(matrix(NA, nrow = 365, ncol = 8))
colnames(sce.out) <- c("doy",	"Prop_foraging_day",	"Quality_crop",	"Quality_nat", "Prop_foraging_crop",	
                       "Nectar_conc",	"Pollen_conc",	"Spray_conc")
sce.out$doy <- seq.int(from=1,to=365,by=1)

### Weather-related foraging scenario (column 2)
sce.out$Prop_foraging_day <- 0

# if weather data is not defined for the whole year, assume no foraging 
# on days without weather data
min.doy <- d.weather$DOY[1]
max.doy <- d.weather$DOY[length(d.weather$DOY)]

## simplest scenario: days assigned to foraging (1) or no foraging (0) based on thresholds defined in block (A)
if(wind.data == TRUE){
  for(i in 1:(max.doy-min.doy)){
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
  for(i in 1:(max.doy-min.doy)){
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
  for(i in 1:(max.doy-min.doy)){
    if(sce.out$Prop_foraging_day[d.weather$DOY[i]] == 1){
      if(d.weather$Sun.hrs[i] < max.forag.hrs){
        sce.out$Prop_foraging_day[d.weather$DOY[i]] <- d.weather$Sun.hrs[i]/max.forag.hrs
      }
    }
  }
}

### Crop daily flowering quality (column 3)
lin.crop.fowering <- function(day, start, peak, end, min.Q, max.Q){
  if(day < start){
    return(0)
  }
  if(day <= peak){
    return((day-start)*(max.Q-min.Q)/(peak-start)+min.Q)
  }
  if(day <= end){
    return((day-peak)*(min.Q-max.Q)/(end-peak)+max.Q)
  }
  if(day > end){
    return(0)
  }
}

sce.out$Quality_crop <- 0
# linear increase from start to peak and decrease from peak to end assumed
for(i in 1:(length(sce.out$Quality_crop))){
  if(i >= crop.start){
   if(i <= crop.end){
     if(crop.peak == 0){
       sce.out$Quality_crop[i] <- crop.Q
     }
     if(crop.peak > 0){
       sce.out$Quality_crop[i] <- lin.crop.fowering(i,crop.start, crop.peak, crop.end,
                                                    min.crop.Q, crop.Q)
     }
   }
 }
}

### Non-crop daily flowering quality (column 4)
# Note: no time-variable scenario generation implemented (yet)
sce.out$Quality_nat <- nat.Q

### Proportion foraging on crop
# Remainder of foraging assumed to occur in non-crop areas
sce.out$Prop_foraging_crop <- 0
for(i in 1:(length(sce.out$Prop_foraging_crop))){
  if(i >= crop.start){
    if(i <= crop.end){
      sce.out$Prop_foraging_crop[i] <- prop.crop
    }
  }
}

################ write file ####################
## generate file that is the input file to the population model
write.csv(sce.out, file=paste0(sce.file,".csv"), row.names=FALSE)
## meta file: store characterizations set in this script
#### NOTE: update script name if needed! ####
meta.out <- data.frame(c("Script_version","Date_created", "Weather_file","wind.data", "min.temp","max.wind", 
                         "max.precip","max.humid","use.sunshine","max.forag.hrs", 
                         "crop.start","crop.end","crop.peak","crop.Q", 
                         "min.crop.Q", "nat.Q", "prop.crop"), 
                       c("Foraging_Landscape_InputSce_SolBeePop2022.R", as.character(Sys.Date()), 
                         weather.file, wind.data, 
                         min.temp, max.wind, max.precip, max.humid, use.sunshine, 
                         max.forag.hrs, crop.start, crop.end, crop.peak, crop.Q,
                         min.crop.Q, nat.Q, prop.crop))
colnames(meta.out) <- c("Parameter", "Value")
write.csv(meta.out, file=paste0(sce.file,"_meta.csv"), row.names=FALSE, quote=FALSE)


