### SolBeePop_ecotox #########################################################
### Script for the calculation of proportion of foraging available per day 
###  based on weather data from semi-field studies from 2019 and 2021 (Eurofins data)
###  and O. bicornis foraging characterizations
### Script adapted from "Foraging_Landscape_InputSce_SolBeePop2022.R"
### Author: Amelie Schmolke
### October 2023
###################################################################

library(lubridate)
library(dplyr)
library(stringr)

rm(list = ls()) # clean up workspace

####### Scenario definitions ######################################
## Specific for species, weather, landscape, crop and exposure ####

### (1) Weather-related proportional foraging per day ###
# set working directory - adjust as applicable
setwd("C:/SolBeePop_ecotox/Calibration_Validation_Effects/")
# Output file name: note that the "meta" output includes the file name of this script. 
sce.file <-  "Floral_Eurofins2021_27Apr2023_control" #"Floral_Eurofins2019_26Apr2023_control" 
# weather data file; csv file needs to list weather data from a single year 
# two separate files for Eurofins studies:
# 1. Temperature and relative humidity
#weather.file1 <- "H:/SolBeePop/Semi-field data_Eurofins/Processed/Weather_data_Air_T_RH_Osmia_2019.csv" 
weather.file1 <- "H:/SolBeePop/Semi-field data_Eurofins/Processed/Weather_data_Osmia_2021_Air_T_RH.csv" 
# 2. Precipitation
#weather.file2 <- "H:/SolBeePop/Semi-field data_Eurofins/Processed/Weather_data_Rain_Osmia_2019.csv"
weather.file2 <- "H:/SolBeePop/Semi-field data_Eurofins/Processed/Weather_data_Osmia_2021_precip.csv"

wind.data <- FALSE # set to TRUE if the weather data includes wind speed, set to FALSE if not

# study info for study dates
#study.info <- read.csv(file="H:/SolBeePop/Semi-field data_Eurofins/Processed/Study_Treatments_Eurofins_Study1_2019.csv")
study.info <- read.csv(file="H:/SolBeePop/Semi-field data_Eurofins/Processed/Study_Treatments_Eurofins_Study2_2021.csv")

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
max.wind <- NA # unused due to lack of wind data
# maximum daily precipitation (mm) 
#  set to 25.4mm if unknown: general high value from Drummond 2016, Drummond et al. 2017
max.precip <- 25.4
# maximum humidity (%) (set to 100 if unknown)
max.humid <- 60
##

## if additional data are available:
## (B) scenario refinement: sunshine hours
use.sunshine <- 0 # set to 0 if unused or if the weather data does not include sunshine hours
# maximum foraging duration per day: using sunshine hours to compare to this value
max.forag.hrs <- NA # set use.sunshine = 0 if unknown

### (2) Crop flowering characterizations ###
## minimum parameters needed 
# without refinements: uniform flowering between start and end date; 
# resource quality from crop set to 1
# start date (day of year) of crop flowering
#crop.start <- NA # defined by study start date, see below
# end date (day of year) of crop flowering
#crop.end <- NA # defined by study end date, see below
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
min.crop.Q <- 1 
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

# getting the start and end date 
study_start<-mdy(study.info$Introduction[1])
study_start_doy<-study.info$Introduction.doy[1]
Study_end<-mdy(study.info$Last.day[1])
study_end_doy<-study.info$Last.day.doy[1]
Study_appl<-mdy(study.info$Application.date[1])

# reading weather data; weather data from a single year needs to be listed
d.weather.1 <- read.csv(file=weather.file1, skip = 1, header = TRUE, stringsAsFactors = FALSE)
#d.weather.1$Date <- as.Date(d.weather.1$Date, "%d.%m.%y") # NOTE: date format for 2019 
d.weather.1$Date <- as_date(d.weather.1$Date) # NOTE: date format for 2021

d.weather.1 <- subset(d.weather.1, d.weather.1$Date >= study_start)
d.weather.1 <- subset(d.weather.1, d.weather.1$Date <= Study_end)

d.weather.1.daily.av<-d.weather.1 %>%
  group_by(Date) %>% 
  summarise(T.avg = mean(Temp), T.max = max(Temp), av.RH = mean(RH), min.RH = min(RH))

d.precip.1 <- read.csv(file=weather.file2, skip = 1, header = TRUE, stringsAsFactors = FALSE)
#d.precip.1$Date <- as.Date(d.precip.1$Date, "%d.%m.%y") # NOTE: Date format adjusted in 2019 in csv file
d.precip.1$Date <- as_date(d.precip.1$Date) # NOTE: Date format adjusted in 2021 in csv file
d.precip.1 <- subset(d.precip.1, d.precip.1$Date >= study_start)
d.precip.1 <- subset(d.precip.1, d.precip.1$Date <= Study_end)
d.precip.1.daily <-d.precip.1 %>%
  group_by(Date) %>%
#  summarise(Temp = mean(Temp, na.rm=TRUE), Precip=max(Precipitation_cumulative)) # for 2019 data: precipitation given cumulative every day
  summarise(Temp = mean(Temp, na.rm=TRUE), Precip=sum(Precip, na.rm = TRUE)) # for 2021 data: precipitation given as amount (mm) per 'event'

d.weather <- d.weather.1.daily.av
d.weather$Precip <- d.precip.1.daily$Precip
d.weather$Precip[d.weather$Precip==""]<-0
d.weather$DOY <- seq(from=study_start_doy, to=study_end_doy, by=1)

####### SolBeePop input file generation ##############################
# Scenario data frame with 365 rows = days
sce.out <- data.frame(matrix(NA, nrow = 365, ncol = 10))
colnames(sce.out) <- c("doy",	"Prop_foraging_day",	"Quality_crop",	"Quality_nat", "Prop_foraging_crop",	
                       "Concentration_nectar",	"Concentration_pollen",	"Concentration_spray", 
                       "Concentration.nest.mat", "Concentration.leaf")
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
      if(d.weather$Precip[i] < max.precip){
        if(d.weather$min.RH[i] < max.humid){
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
  if(i >= study_start_doy){
    if(i <= study_end_doy){
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
  if(i >= study_start_doy){
    if(i <= study_end_doy){
      sce.out$Prop_foraging_crop[i] <- prop.crop
    }
  }
}

################ write file ####################
## generate file that is the input file to the population model
write.csv(sce.out, file=paste0("EffectsCalib_semifield/",sce.file,".csv"), row.names=FALSE)
## meta file: store characterizations set in this script
#### NOTE: update script name if needed! ####
meta.out <- data.frame(c("Script_version","Date_created", "Weather_file1","Weather_file2", "min.temp","max.wind", 
                         "max.precip","max.humid","use.sunshine","max.forag.hrs", 
                         "study_start_doy","study_end_doy","crop.peak","crop.Q", 
                         "min.crop.Q", "nat.Q", "prop.crop"), 
                       c("Weather_foraging_semifield_Apr2023.R", as.character(Sys.Date()), 
                         weather.file1, weather.file2, 
                         min.temp, max.wind, max.precip, max.humid, use.sunshine, 
                         max.forag.hrs, study_start_doy, study_end_doy, crop.peak, crop.Q,
                         min.crop.Q, nat.Q, prop.crop))
colnames(meta.out) <- c("Parameter", "Value")
write.csv(meta.out, file=paste0("EffectsCalib_semifield/",sce.file,"_meta.csv"), row.names=FALSE, quote=FALSE)


#### plot for demonstration
# plot average daily temperature from the two source files for reference
# temperature in the the second file (precip) is reported nearly consistently higher than in the other file
#  (patterns are very similar)
# using the first file (weather) for temperature for generation of SolBeePop input
jpeg(filename = "EffectsCalib_semifield/Temp2019_comp.jpg")
plot(x=d.weather.1.daily.av$Date, y=d.weather.1.daily.av$T.avg, type="l", col="blue",
     ylim=c(0,20), xlab="Date", ylab="Average temperature (deg. C)")
points(x=d.precip.1.daily$Date, y=d.precip.1.daily$Temp, type="l", col="darkgreen")
dev.off()

forag <- subset(sce.out,sce.out$doy >= study_start_doy)
forag <- subset(forag, forag$doy <= study_end_doy)
# plot daily temperature, humidity and rainfall with daily foraging
jpeg(filename = "Weather_foraging_semifield2021.jpg", width=9, height=5, units="in", res=300)
plot(x=d.weather$Date, y=d.weather$T.max, type="l", lty= "dashed", col="red",
     ylim=c(0,100), xlab="Date", ylab="Value", main="Weather 2021")
points(x=d.weather$Date, y=d.weather$min.RH, type="l", lty= "dashed", col="darkgreen")
points(x=d.weather$Date, y=d.weather$Precip, type="l", lty= "dashed", col="blue")
points(x=d.weather$Date, y=100*forag$Prop_foraging_day, type="l", lty= "solid", col="black", lwd=2)
legend(x=d.weather$Date[1]-1, y=95, c("Max. temperature (deg. C)", "Min. relative humidity (%)",
                               "Precipitation (mm)", "Foraging_prop (%)"), bty="n", cex = 0.6,
       lty=c("dashed","dashed","dashed","solid"), lwd = c(1,1,1,2), col=c("red", "darkgreen","blue", "black"))
dev.off()

