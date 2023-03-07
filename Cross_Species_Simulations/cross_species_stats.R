### SolBeePop ####################################################
### Cross-species simulations
### Script for calculating population properties
### Author: Amelie Schmolke / Colleen Roy
### Date: December 2022
###################################################################

rm(list = ls()) # clean up workspace
library(plyr)
library(reshape2)

## Inputs ---------------------------------------------------------------------
ofp = 'C:\\SolBeePop\\Cross_Species_Simulations'  # output file path

## Set Up ---------------------------------------------------------------------
setwd(ofp)
sls = c(
  'Osmia bicornis' = 'osmia_bicornis.csv',
  'Megachile rotundata' = 'megachile_rotundata.csv',
  'Nomia melanderi' = 'nomia_melanderi.csv',
  'Eucera pruinosa' = 'eucera_pruinosa.csv')
ols = c('bees.emerged.yr', 'f.emerged.yr', 'm.emerged.yr', 'bees.nesting',
  'bees.nesting.today', 'sum.cells.today', 'sum.f.cells.today',
  'sum.m.cells.today', 'sum.cells', 'sum.f.cells', 'sum.m.cells',
  'mean.cells.today', 'mean.f.cells.today', 'mean.m.cells.today',
  'mean.cells', 'mean.f.cells', 'mean.m.cells')

## Read simulation outputs -----------------------------------------------------------------
df = data.frame(matrix(nrow = 0, ncol = length(ols) + 4))
colnames(df) = c('sp', 'RndSeed', 'year', 'doy', ols)

for(i in names(sls)){
  tdf = read.csv(sls[[i]], stringsAsFactors = FALSE, skip = 6)
  tdf$sp = i
  df = rbind(df, tdf[colnames(df)])
  rm(tdf)
}
rm(i)

df$sex.ratio = df$sum.f.cells / df$sum.cells
df = df[df$doy == 365,]  # last day of year

# output set up
odf = data.frame(matrix(nrow = 0, ncol = 6))
colnames(odf) = c('Statistic', 'Years',
  'Osmia bicornis', 'Megachile rotundata',
  'Nomia melanderi', 'Eucera pruinosa')

## Carrying Capacity/Population Size  -----------------------------------------
# Dataset for years 21-30 together and 21-30 individually
tdf = df[df$year %in% 21:30,]
tdf$year = '21-30'
tdf = rbind(tdf, df[df$year %in% 21:30,])

# Carrying Capacity + Min/Max Population
tdf = ddply(tdf, .(sp, year), summarize,
  CarryingCapacity = mean(sum.cells),
  MinPopSize = min(sum.cells), MaxPopSize = max(sum.cells))

# Reformat and add to output df
tdf = melt(tdf, id.vars = c('sp', 'year'))
tdf = dcast(tdf, variable + year ~ sp)
colnames(tdf)[1:2] = c('Statistic', 'Years')
odf = rbind(odf, tdf[colnames(odf)])

## Years to Carrying Capacity  ------------------------------------------------
# Carrying Capacity Value as Column
rdf = odf[odf$Statistic == 'CarryingCapacity' & odf$Years == '21-30',]
rdf = melt(rdf, id.vars = c('Statistic', 'Years'))
colnames(rdf)[3:4] = c('sp', 'CarryingCapacity')
tdf = merge(df, rdf[3:4])

# Calculate Year for each seed that sum.cells > CarryingCapacity
tdf = ddply(tdf, .(sp, RndSeed), summarize,
  YearsToCarryingCapacity = min(year[sum.cells > CarryingCapacity]))

# check for sims that never reach carrying capacity
tdf[is.infinite(tdf$YearsToCarryingCapacity),]

# Min/Mean/Max YearsTo & Count for 20+
tdf = ddply(tdf, .(sp), summarize,
  MeanYearsToCarryingCapacity =
    mean(YearsToCarryingCapacity[!is.infinite(YearsToCarryingCapacity)]),
  MinYearsToCarryingCapacity = min(YearsToCarryingCapacity),
  MaxYearsToCarryingCapacity = 
    max(YearsToCarryingCapacity[!is.infinite(YearsToCarryingCapacity)]),
  Cnt20YearsToCarryingCapacity = sum(YearsToCarryingCapacity >= 20))

# Reformat and add to output df
tdf = melt(tdf, id.vars = c('sp'))
tdf$year = NA
tdf = dcast(tdf, variable + year ~ sp)
colnames(tdf)[1:2] = c('Statistic', 'Years')
odf = rbind(odf, tdf[colnames(odf)])

## Sex Ratio / Reprod Rate  ---------------------------------------------------
# Dataset for years 1 & 30 individually
tdf = df[df$year %in% c(1, 30),]

# Sex Ratio & Reproductive Rate
tdf = ddply(tdf, .(sp, year), summarize,
  SexRatio = mean(sex.ratio), ReproductiveRate = mean(mean.cells))

# Reformat and add to output df
tdf = melt(tdf, id.vars = c('sp', 'year'))
tdf = dcast(tdf, variable + year ~ sp)
colnames(tdf)[1:2] = c('Statistic', 'Years')
odf = rbind(odf, tdf[colnames(odf)])

## END OF CALCULATIONS  -------------------------------------------------------

write.csv(odf, 'csp_stat_30yr.csv', row.names = FALSE)
