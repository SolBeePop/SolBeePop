### SolBeePop_ecotox ####################################################
### Script for SolBeePop_ecotox setup and runs (NetLogo BehaviorSpace runs from batch) 
### Script used for model analysis, calibration, validation and case runs
### Requires a csv table input ("run_*.csv") that lists all parameters (one row per parameter combination)
### Author: Amelie Schmolke 
### Date: March 2023
###################################################################

library(XML) # load library

rm(list = ls()) # clean up workspace

## Inputs ---------------------------------------------------------------------
## change those for a new set of simulations
ofp = 'C:\\SolBeePop_ecotox\\Model_Analysis\\Sensitivity_analysis_effects\\Osmia\\sims_Osmia_SA_GUTS_SD_Mar2023\\'  # output file path
mfp = 'C:\\SolBeePop_ecotox\\SolBeePop_ecotox.nlogo'
csv_in = 'run_Osmia_GUTS_SD_Mar2023.csv' # table all parameter values for each experiment: set relevant file name 
xml_out = 'run_Osmia_GUTS_SD_Mar2023.xml'
bat_out = 'run_Osmia_GUTS_SD_Mar2023.bat'

## Set Up ---------------------------------------------------------------------
setwd(ofp)
vdf = read.csv(csv_in, stringsAsFactors = FALSE, check.names = FALSE)
vls = c(  # numeric inputs to NetLogo
  'Start.day', 'Initial.num.f', 'Initial.num.m', 'Initial.age', 'RndSeed',
  'Num.repeat.yr', 'DD.thresh.s', 'DD.max.cells.s', 'DD.log.slope',
  'day.emerge.f', 'var.emerge.f', 'day.emerge.m',
  'var.emerge.m', 'latest.emerge', 'dev.egg', 'dev.larva', 'dev.cocoon',
  't.maturation', 'm.life', 'max.nesting.life', 'p.max.nesting.life',
  'max.f.ratio', 'max.cells', 'max.survival.e.f', 'max.survival.e.m',
  'emerged.survival', 'a.cell.age', 'a.sex.age', 'a.size.age',
  'a.cell.resource', 'a.sex.resource', 'a.size.resource',
  'ad.nectar.cons', 'ad.pollen.cons', 'k_CA', 'ad.ET', 'TC_soil', 'TC_leaf',
  't.guts', 'kd_SD', 'bw_SD', 'mw_SD', 'kd_IT', 'mw_IT', 'Fs_IT',
  'nectar_prop', 'weight_prov', 'SM', 'F', 'SA_i', 'dr.intercept', 'dr.slope')
# model outputs to be collected
ols = c('doy', 'year', 'DateREP', 'count turtles', 'bees.emerged.yr',
  'f.emerged.yr', 'm.emerged.yr', 'bees.nesting', 'bees.nesting.today',
  'sum.cells.today', 'sum.f.cells.today', 'sum.m.cells.today', 'sum.cells',
  'sum.f.cells', 'sum.m.cells', 'mean.cells.today', 'mean.f.cells.today',
  'mean.m.cells.today', 'mean.cells', 'mean.f.cells', 'mean.m.cells',
  'deaths.exp.in', 'deaths.exp.ad.today',	'deaths.exp.ad',	
  'exposure.in',	'C.effective.today')

## Create xml and bat files ---------------------------------------------------
bls = c()

xls = xmlHashTree() 
nde = addNode(xmlNode('experiments'), character(), xls)

# Loop through scenarios
for(i in 1:nrow(vdf)){
  ## assign variable values
  for(j in colnames(vdf)){
    x = vdf[i,j]
    if(grepl(',', x)) x = trimws(strsplit(x, ',')[[1]])  # split list
    if(j %in% vls) x = as.numeric(x)  # format number
    assign(j, x)
    rm(x)
  }
  rm(j)
  
  ## Add to bat file
  bls = c(bls, 
    paste('java -Xmx8000m -Dfile.encoding=UTF-8 -cp netlogo-6.2.0.jar', 
      'org.nlogo.headless.Main --model', mfp,
      '--setup-file', paste0(ofp, paste0('\\',xml_out)),
      '--experiment', i,
      '--table', paste0(ofp, '\\', name, '.csv')))

  ## Add to xml file
  snde = addNode(xmlNode('experiment',  # scenario name
    attrs = c(name = i, repetitions = '1', 
      runMetricsEveryStep = 'true')), nde, xls)
  tnde = addNode(xmlNode('setup'), snde, xls)  # set up
  addNode(xmlTextNode('setup'), tnde, xls)
  tnde = addNode(xmlNode('go'), snde, xls)  # go
  addNode(xmlTextNode('go'), tnde, xls)
  # addNode(xmlNode('timeLimit', attrs = c(steps = 365*2)), snde, xls)  # steps
  for(j in ols){
    tnde = addNode(xmlNode('metric'), snde, xls)
    addNode(xmlTextNode(j), tnde, xls)
  }
  rm(j)

  for(j in colnames(vdf)){  # input values
    if(j == 'name') next
    tnde = addNode(xmlNode('enumeratedValueSet', 
      attrs = c(variable = j)), snde, xls)
    for(k in get(j)) {
      if(class(k) == 'character' | is.na(k)){
        k = paste0('\"', k, '\"')
      } else if(class(k) == 'logical'){
        k =  tolower(k)
      }
      addNode(xmlNode('value', attrs = c(value = k)), tnde, xls)
    }
    rm(k)
  }
  rm(j)
}

# write xml file
write('<?xml version="1.0" encoding="UTF-8"?>', file = xml_out)
write('<!DOCTYPE experiments SYSTEM "behaviorspace.dtd">',
  file = xml_out, append = TRUE)
capture.output(xls, file = xml_out, append = TRUE)

writeLines(bls, bat_out)  # write bat file

rm(list = ls()) # clean up workspace
