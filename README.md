# SolBeePop Repository

Last Edited: 25 October 2023

## Disclaimer
The software and associated files uploaded in this repository were used to generate the results published in:

Schmolke A, Galic N, Hinarejos S (2023). SolBeePop: a model of solitary bee populations in agricultural landscapes. _Accepted for publication in the Journal of Applied Ecology._

_This software and associated files are provided "as is" with the sole purpose to allow the reproduction of the published results without any warranties of performance or fitness for any other purpose._

## SolBeePop Code and Documentation

SolBeePop.nlogo: Code of the SolBeePop model. 
_To run SolBeePop, the NetLogo software is required_(for runs in this study, NetLogo 6.2.0 was used). The link to download NetLogo 6.2.0 is here: [https://ccl.northwestern.edu/netlogo/6.2.0/](https://ccl.northwestern.edu/netlogo/6.2.0/)

SolBeePop\_TRACE.pdf: Documentation of the SolBeePop model following TRACE. The documentation includes the conceptual model development following Pop-GUIDE, the model description according to the ODD protocol and the detailed description of the model analysis and corroboration.

SolBeePop\_Tables.xlsx: Tables with descriptions of the procedures in SolBeePop, model parameters, input files and variables. Compilations of parameters for species Eucera pruinosa, Megachile rotundata, Nomia melanderi and Osmia bicornis are also included.

SolBeePop\_Manual.pdf: Short overview of the user interface of the SolBeePop model and how simulations can be started from the interface. 

Floral\_generic\_optimal.csv: Example input file for the model. An input file is required to run the model. 

Foraging\_Landscape\_InputSce\_SolBeePop2022.R: R-script for the generation of SolBeePop input files (requires daily weather data as input).

## SolBeePop Simulations (Schmolke, Galic & Hinarejos, 2023)

__Model analysis:__

SolBeePop input files:

- Model\_Analysis\Floral\_generic\_optimal.csv
- Model\_Analysis\Floral\_generic\_Sce2.csv
- Model\_Analysis\Floral\_generic\_Sce3\_Osmia.csv
- Model\_Analysis\Floral\_generic\_Sce3\_Nomia.csv

Setup of simulations (definition of model parameters and ranges):

- Model\_Analysis\input\_\<Osmia or Nomia\>\_\<scenario number\>.csv

PRCC analysis of simulations:

- Model\_Analysis\prcc\_run\_\<Osmia or Nomia\>\_\<scenario number\>.csv

R-scripts:

- Model\_Analysis\run\_lhc.R – setup of Latin Hypercube for model analysis simulations
- Model\_Analysis\run\_netlogo.R – generates .xml (NetLogo BehaviorSpace) and .bat files to run simulations from batch (using CommandPrompt)
- Model\_Analysis\ua\_prcc\_analysis.R – read in simulation output files from one species and scenario, and calculates PRCCs; writes PRCC table to file; generates PRCC plots across model outputs
- Model\_Analysis\ua\_prcc\_plot\_Fig.R – generates PRCC plot (manuscript figure)

__Calibration and validation:__

SolBeePop input files:

- Calibration\_Validation\Floral\_S15\_01802\_Sce3\_Jan2022.csv
- Calibration\_Validation\Floral\_S15\_01803\_Sce3\_Jan2022.csv
- Calibration\_Validation\Floral\_S15\_01804\_Sce3\_Jan2022.csv

Data from Ruddle et al. (2018) semi-field studies with Osmia bicornis (including weather data) are found the folders:

- Calibration\_Validation\S15-01802-01\_Tables
- Calibration\_Validation\S15-01803-01\_Tables
- Calibration\_Validation\S15-01804-01\_Tables

Calibration\_Validation\Calibration\: contains necessary input files to set up calibration simulations (by R-scripts listed below), goodness-of-fit measures (GOFM) calculated from outputs and simulation outputs prepared for plotting

Calibration\_Validation\Validation\: contains setup of validation simulations (by R-scripts listed below), goodness-of-fit measures (GOFM) calculated from outputs and simulation outputs prepared for plotting

R-scripts:

- Calibration\_Validation\run\_lhc\_calib.R – setup of Latin Hypercube for calibration simulations
- Calibration\_Validation\run\_netlogo\_calib\_valid.R – generates .xml (NetLogo BehaviorSpace) and .bat files to run simulations from batch (using CommandPrompt)
- Calibration\_Validation\Calibration\_Validation\_analysis.R – read in simulation output files and calculate goodness-of-fit measures (GOFM)
- Calibration\_Validation\Plot\_Calibration\_Validation\_Feb2023.R – plot simulation outputs from single parameter combinations along with study data (manuscript figure)

__Simulations across species:__

SolBeePop input file:

- Cross\_Species\_Simulations\Floral\_generic\_optimal.csv

Setup of simulations (definition of model parameters and ranges):

- Cross\_Species\_Simulations\run\_cross\_species.csv

Summary statistics of simulation outputs:

- Cross\_Species\_Simulations\csp\_stat\_30yr.csv

R-scripts:

- Cross\_Species\_Simulations\run\_netlogo\_cross\_species.R – generates .xml (NetLogo BehaviorSpace) and .bat files to run simulations from batch (using CommandPrompt)
- Cross\_Species\_Simulations\cross\_species\_stats.R – reads the simulation outputs and calculates population properties (carrying capacity, time to reach it, final population size, etc.)
- Cross\_Species\_Simulations\cross\_species\_plots.R – read the simulation outputs and generates plots, including the manuscript figures

