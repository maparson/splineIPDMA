#--------------------------------#
# Main simulation script         #
#--------------------------------#

#--------------------------------#
# Clear data
rm(list=ls())
# Load packages
source("./Scripts/packages.R")

#--------------------------------#
# Load functions
source("./Scripts/functions2.R")
source("./Scripts/metaspline5.R")
source("./Scripts/mvspline3.R")
source("./Scripts/mvmeta2.R")
source("./Scripts/metacurve2.R")

#--------------------------------#
# Load simulated data
simDatTemp <- read_csv("Data/simulation_2023-05-16.csv")
simDat <- simDatTemp
simFit.metafp <- read_csv("Data/simFit.metafp_2023-05-16.csv")
simFit.mvfp <- read_csv("Data/simFit.mvfp_2023-05-16.csv")
simFit.meta <- read_csv("Data/simFit.meta_2023-05-16.csv")
simFit.mv <- read_csv("Data/simFit.mv_2023-05-16.csv")





