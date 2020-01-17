# Pipeline of analysing the data

# 1. Preprocessing the data

is.subset <- FALSE

## For all participants
if (!is.subset) { participant.list <- list.files(path = "./PilotData/IndividualData/", pattern = "^P")}
## For a sub-group of participants
else { participant.list <- c(paste0("P0", c(1:9)), paste0("P", c(11:20)) }

source("./PilotAnalysis/Preprocessing.R")


# 2. Plot individual data

load("./PilotData/IndividualData.RData")

source("./PilotAnalysis/PlotIndividualData.R")

