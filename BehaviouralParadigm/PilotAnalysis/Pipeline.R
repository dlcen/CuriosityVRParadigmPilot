# Pipeline of analysing the data

# 1. Preprocessing the data

is.subset <- FALSE

## For all participants
if (!is.subset) { participant.list <- list.files(path = "./PilotData/IndividualData/", pattern = "^P")}
## For a sub-group of participants
else { participant.list <- c(paste0("P0", c(1:9)), paste0("P", c(11:20)) }

participant.list <- c(paste0("P", c(16:20)))

is.day1.only <- FALSE

source("./PilotAnalysis/Preprocessing.R")


# 2. Plot individual data

load("./PilotData/IndividualData.RData")

## Check the number of rooms in each split curiosity groups

is.day1.only <- TRUE

source("./PilotAnalysis/GroupRoomNo.R")

# for (this.p in participant.list) { GroupRoomNo(this.p) }

for (this.p in participant.list) { GroupRoomNo2(this.p) }

source("./PilotAnalysis/PlotIndividualData.R")

# 3. Check the group data

## Check how the curiosity ratings are distributed for each room.
source("./PilotAnalysis/RoomRatings.R")

## Check how the effects look for the group as a whole
source("./PilotAnalysis/PlotGroupData.R")

