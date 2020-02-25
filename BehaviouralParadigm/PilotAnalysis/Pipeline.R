# Pipeline of analysing the data

# 1. Preprocessing the data

is.subset <- FALSE

## For all participants
if (!is.subset) { participant.list <- list.files(path = "./PilotData/IndividualData/", pattern = "^P")}
## For a sub-group of participants
else { participant.list <- c(paste0("P0", c(1:9)), paste0("P", c(11:20)) }

participant.list <- c(paste0("P", c(29:31)))

is.day1.only <- FALSE

source("./PilotAnalysis/Preprocessing.R")


# 2. Plot individual data

load("./PilotData/IndividualData.RData")

## Check the number of rooms in each split curiosity groups

is.day1.only <- FALSE

source("./PilotAnalysis/GroupRoomNo.R")

# for (this.p in participant.list) { GroupRoomNo(this.p) }

for (this.p in participant.list) { GroupRoomNo2(this.p) }

source("./PilotAnalysis/PlotIndividualData.R")

# 3. Check the group data

## Check how the curiosity ratings are distributed for each room.
source("./PilotAnalysis/RoomRatings.R")

## Check how the effects look for the group as a whole

## Check how many participants have a corrected recollection rate < 10%
corr.hit.rate <- data.table(participant.list, Recall.Freq.Rsp[Group == "OldItem" & Response == "Seen"]$Frequency - Recall.Freq.Rsp[Group == "Distractor" & Response == "Seen"]$Frequency)
names(corr.hit.rate) <- c("SubjectNo", "SAcc")

criteria <- 0.05

excluded.ps <- corr.hit.rate[SAcc < criteria]$SubjectNo

source("./PilotAnalysis/PlotGroupData.R")
