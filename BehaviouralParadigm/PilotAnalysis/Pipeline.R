# Pipeline of analysing the data

# 1. Preprocessing the data

is.subset <- FALSE

## For all participants
if (!is.subset) { participant.list <- list.files(path = "./PilotData/IndividualData/", pattern = "^P")}
## For a sub-group of participants
else { participant.list <- c(paste0("P0", c(1:9)), paste0("P", c(11:20)) }

participant.list <- c(paste0("P", c(32)))

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
require(data.table)

corr.hit.rate <- data.table(participant.list, Recall.Freq.Rsp[Group == "OldItem" & Response == "Seen"]$Frequency - Recall.Freq.Rsp[Group == "Distractor" & Response == "Seen"]$Frequency)
names(corr.hit.rate) <- c("SubjectNo", "SAcc")

### Check the total number of participants that have been tested
nrow(corr.hit.rate)  # N = 29

criteria <- 0.05

excluded.ps <- corr.hit.rate[SAcc < criteria]$SubjectNo

### Check the number of participants that would be excluded from further analysis
length(excluded.ps)
# when criteria = 5%, N = 3
# when criteria = 10%, N = 7

save(excluded.ps, file = "./PilotData/ExcludedSubjectList.RData")

source("./PilotAnalysis/PlotGroupData.R")
