library(data.table)
## The familiar and novel groups
grouping <- data.frame(Familiar = c("Bedroom", "Classroom", "Gym"), Novel = c("Library", "LivingRoom", "StorageRoom"))
rooms    <- c("Bedroom", "Classroom", "Gym", "Library", "LivingRoom", "StorageRoom")

GroupA <- paste0("P", c(13:20, 30, 33:34, 37:38, 41:42, 45:46, 49:50, 53:54, 57:58))
GroupB <- paste0("P", c(21:29, 31:32, 35:36, 39:40, 43:44, 47:48, 51:52, 55:56))

## Get the list of participant's folders
participant.folders <- dir(path = "IndividualRawData/", pattern = "^P")

## Get the order of the items outside the room
outside.orders <- read.table("OutsideOrders.csv", header = T)
outside.orders <- data.table(outside.orders)

outside.orders[Item == "MokeExpress"]$Item <- "MokaEspress"
outside.orders[Item == "Cabbaage"]$Item   <- "Cabbage"

## Get the rating in each folder (also need to determine whether the room is familiar or novel)
Curiosity.Recall <- NULL

for (thisFolder in participant.folders) {
  this.response <- read.csv(paste0("IndividualRawData", .Platform$file.sep, thisFolder, .Platform$file.sep, "Curiosity_Memory_Test_Response.csv"), header = T)
  this.response<- data.table(this.response)
  
  this.response$SubjectNo <- thisFolder
  this.response$Group <- "Familiar"
  
  # this.subjectNo <- as.numeric(strsplit(thisFolder, 'P')[[1]][2])
  this.subjectNo <- thisFolder
  
  # After P12 I switched the familiar and novel groups
  if (this.subjectNo %in% GroupA) {
    this.response[Context %in% grouping$Novel]$Group <- "Novel"
  } else {
    this.response[Context %in% grouping$Familiar]$Group <- "Novel"
  }
  
  this.response[Context == "None"]$Group <- "Distractor"
  
  this.response[Object == "MokeExpress"]$Object <- "MokaEspress"
  this.response[Object == "Cabbaage"]$Object <- "Cabbage"

  # Read the curiosity rating 
  this.rating <- read.csv(paste0("IndividualRawData", .Platform$file.sep, thisFolder, .Platform$file.sep, "CuriosityRatings.csv"), header = T, stringsAsFactors=F, fileEncoding= "UTF-8-BOM")
  this.rating <- data.table(this.rating)

  # Add the curiosity rating to the response file
  this.response$CurRating <- 0
  
  for (this.room in rooms) {
    this.response[Context == this.room]$CurRating <- this.rating[Room == this.room]$CuriosityRating
  }

  this.response[CurRating == 0]$CurRating <- NA

  # Add the order of room visits in each block
  even_indexes <- seq(2, nrow(this.rating), 2)
  this.rating$RoomOrder <- "First"
  this.rating[even_indexes]$RoomOrder <- "Second"

  this.response$RoomOrder <- ""

  for (this.room in rooms) {
    this.response[Context == this.room]$RoomOrder <- this.rating[Room == this.room]$RoomOrder
  }
  
  # Add the order of items outside each room
  this.response$ItemOrder <- 0
  
  item.list <- as.character(unique(outside.orders$Item))
  
  for (this.room in rooms) {
    for (this.item in item.list) {
      this.response[Context == this.room & Object == this.item]$ItemOrder <- outside.orders[SubjectNo == thisFolder & Room == this.room & Item == this.item]$Order
    }
  }
  
  this.response[ItemOrder == 0]$ItemOrder <- NA

  # Add the duration of staying outside the room
  this.response$Duration <- 0

  outside.trajectories <- list.files(path = paste0("IndividualRawData", .Platform$file.sep, thisFolder), pattern = "*_OutsideTrajectory*")
  
  if (length(outside.trajectories) > 0) {
    for (this.trj in outside.trajectories) {
      this.trj.info <- strsplit(this.trj, split = "_")
      this.room     <- this.trj.info[[1]][1]
      
      this.trj.data <- read.csv(paste("IndividualRawData", thisFolder, this.trj, sep = .Platform$file.sep), header = TRUE)
      
      rep.idx <- which(this.trj.data$TimeStamp == "TimeStamp")
      
      if (length(rep.idx) > 0) { this.trj.data <- this.trj.data[c(1:rep.idx[1] - 1), ]; droplevels(this.trj.data$TimeStamp)}
      
      if (nrow(this.trj.data) > 1) {this.response[Context == this.room]$Duration <- as.numeric(as.character(this.trj.data$TimeStamp[nrow(this.trj.data)])) - as.numeric(as.character(this.trj.data$TimeStamp[1])) }
    }
  }
  
  this.response[Duration == 0]$Duration <- NA
  
  Curiosity.Recall <- rbind(Curiosity.Recall, this.response)
}

Curiosity.Recall$CorrObjResp <- "Seen"
Curiosity.Recall[Group == "Distractor"]$CorrObjResp <- "New"

source("../Code/CalFunc.R")

Curiosity.Recall[, SFHit   := mapply(SFHitCal,      ObjectResponse, CorrObjResp)]
Curiosity.Recall[, SHit    := mapply(SHitCal,       ObjectResponse, CorrObjResp)]
Curiosity.Recall[, SFFalse := mapply(SFFalseHitCal, ObjectResponse, CorrObjResp)]
Curiosity.Recall[, SFalse  := mapply(SFalseHitCal,  ObjectResponse, CorrObjResp)]

Curiosity.Recall[, SrcHit  := mapply(SourceHitCal, ContextResponse, Context)]
Curiosity.Recall[, SrcFalse := mapply(SourceFalseHitCal, ContextResponse, Context)]

#  Calculate the false alarm rate for each individual participant
idv.false.alarm.rate <- Curiosity.Recall[Group == "Distractor", .(SFFalse = mean(SFFalse), SFalse = mean(SFalse), SrcFalse = mean(SrcFalse)/3), by = c("SubjectNo")]

#  Calculate the hit rates for each individual participant and each room
outside.hit.rate.per.room <- Curiosity.Recall[Context != "None", .(SFHit = mean(SFHit, na.rm = TRUE), SHit = mean(SHit, na.rm = TRUE), SrcHit = mean(SrcHit, na.rm = TRUE)), by = c("SubjectNo", "Context", "Group", "CurRating", "RoomOrder", "Duration")]
outside.hit.rate.per.room <- outside.hit.rate.per.room[order(SubjectNo, Context), ]
outside.hit.rate.per.room <- merge(outside.hit.rate.per.room, idv.false.alarm.rate, all = TRUE)

outside.hit.rate.per.room[, SFAcc  := (SFHit - SFFalse)]
outside.hit.rate.per.room[, SAcc   := (SHit  - SFalse)]
outside.hit.rate.per.room[, SrcAcc := (SrcHit - SrcFalse)] 

write.csv(outside.hit.rate.per.room, "GroupData/OutsideObjectsResponsePerRoom.csv", row.names = FALSE)
save(outside.hit.rate.per.room, file = "GroupData/OutsideObjectsResponsePerRoom.RData")

#  Calculate the hit rates for each individual participant and each novelty group
outside.hit.rate.novelty.group <- Curiosity.Recall[Context != "None", .(MeanCurRt = mean(CurRating, na.rm = TRUE), MeanDur = mean(Duration, na.rm = TRUE), SFHit = mean(SFHit, na.rm = TRUE), SHit = mean(SHit, na.rm = TRUE), SrcHit = mean(SrcHit, na.rm = TRUE)), by = c("SubjectNo", "Group")]
outside.hit.rate.novelty.group <- merge(outside.hit.rate.novelty.group, idv.false.alarm.rate)

outside.hit.rate.novelty.group[, SFAcc  := (SFHit - SFFalse)]
outside.hit.rate.novelty.group[, SAcc   := (SHit  - SFalse)]
outside.hit.rate.novelty.group[, SrcAcc := (SrcHit - SrcFalse)] 

save(outside.hit.rate.novelty.group, file = "GroupData/OutsideObjectResponseNoveltyGroups.RData")


# Calculate the hit rates for each individual participant and each curiosity group
## Calculate the mean and median curiosity rating for each participant 
average.curiosity.rating <- Curiosity.Recall[, .(MeanCur = mean(CurRating, na.rm = TRUE), MedianCur = median(CurRating, na.rm = TRUE)), by = c("SubjectNo")]
Curiosity.Recall         <- merge(Curiosity.Recall, average.curiosity.rating, all = TRUE)

## Separate the curiosity "high" and "low" groups according to the mean rating, median rating and 4 respectively.
Curiosity.Recall[, CurGrpMean      := mapply(CurGrpMeanSep,   CurRating, MeanCur) ]
Curiosity.Recall[, CurGrpMedian    := mapply(CurGrpMedianSep, CurRating, MedianCur) ]
Curiosity.Recall[, CurGrp          := mapply(CurGrpSep,       CurRating)]

outside.hit.rate.curiosity.group.mean    <- Curiosity.Recall[!is.na(CurGrpMean),   .(MeanDur = mean(Duration, na.rm = TRUE), SFHit = mean(SFHit, na.rm = TRUE), SHit = mean(SHit, na.rm = TRUE), SrcHit = mean(SrcHit, na.rm = TRUE)), by = c("SubjectNo", "CurGrpMean")]
outside.hit.rate.curiosity.group.median  <- Curiosity.Recall[!is.na(CurGrpMedian), .(MeanDur = mean(Duration, na.rm = TRUE), SFHit = mean(SFHit, na.rm = TRUE), SHit = mean(SHit, na.rm = TRUE), SrcHit = mean(SrcHit, na.rm = TRUE)), by = c("SubjectNo", "CurGrpMedian")]
outside.hit.rate.curiosity.group         <- Curiosity.Recall[!is.na(CurGrp),       .(MeanDur = mean(Duration, na.rm = TRUE), SFHit = mean(SFHit, na.rm = TRUE), SHit = mean(SHit, na.rm = TRUE), SrcHit = mean(SrcHit, na.rm = TRUE)), by = c("SubjectNo", "CurGrp")]

outside.hit.rate.curiosity.group.mean    <- merge(outside.hit.rate.curiosity.group.mean, idv.false.alarm.rate, all = TRUE)
outside.hit.rate.curiosity.group.median  <- merge(outside.hit.rate.curiosity.group.median, idv.false.alarm.rate, all = TRUE)
outside.hit.rate.curiosity.group         <- merge(outside.hit.rate.curiosity.group, idv.false.alarm.rate, all = TRUE)

outside.hit.rate.curiosity.group.mean[, c("SFAcc", "SAcc", "SrcAcc") := list( (SFHit - SFFalse), (SHit - SFalse), (SrcHit - SrcFalse))]
outside.hit.rate.curiosity.group.median[, c("SFAcc", "SAcc", "SrcAcc") := list( (SFHit - SFFalse), (SHit - SFalse), (SrcHit - SrcFalse))]
outside.hit.rate.curiosity.group[, c("SFAcc", "SAcc", "SrcAcc") := list( (SFHit - SFFalse), (SHit - SFalse), (SrcHit - SrcFalse))]

save(outside.hit.rate.curiosity.group.mean, outside.hit.rate.curiosity.group.median, outside.hit.rate.curiosity.group, file = "GroupData/OutsideObjectResponseCuriosityGroups.RData")


# Calculate the hit rates for each individual participant and each item order
## For each room 
Curiosity.Recall[, ObjOrdGrp := mapply(ObjOrdGrpSep, ItemOrder) ]

outside.hit.rate.item.order.group <- Curiosity.Recall[Context != "None", .(SFHit = mean(SFHit, na.rm = TRUE), SHit = mean(SHit, na.rm = TRUE), SrcHit = mean(SrcHit, na.rm = TRUE)), by = c("SubjectNo", "Context", "Group", "CurRating", "RoomOrder", "ObjOrdGrp")]
outside.hit.rate.item.order.group <- merge(outside.hit.rate.item.order.group, idv.false.alarm.rate, all = TRUE)
outside.hit.rate.item.order.group[, c("SFAcc", "SAcc", "SrcAcc") := list( (SFHit - SFFalse), (SHit - SFalse), (SrcHit - SrcFalse))]

## For each novelty group
outside.hit.rate.item.order.novelty <- Curiosity.Recall[Context != "None", .(MeanCurRt = mean(CurRating, na.rm = TRUE), MeanDur = mean(Duration, na.rm = TRUE), SFHit = mean(SFHit, na.rm = TRUE), SHit = mean(SHit, na.rm = TRUE), SrcHit = mean(SrcHit, na.rm = TRUE)), by = c("SubjectNo", "Group", "ObjOrdGrp")]
outside.hit.rate.item.order.novelty <- merge(outside.hit.rate.item.order.novelty, idv.false.alarm.rate, all = TRUE)
outside.hit.rate.item.order.novelty[, c("SFAcc", "SAcc", "SrcAcc") := list( (SFHit - SFFalse), (SHit - SFalse), (SrcHit - SrcFalse))]

## For each curiosity group
outside.hit.rate.item.order.curiosity.mean   <- Curiosity.Recall[Context != "None", .(SFHit = mean(SFHit, na.rm = TRUE), SHit = mean(SHit, na.rm = TRUE), SrcHit = mean(SrcHit, na.rm = TRUE)), by = c("SubjectNo", "CurGrpMean", "ObjOrdGrp")]
outside.hit.rate.item.order.curiosity.median <- Curiosity.Recall[Context != "None", .(SFHit = mean(SFHit, na.rm = TRUE), SHit = mean(SHit, na.rm = TRUE), SrcHit = mean(SrcHit, na.rm = TRUE)), by = c("SubjectNo", "CurGrpMedian", "ObjOrdGrp")]
outside.hit.rate.item.order.curiosity        <- Curiosity.Recall[Context != "None", .(SFHit = mean(SFHit, na.rm = TRUE), SHit = mean(SHit, na.rm = TRUE), SrcHit = mean(SrcHit, na.rm = TRUE)), by = c("SubjectNo", "CurGrp", "ObjOrdGrp")]

outside.hit.rate.item.order.curiosity.mean   <- merge(outside.hit.rate.item.order.curiosity.mean, idv.false.alarm.rate, all = TRUE)
outside.hit.rate.item.order.curiosity.median <- merge(outside.hit.rate.item.order.curiosity.median, idv.false.alarm.rate, all = TRUE)
outside.hit.rate.item.order.curiosity        <- merge(outside.hit.rate.item.order.curiosity, idv.false.alarm.rate, all = TRUE)

outside.hit.rate.item.order.curiosity.mean[, c("SFAcc", "SAcc", "SrcAcc") := list( (SFHit - SFFalse), (SHit - SFalse), (SrcHit - SrcFalse))]
outside.hit.rate.item.order.curiosity.median[, c("SFAcc", "SAcc", "SrcAcc") := list( (SFHit - SFFalse), (SHit - SFalse), (SrcHit - SrcFalse))]
outside.hit.rate.item.order.curiosity[, c("SFAcc", "SAcc", "SrcAcc") := list( (SFHit - SFFalse), (SHit - SFalse), (SrcHit - SrcFalse))]

save(outside.hit.rate.item.order.group, outside.hit.rate.item.order.novelty, outside.hit.rate.item.order.curiosity.mean, outside.hit.rate.item.order.curiosity.median, outside.hit.rate.item.order.curiosity, file = "GroupData/OutsideObjectResponseItemOrders.RData")

# Save the most detailed data sheet 

save(Curiosity.Recall, file = "GroupData/OutsideObjectResponse.RData")

# Calculate the frequence of reponses
Curiosity.Recall$ObjectType <- "Old"
Curiosity.Recall[Group == "Distractor"]$ObjectType <- "New"

with(Curiosity.Recall, table( ObjectResponse, SubjectNo, ObjectType))

RespFreqCal <- function(ObjResp, ... ){
  FreqTable <- as.data.frame(table(ObjResp)/sum(table(ObjResp)))
  names(FreqTable) <- c("Response", "Frequency")
  return(FreqTable)
}

Curiosity.Recall.Freq.Grp <- Curiosity.Recall[, c(RespFreqCal(ObjectResponse)), by = c("SubjectNo", "Group")]
Curiosity.Recall.Freq.Rsp <- Curiosity.Recall[, c(RespFreqCal(ObjectResponse)), by = c("SubjectNo", "CorrObjResp")]

save(Curiosity.Recall.Freq.Grp, Curiosity.Recall.Freq.Rsp, file = "GroupData/OutsideObjectResponseFrequencies.RData")
