library(data.table); library(ggplot2); library(Hmisc)

## The familiar and novel groups
grouping <- data.frame(Familiar = c("Bedroom", "Classroom", "Gym"), Novel = c("Library", "LivingRoom", "StorageRoom"))
rooms    <- c("Bedroom", "Classroom", "Gym", "Library", "LivingRoom", "StorageRoom")

## Get the list of participant's folders
participant.folders <- dir(path = "IndividualRawData/", pattern = "^P")

## Get the order of the items inside the room
inside.orders <- read.table("InsideOrders.csv", header = T)
inside.orders <- data.table(inside.orders)

## Get the rating in each folder (also need to determine whether the room is familiar or novel)
Encoding.Recall <- NULL

for (thisFolder in participant.folders) {
  this.response <- read.csv(paste0("IndividualRawData", .Platform$file.sep, thisFolder, .Platform$file.sep, "Encoding_Memory_Test_Response.csv"), header = T)
  this.response<- data.table(this.response)
  
  this.response$SubjectNo <- thisFolder
  this.response$Group <- "Familiar"
  
  this.subjectNo <- as.numeric(strsplit(thisFolder, 'P')[[1]][2])
  
  # After P12 I switched the familiar and novel groups
  if (this.subjectNo < 13) {
    this.response[Context %in% grouping$Novel]$Group <- "Novel"
  } else {
    this.response[Context %in% grouping$Familiar]$Group <- "Novel"
  }
  
  this.response[Context == "None"]$Group <- "Distractor"
  
  this.response[Object == "MokeExpress"]$Object <- "MokaEspress"
  this.response[Object == "Cabbaage"]$Object <- "Cabbage"

  # Read the curiosity rating 
  this.rating <- read.csv(paste0("IndividualRawData", .Platform$file.sep, thisFolder, .Platform$file.sep, "EncodingRatings.csv"), header = T, stringsAsFactors=F, fileEncoding= "UTF-8-BOM")
  this.rating <- data.table(this.rating)

  # Add the curiosity rating to the response file
  this.response$CurRating <- 0
  
  for (this.room in rooms) {
    this.response[Context == this.room]$CurRating <- this.rating[Room == this.room]$EncodingRating
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
  
  # Add the order of items inside each room
  this.response$ItemOrder <- 0
  
  item.list <- as.character(unique(inside.orders$Item))
  
  for (this.room in rooms) {
    for (this.item in item.list) {
      this.response[Context == this.room & Object == this.item]$ItemOrder <- inside.orders[SubjectNo == thisFolder & Room == this.room & Item == this.item]$Order
    }
  }
  
  this.response[ItemOrder == 0]$ItemOrder <- NA

  # Add the duration of staying inside the room
  this.response$Duration <- 0

  inside.trajectories <- list.files(path = paste0("IndividualRawData", .Platform$file.sep, thisFolder), pattern = "*_InsideTrajectory*")
  
  if (length(inside.trajectories) > 0) {
    for (this.trj in inside.trajectories) {
      this.trj.info <- strsplit(this.trj, split = "_")
      this.room     <- this.trj.info[[1]][1]
      
      this.trj.data <- read.csv(paste("IndividualRawData", thisFolder, this.trj, sep = .Platform$file.sep), header = TRUE)
      
      rep.idx <- which(this.trj.data$TimeStamp == "TimeStamp")
      
      if (length(rep.idx) > 0) { this.trj.data <- this.trj.data[c(1:rep.idx[1] - 1), ]; droplevels(this.trj.data$TimeStamp)}
      
      if (nrow(this.trj.data) > 1) {this.response[Context == this.room]$Duration <- as.numeric(as.character(this.trj.data$TimeStamp[nrow(this.trj.data)])) - as.numeric(as.character(this.trj.data$TimeStamp[1])) }
    }
  }
  
  this.response[Duration == 0]$Duration <- NA
  
  Encoding.Recall <- rbind(Encoding.Recall, this.response)
}

Encoding.Recall$CorrObjResp <- "Seen"
Encoding.Recall[Group == "Distractor"]$CorrObjResp <- "New"

source("../Code/CalFunc.R")

Encoding.Recall[, SFHit   := mapply(SFHitCal,      ObjectResponse, CorrObjResp)]
Encoding.Recall[, SHit    := mapply(SHitCal,       ObjectResponse, CorrObjResp)]
Encoding.Recall[, SFFalse := mapply(SFFalseHitCal, ObjectResponse, CorrObjResp)]
Encoding.Recall[, SFalse  := mapply(SFalseHitCal,  ObjectResponse, CorrObjResp)]

Encoding.Recall[, SrcHit  := mapply(SourceHitCal, ContextResponse, Context)]
Encoding.Recall[, SrcFalse := mapply(SourceFalseHitCal, ContextResponse, Context)]

#  Calculate the false alarm rate for each individual participant
idv.false.alarm.rate <- Encoding.Recall[Group == "Distractor", .(SFFalse = mean(SFFalse), SFalse = mean(SFalse), SrcFalse = mean(SrcFalse)/3), by = c("SubjectNo")]

#  Calculate the hit rates for each individual participant and each room
inside.hit.rate.per.room <- Encoding.Recall[Context != "None", .(SFHit = mean(SFHit, na.rm = TRUE), SHit = mean(SHit, na.rm = TRUE), SrcHit = mean(SrcHit, na.rm = TRUE)), by = c("SubjectNo", "Context", "Group", "CurRating", "RoomOrder", "Duration")]
inside.hit.rate.per.room <- inside.hit.rate.per.room[order(SubjectNo, Context), ]
inside.hit.rate.per.room <- merge(inside.hit.rate.per.room, idv.false.alarm.rate, all = TRUE)

inside.hit.rate.per.room[, SFAcc  := (SFHit - SFFalse)]
inside.hit.rate.per.room[, SAcc   := (SHit  - SFalse)]
inside.hit.rate.per.room[, SrcAcc := (SrcHit - SrcFalse)] 

write.csv(inside.hit.rate.per.room, "GroupData/InsideObjectsResponsePerRoom.csv", row.names = FALSE)
save(inside.hit.rate.per.room, file = "GroupData/InsideObjectsResponsePerRoom.RData")

#  Calculate the hit rates for each individual participant and each novelty group
inside.hit.rate.novelty.group <- Encoding.Recall[Context != "None", .(MeanCurRt = mean(CurRating, na.rm = TRUE), MeanDur = mean(Duration, na.rm = TRUE), SFHit = mean(SFHit, na.rm = TRUE), SHit = mean(SHit, na.rm = TRUE), SrcHit = mean(SrcHit, na.rm = TRUE)), by = c("SubjectNo", "Group")]
inside.hit.rate.novelty.group <- merge(inside.hit.rate.novelty.group, idv.false.alarm.rate)

inside.hit.rate.novelty.group[, SFAcc  := (SFHit - SFFalse)]
inside.hit.rate.novelty.group[, SAcc   := (SHit  - SFalse)]
inside.hit.rate.novelty.group[, SrcAcc := (SrcHit - SrcFalse)] 

save(inside.hit.rate.novelty.group, file = "GroupData/InsideObjectResponseNoveltyGroups.RData")


# Calculate the hit rates for each individual participant and each curiosity group
## Calculate the mean and median curiosity rating for each participant 
average.curiosity.rating <- Encoding.Recall[, .(MeanCur = mean(CurRating, na.rm = TRUE), MedianCur = median(CurRating, na.rm = TRUE)), by = c("SubjectNo")]
Encoding.Recall         <- merge(Encoding.Recall, average.curiosity.rating, all = TRUE)

## Separate the curiosity "high" and "low" groups according to the mean rating, median rating and 4 respectively.
Encoding.Recall[, CurGrpMean      := mapply(CurGrpMeanSep,   CurRating, MeanCur) ]
Encoding.Recall[, CurGrpMedian    := mapply(CurGrpMedianSep, CurRating, MedianCur) ]
Encoding.Recall[, CurGrp          := mapply(CurGrpSep,       CurRating)]

inside.hit.rate.curiosity.group.mean    <- Encoding.Recall[!is.na(CurGrpMean),   .(MeanDur = mean(Duration, na.rm = TRUE), SFHit = mean(SFHit, na.rm = TRUE), SHit = mean(SHit, na.rm = TRUE), SrcHit = mean(SrcHit, na.rm = TRUE)), by = c("SubjectNo", "CurGrpMean")]
inside.hit.rate.curiosity.group.median  <- Encoding.Recall[!is.na(CurGrpMedian), .(MeanDur = mean(Duration, na.rm = TRUE), SFHit = mean(SFHit, na.rm = TRUE), SHit = mean(SHit, na.rm = TRUE), SrcHit = mean(SrcHit, na.rm = TRUE)), by = c("SubjectNo", "CurGrpMedian")]
inside.hit.rate.curiosity.group         <- Encoding.Recall[!is.na(CurGrp),       .(MeanDur = mean(Duration, na.rm = TRUE), SFHit = mean(SFHit, na.rm = TRUE), SHit = mean(SHit, na.rm = TRUE), SrcHit = mean(SrcHit, na.rm = TRUE)), by = c("SubjectNo", "CurGrp")]

inside.hit.rate.curiosity.group.mean    <- merge(inside.hit.rate.curiosity.group.mean, idv.false.alarm.rate, all = TRUE)
inside.hit.rate.curiosity.group.median  <- merge(inside.hit.rate.curiosity.group.median, idv.false.alarm.rate, all = TRUE)
inside.hit.rate.curiosity.group         <- merge(inside.hit.rate.curiosity.group, idv.false.alarm.rate, all = TRUE)

inside.hit.rate.curiosity.group.mean[, c("SFAcc", "SAcc", "SrcAcc") := list( (SFHit - SFFalse), (SHit - SFalse), (SrcHit - SrcFalse))]
inside.hit.rate.curiosity.group.median[, c("SFAcc", "SAcc", "SrcAcc") := list( (SFHit - SFFalse), (SHit - SFalse), (SrcHit - SrcFalse))]
inside.hit.rate.curiosity.group[, c("SFAcc", "SAcc", "SrcAcc") := list( (SFHit - SFFalse), (SHit - SFalse), (SrcHit - SrcFalse))]

save(inside.hit.rate.curiosity.group.mean, inside.hit.rate.curiosity.group.median, inside.hit.rate.curiosity.group, file = "GroupData/InsideObjectResponseEncodingGroups.RData")


# Calculate the hit rates for each individual participant and each item order
## For each room 
Encoding.Recall[, ObjOrdGrp := mapply(ObjOrdGrpSep, ItemOrder) ]

inside.hit.rate.item.order.group <- Encoding.Recall[Context != "None", .(SFHit = mean(SFHit, na.rm = TRUE), SHit = mean(SHit, na.rm = TRUE), SrcHit = mean(SrcHit, na.rm = TRUE)), by = c("SubjectNo", "Context", "Group", "CurRating", "RoomOrder", "ObjOrdGrp")]
inside.hit.rate.item.order.group <- merge(inside.hit.rate.item.order.group, idv.false.alarm.rate, all = TRUE)
inside.hit.rate.item.order.group[, c("SFAcc", "SAcc", "SrcAcc") := list( (SFHit - SFFalse), (SHit - SFalse), (SrcHit - SrcFalse))]

## For each novelty group
inside.hit.rate.item.order.novelty <- Encoding.Recall[Context != "None", .(MeanCurRt = mean(CurRating, na.rm = TRUE), MeanDur = mean(Duration, na.rm = TRUE), SFHit = mean(SFHit, na.rm = TRUE), SHit = mean(SHit, na.rm = TRUE), SrcHit = mean(SrcHit, na.rm = TRUE)), by = c("SubjectNo", "Group", "ObjOrdGrp")]
inside.hit.rate.item.order.novelty <- merge(inside.hit.rate.item.order.novelty, idv.false.alarm.rate, all = TRUE)
inside.hit.rate.item.order.novelty[, c("SFAcc", "SAcc", "SrcAcc") := list( (SFHit - SFFalse), (SHit - SFalse), (SrcHit - SrcFalse))]

## For each curiosity group
inside.hit.rate.item.order.curiosity.mean   <- Encoding.Recall[Context != "None", .(SFHit = mean(SFHit, na.rm = TRUE), SHit = mean(SHit, na.rm = TRUE), SrcHit = mean(SrcHit, na.rm = TRUE)), by = c("SubjectNo", "CurGrpMean", "ObjOrdGrp")]
inside.hit.rate.item.order.curiosity.median <- Encoding.Recall[Context != "None", .(SFHit = mean(SFHit, na.rm = TRUE), SHit = mean(SHit, na.rm = TRUE), SrcHit = mean(SrcHit, na.rm = TRUE)), by = c("SubjectNo", "CurGrpMedian", "ObjOrdGrp")]
inside.hit.rate.item.order.curiosity        <- Encoding.Recall[Context != "None", .(SFHit = mean(SFHit, na.rm = TRUE), SHit = mean(SHit, na.rm = TRUE), SrcHit = mean(SrcHit, na.rm = TRUE)), by = c("SubjectNo", "CurGrp", "ObjOrdGrp")]

inside.hit.rate.item.order.curiosity.mean   <- merge(inside.hit.rate.item.order.curiosity.mean, idv.false.alarm.rate, all = TRUE)
inside.hit.rate.item.order.curiosity.median <- merge(inside.hit.rate.item.order.curiosity.median, idv.false.alarm.rate, all = TRUE)
inside.hit.rate.item.order.curiosity        <- merge(inside.hit.rate.item.order.curiosity, idv.false.alarm.rate, all = TRUE)

inside.hit.rate.item.order.curiosity.mean[, c("SFAcc", "SAcc", "SrcAcc") := list( (SFHit - SFFalse), (SHit - SFalse), (SrcHit - SrcFalse))]
inside.hit.rate.item.order.curiosity.median[, c("SFAcc", "SAcc", "SrcAcc") := list( (SFHit - SFFalse), (SHit - SFalse), (SrcHit - SrcFalse))]
inside.hit.rate.item.order.curiosity[, c("SFAcc", "SAcc", "SrcAcc") := list( (SFHit - SFFalse), (SHit - SFalse), (SrcHit - SrcFalse))]

save(inside.hit.rate.item.order.group, inside.hit.rate.item.order.novelty, inside.hit.rate.item.order.curiosity.mean, inside.hit.rate.item.order.curiosity.median, inside.hit.rate.item.order.curiosity, file = "GroupData/InsideObjectResponseItemOrders.RData")

# Save the most detailed data sheet 

save(Encoding.Recall, file = "GroupData/InsideObjectResponse.RData")

# Calculate the frequence of reponses
Encoding.Recall$ObjectType <- "Old"
Encoding.Recall[Group == "Distractor"]$ObjectType <- "New"

with(Encoding.Recall, table( ObjectResponse, SubjectNo, ObjectType))

RespFreqCal <- function(ObjResp, ... ){
  FreqTable <- as.data.frame(table(ObjResp)/sum(table(ObjResp)))
  names(FreqTable) <- c("Response", "Frequency")
  return(FreqTable)
}

Encoding.Recall.Freq.Grp <- Encoding.Recall[, c(RespFreqCal(ObjectResponse)), by = c("SubjectNo", "Group")]
Encoding.Recall.Freq.Rsp <- Encoding.Recall[, c(RespFreqCal(ObjectResponse)), by = c("SubjectNo", "CorrObjResp")]

save(Encoding.Recall.Freq.Grp, Encoding.Recall.Freq.Rsp, file = "GroupData/InsideObjectResponseFrequencies.RData")
