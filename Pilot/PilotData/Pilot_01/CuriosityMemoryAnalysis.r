library(data.table); library(ggplot2); library(Hmisc)

## The familiar and novel groups
grouping <- data.frame(Familiar = c("Bedroom", "Classroom", "Gym"), Novel = c("Library", "LivingRoom", "StorageRoom"))
rooms    <- c("Bedroom", "Classroom", "Gym", "Library", "LivingRoom", "StorageRoom")

## Get the list of participant's folders
participant.folders <- dir(path = "IndividualRawData/", pattern = "^P")

## Get the order of the items outside the room
outside.orders <- read.table("OutsideOrders.csv", header = T)
outside.orders <- data.table(outside.orders)

## Get the rating in each folder (also need to determine whether the room is familiar or novel)
Curiosity.Recall <- NULL

for (thisFolder in participant.folders) {
  this.response <- read.csv(paste0("IndividualRawData", .Platform$file.sep, thisFolder, .Platform$file.sep, "Curiosity_Memory_Test_Response.csv"), header = T)
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






