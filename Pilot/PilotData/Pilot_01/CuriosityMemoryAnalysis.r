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

  Curiosity.Recall <- rbind(Curiosity.Recall, this.response)
}

Curiosity.Recall$CorrObjResp <- "Seen"
Curiosity.Recall[Group == "Distractor"]$CorrObjResp <- "New"

source("../Code/CalFunc.R")

Curiosity.Recall[, Accuracy :=  mapply(AccuracyCal,ObjectResponse, CorrObjResp)]
Curiosity.Recall[, SeenHit  :=  mapply(SeenAccuracyCal, ObjectResponse, CorrObjResp)]
Curiosity.Recall[, FalseAlarm := mapply(FalseAlarmCal, ObjectResponse, CorrObjResp)]
Curiosity.Recall[, FalseSeenHit := mapply(FalseSeenHitCal, ObjectResponse, CorrObjResp)]

Curiosity.Recall[, ContextAccuracy := mapply(ContextAccuracyCal, ContextResponse, Context)]
Curiosity.Recall[, ContextFalseHit := mapply(ContextFalseHitCal, ContextResponse, Context)]

#  Calculate the false alarm rate for each individual participant
idv.false.alarm.rate <- Curiosity.Recall[Group == "Distractor", .(ItemFalseAlarm = mean(FalseAlarm), ItemFalseSeenHit = mean(FalseSeenHit), ContextFalseHit = mean(ContextFalseHit)/3), by = c("SubjectNo")]

 
## Calculate hit rate with only "seen" response
OutsideObjectsMemory <- NULL

for (thisP in participant.folders) {
  for (thisR in Rooms) {
    thisD <- Curiosity.Recall[SubjectNo == thisP & Context == thisR]
    
    if (nrow(thisD) > 0) {
      
      false.alarm.rate <- mean(new.item.responses[SubjectNo == thisP]$far, na.rm = FALSE)
      hit.rate <- mean(thisD$Accuracy, na.rm = FALSE)
      seen.hit <- mean(thisD$SeenHit )
      false.seen.hit <- mean(Curiosity.Recall[SubjectNo == thisP]$FalseSeenHit)
      corr.seen.hit <- seen.hit - false.seen.hit
      item.accuracy <- hit.rate - false.alarm.rate
      
      
      if (item.accuracy < 0) {
        item.accuracy = 0
      }
      
      context.hit.rate <- mean(thisD$ContextAccuracy, na.rm = FALSE)

      curiosity.rating <- Curiosity.Ratings[SubjectNo == thisP & Room == thisR]$CuriosityRating
      n.curiosity.rating <- Curiosity.Ratings[SubjectNo == thisP & Room == thisR]$NRating
      curiosity.group.mean <- Curiosity.Ratings[SubjectNo == thisP & Room == thisR]$RatingGroupMean
      curiosity.group.median <- Curiosity.Ratings[SubjectNo == thisP & Room == thisR]$RatingGroupMedian
      
      room.order <- Curiosity.Ratings[SubjectNo == thisP & Room == thisR]$RoomOrder
      group <- Curiosity.Recall[SubjectNo == thisP & Context == thisR]$Group[1]
      this.temp <- data.table(SubjectNo = thisP, Room = thisR, Group = group, CuriosityRating = curiosity.rating, NCuriosityRating = n.curiosity.rating, RatingGroupMean = curiosity.group.mean, RatingGroupMedian = curiosity.group.median, RoomOrder = room.order, ItemFAR = false.alarm.rate, ItemAccuracy = item.accuracy, ContextAccuracy = context.hit.rate, HitRate = hit.rate, SeenHit = seen.hit, SeenFAR = false.seen.hit, CorrSeenHit = corr.seen.hit)
      OutsideObjectsMemory <- rbind(OutsideObjectsMemory, this.temp)  
      
    }
  }
}

OutsideObjectsMemory[CorrSeenHit < 0]$CorrSeenHit <- NA

## Check the context memory test response for corrected labelled distractors
Curiosity.Recall.New <- Curiosity.Recall[Context == 'None' & ObjectResponse == 'New']
Curiosity.Recall.New$ContextResponseType <- 'None'
Curiosity.Recall.New[ContextResponse != 'None']$ContextResponseType <- 'Room'

ContextRespFreqCal <- function(ObjResp, ... ){
  
  FreqTable = as.data.frame(table(ObjResp)/sum(table(ObjResp)))
  names(FreqTable) <- c("Response", "Frequency")
  
  if (nrow(FreqTable) < 2) {
    FreqTable <- rbind(FreqTable, data.frame(Response = 'Room', Frequency = c(0)))
  }
  
  return(FreqTable)
}

Curiosity.Recall.New.Freq <- Curiosity.Recall.New[, c(ContextRespFreqCal(ContextResponseType)), by = c("SubjectNo", "Group")]

## Check the context memory test response for correctly recollected objects
Curiosity.Recall.Old.Corr <- Curiosity.Recall.Old[Accuracy == 1]
Curiosity.Recall.Old.Corr$ContextResponseType <- "CorrectRoom"
Curiosity.Recall.Old.Corr[ContextResponse == 'None' & ContextAccuracy == 0]$ContextResponseType <- 'None'
Curiosity.Recall.Old.Corr[ContextResponse != 'None' & ContextAccuracy == 0]$ContextResponseType <- 'OtherRoom'

ContextOldRespFreqCal <- function(ObjResp, ... ){
  
  rsps <- c('None', 'OtherRoom', 'CorrectRoom')
  
  FreqTable = as.data.frame(table(ObjResp)/sum(table(ObjResp)))
  names(FreqTable) <- c("Response", "Frequency")
  
  if (nrow(FreqTable) == 2) {
    theRooms <- as.character(FreqTable$Response)
    missingRoom <- rsps[! (rsps %in% theRooms)]
    FreqTable <- rbind(FreqTable, data.frame(Response = missingRoom, Frequency = c(0)))
  }
  
  if (nrow(FreqTable) == 1){
    theRooms <- as.character(FreqTable$Response)
    missingRoom <- rsps[! (rsps %in% theRooms)]
    FreqTable <- rbind(FreqTable, data.frame(Response = missingRoom[1], Frequency = c(0)))
    FreqTable <- rbind(FreqTable, data.frame(Response = missingRoom[2], Frequency = c(0)))
  }
  
  return(FreqTable)
}

Curiosity.Recall.Old.Corr.Freq <- NULL

for (thisP in Participants) {
  for (thisG in c("Familiar", "Novel")) {
    thisP.data <- Curiosity.Recall.Old.Corr[SubjectNo == thisP & Group == thisG, c(ContextOldRespFreqCal(ContextResponseType))]
    thisP.data$SubjectNo <- thisP
    thisP.data$Group <- thisG
    Curiosity.Recall.Old.Corr.Freq <- rbind(Curiosity.Recall.Old.Corr.Freq, thisP.data)
  }
}

Curiosity.Recall.Old.Corr.Freq <- data.table(Curiosity.Recall.Old.Corr.Freq)

## Collapse the data
### Calculate corrected seen hit rate
#### First calculate the seen FAR for each participant
Curiosity.Recall.New <- Curiosity.Recall[Context == "None"]
Curiosity.Recall.Old$SeenFAR <- 0
Curiosity.Recall.Old$CorrSeenHit <- 0
Curiosity.Recall.Old$ContextFAR <- 0
Curiosity.Recall.Old$CorrContextAccuracy <- 0

OutsideObjectsMemory$ContextFAR          <- 0
OutsideObjectsMemory$CorrContextAccuracy <- 0

# Calculate corrected hit rate for source memory
Curiosity.Recall.New$ContextFAR <- 0

for (this.p in participant.folders) {
  for (this.r in Rooms) {
    this.false.response <- Curiosity.Recall.New[SubjectNo == this.p & ContextResponse == this.r]
    if (nrow(this.false.response) > 0) {
      Curiosity.Recall.New[SubjectNo == this.p & ContextResponse == this.r]$ContextFAR <- nrow(this.false.response)/nrow(Curiosity.Recall.New[SubjectNo == this.p])
    }
  }
}

for (this.p in participant.folders) {
  Curiosity.Recall.Old[SubjectNo == this.p]$SeenFAR <- mean(Curiosity.Recall.New[SubjectNo == this.p]$FalseSeenHit)
  Curiosity.Recall.Old[SubjectNo == this.p]$CorrSeenHit <- Curiosity.Recall.Old[SubjectNo == this.p]$SeenHit - Curiosity.Recall.Old[SubjectNo == this.p]$SeenFAR

  for (this.r in Rooms) {
    this.context.far <- as.numeric(unique(Curiosity.Recall.New[SubjectNo == this.p & ContextResponse == this.r]$ContextFAR))

    if (length(this.context.far) > 0) {
      Curiosity.Recall.Old[SubjectNo == this.p & Context == this.r]$ContextFAR       <- this.context.far 
      OutsideObjectsMemory[SubjectNo == this.p & Room == this.r]$ContextFAR          <- this.context.far
    }

    Curiosity.Recall.Old[SubjectNo == this.p & Context == this.r]$CorrContextAccuracy <- Curiosity.Recall.Old[SubjectNo == this.p & Context == this.r]$ContextAccuracy - Curiosity.Recall.Old[SubjectNo == this.p & Context == this.r]$ContextFAR
    OutsideObjectsMemory[SubjectNo == this.p & Room == this.r]$CorrContextAccuracy <- OutsideObjectsMemory[SubjectNo == this.p & Room == this.r]$ContextAccuracy  
  }
}

OutsideObjectsMemory[CorrContextAccuracy < 0]$CorrContextAccuracy <- NA

Curiosity.Recall.Old$ItemOrderType <- "Early"
Curiosity.Recall.Old[ItemOrder %in% c(4:6)]$ItemOrderType <- "Late"

Curiosity.Recall.Old.Novelty                 <- Curiosity.Recall.Old[, .(Accuracy = mean(Accuracy), SeenHit = mean(SeenHit), CorrSeenHit = mean(CorrSeenHit), ContextAccuracy = mean(ContextAccuracy), CorrContextAccuracy = mean(CorrContextAccuracy)), by = c("SubjectNo", "ItemOrder", "Group")]
Curiosity.Recall.Old.CuriosityRating         <- Curiosity.Recall.Old[, .(Accuracy = mean(Accuracy), SeenHit = mean(SeenHit), CorrSeenHit = mean(CorrSeenHit), ContextAccuracy = mean(ContextAccuracy), CorrContextAccuracy = mean(CorrContextAccuracy)), by = c("SubjectNo", "ItemOrder", "CuriosityRating")]
Curiosity.Recall.Old.CuriosityRatingN        <- Curiosity.Recall.Old[, .(Accuracy = mean(Accuracy), SeenHit = mean(SeenHit), CorrSeenHit = mean(CorrSeenHit), ContextAccuracy = mean(ContextAccuracy), CorrContextAccuracy = mean(CorrContextAccuracy)), by = c("SubjectNo", "ItemOrder", "NCuriosityRating")]
Curiosity.Recall.Old.CuriosityType           <- Curiosity.Recall.Old[, .(Accuracy = mean(Accuracy), SeenHit = mean(SeenHit), CorrSeenHit = mean(CorrSeenHit), ContextAccuracy = mean(ContextAccuracy), CorrContextAccuracy = mean(CorrContextAccuracy)), by = c("SubjectNo", "ItemOrder", "CuriosityRatingType")]
Curiosity.Recall.Old.Interaction             <- Curiosity.Recall.Old[, .(Accuracy = mean(Accuracy), SeenHit = mean(SeenHit), CorrSeenHit = mean(CorrSeenHit), ContextAccuracy = mean(ContextAccuracy), CorrContextAccuracy = mean(CorrContextAccuracy)), by = c("SubjectNo", "Group", "CuriosityRatingType")]
Curiosity.Recall.Old.Order.Novelty           <- Curiosity.Recall.Old[, .(Accuracy = mean(Accuracy), SeenHit = mean(SeenHit), CorrSeenHit = mean(CorrSeenHit), ContextAccuracy = mean(ContextAccuracy), CorrContextAccuracy = mean(CorrContextAccuracy)), by = c("SubjectNo", "ItemOrderType", "Group")]
Curiosity.Recall.Old.Order.CuriosityType     <- Curiosity.Recall.Old[, .(Accuracy = mean(Accuracy), SeenHit = mean(SeenHit), CorrSeenHit = mean(CorrSeenHit), ContextAccuracy = mean(ContextAccuracy), CorrContextAccuracy = mean(CorrContextAccuracy)), by = c("SubjectNo", "ItemOrderType", "CuriosityRatingType")]
Curiosity.Recall.Old.Collapsed.CuriosityType <- Curiosity.Recall.Old[, .(Accuracy = mean(Accuracy), SeenHit = mean(SeenHit), CorrSeenHit = mean(CorrSeenHit), ContextAccuracy = mean(ContextAccuracy), CorrContextAccuracy = mean(CorrContextAccuracy)), by = c("SubjectNo", "CuriosityRatingType")]

Curiosity.Recall.Old.Novelty[CorrSeenHit < 0]$CorrSeenHit                 <- NA
Curiosity.Recall.Old.CuriosityRating[CorrSeenHit < 0]$CorrSeenHit         <- NA
Curiosity.Recall.Old.CuriosityRatingN[CorrSeenHit < 0]$CorrSeenHit        <- NA
Curiosity.Recall.Old.CuriosityType[CorrSeenHit < 0]$CorrSeenHit           <- NA
Curiosity.Recall.Old.Interaction[CorrSeenHit < 0]$CorrSeenHit             <- NA
Curiosity.Recall.Old.Order.Novelty[CorrSeenHit < 0]$CorrSeenHit           <- NA
Curiosity.Recall.Old.Order.CuriosityType[CorrSeenHit < 0]$CorrSeenHit     <- NA
Curiosity.Recall.Old.Collapsed.CuriosityType[CorrSeenHit < 0]$CorrSeenHit <- NA
Curiosity.Recall.Old.Novelty[CorrContextAccuracy < 0]$CorrContextAccuracy                  <- NA
Curiosity.Recall.Old.CuriosityRating[CorrContextAccuracy < 0]$CorrContextAccuracy          <- NA
Curiosity.Recall.Old.CuriosityRatingN[CorrContextAccuracy < 0]$CorrContextAccuracy         <- NA
Curiosity.Recall.Old.CuriosityType[CorrContextAccuracy < 0]$CorrContextAccuracy            <- NA
Curiosity.Recall.Old.Interaction[CorrContextAccuracy < 0]$CorrContextAccuracy              <- NA
Curiosity.Recall.Old.Order.Novelty[CorrContextAccuracy < 0]$CorrContextAccuracy            <- NA
Curiosity.Recall.Old.Order.CuriosityType[CorrContextAccuracy < 0]$CorrContextAccuracy      <- NA
Curiosity.Recall.Old.Collapsed.CuriosityType[CorrContextAccuracy < 0]$CorrContextAccuracy  <- NA

Curiosity.Recall.Old.CuriosityType            <- Curiosity.Recall.Old.CuriosityType[CuriosityRatingType != 0]
Curiosity.Recall.Old.Interaction              <- Curiosity.Recall.Old.Interaction[CuriosityRatingType != 0]             
Curiosity.Recall.Old.Order.CuriosityType      <- Curiosity.Recall.Old.Order.CuriosityType[CuriosityRatingType != 0]     
Curiosity.Recall.Old.Collapsed.CuriosityType  <- Curiosity.Recall.Old.Collapsed.CuriosityType[CuriosityRatingType != 0]

Curiosity.Recall.Old.CuriosityType$CuriosityRatingType           <- factor(Curiosity.Recall.Old.CuriosityType$CuriosityRatingType, levels = c(1:2), labels = c("Low", "High"))
Curiosity.Recall.Old.Interaction$CuriosityRatingType             <- factor(Curiosity.Recall.Old.Interaction$CuriosityRatingType, levels = c(1:2), labels = c("Low", "High"))
Curiosity.Recall.Old.Order.CuriosityType$CuriosityRatingType     <- factor(Curiosity.Recall.Old.Order.CuriosityType$CuriosityRatingType, levels = c(1:2), labels = c("Low", "High"))
Curiosity.Recall.Old.Collapsed.CuriosityType$CuriosityRatingType <- factor(Curiosity.Recall.Old.Collapsed.CuriosityType$CuriosityRatingType, levels = c(1:2), labels = c("Low", "High"))

### Using alternative ways to calculate curiosity rating groups
Curiosity.Recall.Old.CuriosityType.MeanSep                 <- Curiosity.Recall.Old[, .(Accuracy = mean(Accuracy), SeenHit = mean(SeenHit), CorrSeenHit = mean(CorrSeenHit), ContextAccuracy = mean(ContextAccuracy), CorrContextAccuracy = mean(CorrContextAccuracy)), by = c("SubjectNo", "ItemOrder",  "RatingGroup")]
Curiosity.Recall.Old.Interaction.MeanSep                   <- Curiosity.Recall.Old[, .(Accuracy = mean(Accuracy), SeenHit = mean(SeenHit), CorrSeenHit = mean(CorrSeenHit), ContextAccuracy = mean(ContextAccuracy), CorrContextAccuracy = mean(CorrContextAccuracy)), by = c("SubjectNo", "Group", "RatingGroup")]
Curiosity.Recall.Old.Order.CuriosityType.MeanSep           <- Curiosity.Recall.Old[, .(Accuracy = mean(Accuracy), SeenHit = mean(SeenHit), CorrSeenHit = mean(CorrSeenHit), ContextAccuracy = mean(ContextAccuracy), CorrContextAccuracy = mean(CorrContextAccuracy)), by = c("SubjectNo", "ItemOrderType", "RatingGroup")]
Curiosity.Recall.Old.Collapsed.CuriosityType.MeanSep       <- Curiosity.Recall.Old[, .(Accuracy = mean(Accuracy), SeenHit = mean(SeenHit), CorrSeenHit = mean(CorrSeenHit), ContextAccuracy = mean(ContextAccuracy), CorrContextAccuracy = mean(CorrContextAccuracy)), by = c("SubjectNo", "RatingGroup")]

Curiosity.Recall.Old.CuriosityType.MeanSep [CorrSeenHit < 0]$CorrSeenHit          <- NA
Curiosity.Recall.Old.Interaction.MeanSep [CorrSeenHit < 0]$CorrSeenHit            <- NA
Curiosity.Recall.Old.Order.CuriosityType.MeanSep [CorrSeenHit < 0]$CorrSeenHit    <- NA
Curiosity.Recall.Old.Collapsed.CuriosityType.MeanSep[CorrSeenHit < 0]$CorrSeenHit <- NA
Curiosity.Recall.Old.CuriosityType.MeanSep[CorrContextAccuracy < 0]$CorrContextAccuracy            <- NA
Curiosity.Recall.Old.Interaction.MeanSep[CorrContextAccuracy < 0]$CorrContextAccuracy              <- NA
Curiosity.Recall.Old.Order.CuriosityType.MeanSep[CorrContextAccuracy < 0]$CorrContextAccuracy      <- NA
Curiosity.Recall.Old.Collapsed.CuriosityType.MeanSep[CorrContextAccuracy < 0]$CorrContextAccuracy  <- NA

Curiosity.Recall.Old.CuriosityType.MeanSep            <- Curiosity.Recall.Old.CuriosityType.MeanSep[RatingGroup != 0]
Curiosity.Recall.Old.Interaction.MeanSep              <- Curiosity.Recall.Old.Interaction.MeanSep[RatingGroup != 0]             
Curiosity.Recall.Old.Order.CuriosityType.MeanSep      <- Curiosity.Recall.Old.Order.CuriosityType.MeanSep[RatingGroup != 0]     
Curiosity.Recall.Old.Collapsed.CuriosityType.MeanSep  <- Curiosity.Recall.Old.Collapsed.CuriosityType.MeanSep[RatingGroup != 0]

Curiosity.Recall.Old.CuriosityType.MeanSep$RatingGroup               <- factor(Curiosity.Recall.Old.CuriosityType.MeanSep$RatingGroup,           levels = c(1:2), labels = c("Low", "High"))
Curiosity.Recall.Old.Interaction.MeanSep$RatingGroup                 <- factor(Curiosity.Recall.Old.Interaction.MeanSep$RatingGroup,             levels = c(1:2), labels = c("Low", "High"))
Curiosity.Recall.Old.Order.CuriosityType.MeanSep$RatingGroup         <- factor(Curiosity.Recall.Old.Order.CuriosityType.MeanSep$RatingGroup,     levels = c(1:2), labels = c("Low", "High"))
Curiosity.Recall.Old.Collapsed.CuriosityType.MeanSep$RatingGroup     <- factor(Curiosity.Recall.Old.Collapsed.CuriosityType.MeanSep$RatingGroup, levels = c(1:2), labels = c("Low", "High"))


