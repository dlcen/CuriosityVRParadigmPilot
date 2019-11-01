library(data.table); library(ggplot2); library(Hmisc)

## The familiar and novel groups
grouping <- data.frame(Familiar = c("Bedroom", "Classroom", "Gym"), Novel = c("Library", "LivingRoom", "StorageRoom"))

## Get the list of participant's folders
participant.folders <- dir(pattern = "^P")

## Get the rating in each folder (also need to determine whether the room is familiar or novel)
Encoding.Recall <- NULL

for (thisFolder in participant.folders) {
  this.response <- read.csv(paste0(thisFolder, .Platform$file.sep, "Encoding_Memory_Test_Response.csv"), header = T)
  this.response<- data.table(this.response)
  
  this.response$SubjectNo <- thisFolder
  this.response$Group <- "Familiar"

  this.subjectNo <- as.numeric(strsplit(thisFolder, 'P')[[1]][2])

  if (this.subjectNo %in% GroupA) {
        this.response[Context %in% grouping$Novel]$Group <- "Novel"
  } else {
        this.response[Context %in% grouping$Familiar]$Group <- "Novel"
  }
  
  this.response[Context == "None"]$Group <- "Distractor"
  
  Encoding.Recall <- rbind(Encoding.Recall, this.response)
}

Encoding.Recall$CorrObjResp <- "Seen"
Encoding.Recall[Group == "Distractor"]$CorrObjResp <- "New"

AccuracyCal <- function(ActualResp, CorrResp){
	if (CorrResp == "Seen" & ActualResp %in% c("Seen", "Familiar")) {
		Accuracy <- 1
	} else if (CorrResp == "New" & ActualResp == "New") {
		Accuracy <- 1
	} else {
		Accuracy <- 0
	}	

	return(Accuracy)
}

SeenAccuracyCal <- function(ActualResp, CorrResp){
  if (CorrResp == "Seen" & ActualResp == "Seen") {
    Accuracy <- 1
  } else if (CorrResp == "New" & ActualResp == "New") {
    Accuracy <- 1
  } else {
    Accuracy <- 0
  }	
  
  return(Accuracy)
}

FalseSeenHitCal <- function(ActualResp, CorrResp){
  if (CorrResp == 'New' & ActualResp == 'Seen') {
    FalseSeen <- 1
  } else {
    FalseSeen <- 0
  }
}

Encoding.Recall[, Accuracy :=  mapply(AccuracyCal,ObjectResponse, CorrObjResp)]
Encoding.Recall[, SeenHit  :=  mapply(SeenAccuracyCal, ObjectResponse, CorrObjResp)]
Encoding.Recall[, FalseSeenHit := mapply(FalseSeenHitCal, ObjectResponse, CorrObjResp)]

## Split the old and new items
old.item.responses <- Encoding.Recall[CorrObjResp == "Seen"]
new.item.responses <- Encoding.Recall[CorrObjResp == "New"]

## Calculate false alarm rate
new.item.responses[, far := 1 - Accuracy]

# Context memory
ContextAccuracyCal <- function(ActualResp, CorrResp) {
  if (ActualResp == CorrResp) {
    Accuracy = 1
  } else {
    Accuracy = 0
  }
  
  return(Accuracy)
}

Encoding.Recall[, ContextAccuracy := mapply(ContextAccuracyCal, ContextResponse, Context)]

## Force the context accuracy to be 0 if the corresponding item accuracy is 0
Encoding.Recall[Accuracy == 0 & ContextAccuracy == 1]  # Seems that this situation only happened for distractors


## Calculate the probability of choose each option respectively for old and new objects for each indivdual participant
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

## Calculate hit rate with only "seen" response
Rooms <- c("Bedroom", "Classroom", "Gym", "LivingRoom", "Library", "StorageRoom")
InsideObjectsMemory <- NULL

for (thisP in participant.folders) {
  for (thisR in Rooms) {
    thisD <- Encoding.Recall[SubjectNo == thisP & Context == thisR]
    
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
      group <- Encoding.Recall[SubjectNo == thisP & Context == thisR]$Group[1]
      this.temp <- data.table(SubjectNo = thisP, Room = thisR, Group = group, CuriosityRating = curiosity.rating, NCuriosityRating = n.curiosity.rating, RatingGroupMean = curiosity.group.mean, RatingGroupMedian = curiosity.group.median, RoomOrder = room.order, ItemFAR = false.alarm.rate, ItemAccuracy = item.accuracy, ContextAccuracy = context.hit.rate, HitRate = hit.rate, SeenHit = seen.hit, SeenFAR = false.seen.hit, CorrSeenHit = corr.seen.hit)
      InsideObjectsMemory <- rbind(InsideObjectsMemory, this.temp)  
      
    }
  }
}

InsideObjectsMemory[CorrSeenHit < 0]$CorrSeenHit <- NA

## Check the context memory test response for corrected labelled distractors
Encoding.Recall.New <- Encoding.Recall[Context == 'None' & ObjectResponse == 'New']
Encoding.Recall.New$ContextResponseType <- 'None'
Encoding.Recall.New[ContextResponse != 'None']$ContextResponseType <- 'Room'

ContextRespFreqCal <- function(ObjResp, ... ){
  
  FreqTable = as.data.frame(table(ObjResp)/sum(table(ObjResp)))
  names(FreqTable) <- c("Response", "Frequency")
  
  if (nrow(FreqTable) < 2) {
    FreqTable <- rbind(FreqTable, data.frame(Response = 'Room', Frequency = c(0)))
  }

  return(FreqTable)
}

Encoding.Recall.New.Freq <- Encoding.Recall.New[, c(ContextRespFreqCal(ContextResponseType)), by = c("SubjectNo", "Group")]

## Check the context memory test response for correctly recollected objects
Encoding.Recall.Old.Corr <- Encoding.Recall.Old[Accuracy == 1]
Encoding.Recall.Old.Corr$ContextResponseType <- "CorrectRoom"
Encoding.Recall.Old.Corr[ContextResponse == 'None' & ContextAccuracy == 0]$ContextResponseType <- 'None'
Encoding.Recall.Old.Corr[ContextResponse != 'None' & ContextAccuracy == 0]$ContextResponseType <- 'OtherRoom'

ContextOldRespFreqCal <- function(ObjResp, ... ){
  
  rsps <- c('None', 'OtherRoom', 'CorrectRoom')
  
  FreqTable <- as.data.frame(table(ObjResp)/sum(table(ObjResp)))
  names(FreqTable) <- c("Response", "Frequency")
  
  if (nrow(FreqTable) < 3) {
    if (nrow(FreqTable) == 2) {
      theRooms <- as.character(FreqTable$Response)
      missingRoom <- rsps[! (rsps %in% theRooms)]
      FreqTable <- rbind(FreqTable, data.frame(Response = missingRoom, Frequency = c(0)))
    } else if (nrow(FreqTable) == 1){
      theRooms <- as.character(FreqTable$Response)
      missingRoom <- rsps[! (rsps %in% theRooms)]
      FreqTable <- rbind(FreqTable, data.frame(Response = missingRoom[1], Frequency = c(0)))
      FreqTable <- rbind(FreqTable, data.frame(Response = missingRoom[2], Frequency = c(0)))
    }
  }
  
  return(FreqTable)
}

Encoding.Recall.Old.Corr.Freq <- NULL

for (thisP in Participants) {
  for (thisG in c("Familiar", "Novel")) {
    thisP.data <- Encoding.Recall.Old.Corr[SubjectNo == thisP & Group == thisG, c(ContextOldRespFreqCal(ContextResponseType))]
    thisP.data$SubjectNo <- thisP
    thisP.data$Group <- thisG
    Encoding.Recall.Old.Corr.Freq <- rbind(Encoding.Recall.Old.Corr.Freq, thisP.data)
  }
}

Encoding.Recall.Old.Corr.Freq <- data.table(Encoding.Recall.Old.Corr.Freq)

## Collapse the data
### Calculate corrected seen hit rate
#### First calculate the seen FAR for each participant
Encoding.Recall.New <- Encoding.Recall[Context == "None"]
Encoding.Recall.Old$SeenFAR <- 0
Encoding.Recall.Old$CorrSeenHit <- 0
Encoding.Recall.Old$ContextFAR <- 0
Encoding.Recall.Old$CorrContextAccuracy <- 0

InsideObjectsMemory$ContextFAR          <- 0
InsideObjectsMemory$CorrContextAccuracy <- 0

# Calculate corrected hit rate for source memory
Encoding.Recall.New$ContextFAR <- 0

for (this.p in participant.folders) {
  for (this.r in Rooms) {
    this.false.response <- Encoding.Recall.New[SubjectNo == this.p & ContextResponse == this.r]
    if (nrow(this.false.response) > 0) {
      Encoding.Recall.New[SubjectNo == this.p & ContextResponse == this.r]$ContextFAR <- nrow(this.false.response)/nrow(Encoding.Recall.New[SubjectNo == this.p])
    }
  }
}

for (this.p in participant.folders) {
  Encoding.Recall.Old[SubjectNo == this.p]$SeenFAR <- mean(Encoding.Recall.New[SubjectNo == this.p]$FalseSeenHit)
  Encoding.Recall.Old[SubjectNo == this.p]$CorrSeenHit <- Encoding.Recall.Old[SubjectNo == this.p]$SeenHit - Encoding.Recall.Old[SubjectNo == this.p]$SeenFAR

  for (this.r in Rooms) {
    this.context.far <- as.numeric(unique(Encoding.Recall.New[SubjectNo == this.p & ContextResponse == this.r]$ContextFAR))

    if (length(this.context.far) > 0) {
      Encoding.Recall.Old[SubjectNo == this.p & Context == this.r]$ContextFAR       <- this.context.far 
      InsideObjectsMemory[SubjectNo == this.p & Room == this.r]$ContextFAR          <- this.context.far
    }

    Encoding.Recall.Old[SubjectNo == this.p & Context == this.r]$CorrContextAccuracy <- Encoding.Recall.Old[SubjectNo == this.p & Context == this.r]$ContextAccuracy - Encoding.Recall.Old[SubjectNo == this.p & Context == this.r]$ContextFAR
    InsideObjectsMemory[SubjectNo == this.p & Room == this.r]$CorrContextAccuracy <- InsideObjectsMemory[SubjectNo == this.p & Room == this.r]$ContextAccuracy  
  }
}

InsideObjectsMemory[CorrContextAccuracy < 0]$CorrContextAccuracy <- NA

Encoding.Recall.Old$ItemOrderType <- "Early"
Encoding.Recall.Old[ItemOrder %in% c(4:6)]$ItemOrderType <- "Late"

Encoding.Recall.Old.Novelty                 <- Encoding.Recall.Old[, .(Accuracy = mean(Accuracy), SeenHit = mean(SeenHit), CorrSeenHit = mean(CorrSeenHit), ContextAccuracy = mean(ContextAccuracy), CorrContextAccuracy = mean(CorrContextAccuracy)), by = c("SubjectNo", "ItemOrder", "Group")]
Encoding.Recall.Old.CuriosityRating         <- Encoding.Recall.Old[, .(Accuracy = mean(Accuracy), SeenHit = mean(SeenHit), CorrSeenHit = mean(CorrSeenHit), ContextAccuracy = mean(ContextAccuracy), CorrContextAccuracy = mean(CorrContextAccuracy)), by = c("SubjectNo", "ItemOrder", "CuriosityRating")]
Encoding.Recall.Old.CuriosityRatingN        <- Encoding.Recall.Old[, .(Accuracy = mean(Accuracy), SeenHit = mean(SeenHit), CorrSeenHit = mean(CorrSeenHit), ContextAccuracy = mean(ContextAccuracy), CorrContextAccuracy = mean(CorrContextAccuracy)), by = c("SubjectNo", "ItemOrder", "NCuriosityRating")]
Encoding.Recall.Old.CuriosityType           <- Encoding.Recall.Old[, .(Accuracy = mean(Accuracy), SeenHit = mean(SeenHit), CorrSeenHit = mean(CorrSeenHit), ContextAccuracy = mean(ContextAccuracy), CorrContextAccuracy = mean(CorrContextAccuracy)), by = c("SubjectNo", "ItemOrder", "CuriosityRatingType")]
Encoding.Recall.Old.Interaction             <- Encoding.Recall.Old[, .(Accuracy = mean(Accuracy), SeenHit = mean(SeenHit), CorrSeenHit = mean(CorrSeenHit), ContextAccuracy = mean(ContextAccuracy), CorrContextAccuracy = mean(CorrContextAccuracy)), by = c("SubjectNo", "Group", "CuriosityRatingType")]
Encoding.Recall.Old.Order.Novelty           <- Encoding.Recall.Old[, .(Accuracy = mean(Accuracy), SeenHit = mean(SeenHit), CorrSeenHit = mean(CorrSeenHit), ContextAccuracy = mean(ContextAccuracy), CorrContextAccuracy = mean(CorrContextAccuracy)), by = c("SubjectNo", "ItemOrderType", "Group")]
Encoding.Recall.Old.Order.CuriosityType     <- Encoding.Recall.Old[, .(Accuracy = mean(Accuracy), SeenHit = mean(SeenHit), CorrSeenHit = mean(CorrSeenHit), ContextAccuracy = mean(ContextAccuracy), CorrContextAccuracy = mean(CorrContextAccuracy)), by = c("SubjectNo", "ItemOrderType", "CuriosityRatingType")]
Encoding.Recall.Old.Collapsed.CuriosityType <- Encoding.Recall.Old[, .(Accuracy = mean(Accuracy), SeenHit = mean(SeenHit), CorrSeenHit = mean(CorrSeenHit), ContextAccuracy = mean(ContextAccuracy), CorrContextAccuracy = mean(CorrContextAccuracy)), by = c("SubjectNo", "CuriosityRatingType")]

Encoding.Recall.Old.Novelty[CorrSeenHit < 0]$CorrSeenHit                 <- NA
Encoding.Recall.Old.CuriosityRating[CorrSeenHit < 0]$CorrSeenHit         <- NA
Encoding.Recall.Old.CuriosityRatingN[CorrSeenHit < 0]$CorrSeenHit        <- NA
Encoding.Recall.Old.CuriosityType[CorrSeenHit < 0]$CorrSeenHit           <- NA
Encoding.Recall.Old.Interaction[CorrSeenHit < 0]$CorrSeenHit             <- NA
Encoding.Recall.Old.Order.Novelty[CorrSeenHit < 0]$CorrSeenHit           <- NA
Encoding.Recall.Old.Order.CuriosityType[CorrSeenHit < 0]$CorrSeenHit     <- NA
Encoding.Recall.Old.Collapsed.CuriosityType[CorrSeenHit < 0]$CorrSeenHit <- NA

Encoding.Recall.Old.Novelty[CorrContextAccuracy < 0]$CorrContextAccuracy                  <- NA
Encoding.Recall.Old.CuriosityRating[CorrContextAccuracy < 0]$CorrContextAccuracy          <- NA
Encoding.Recall.Old.CuriosityRatingN[CorrContextAccuracy < 0]$CorrContextAccuracy         <- NA
Encoding.Recall.Old.CuriosityType[CorrContextAccuracy < 0]$CorrContextAccuracy            <- NA
Encoding.Recall.Old.Interaction[CorrContextAccuracy < 0]$CorrContextAccuracy              <- NA
Encoding.Recall.Old.Order.Novelty[CorrContextAccuracy < 0]$CorrContextAccuracy            <- NA
Encoding.Recall.Old.Order.CuriosityType[CorrContextAccuracy < 0]$CorrContextAccuracy      <- NA
Encoding.Recall.Old.Collapsed.CuriosityType[CorrContextAccuracy < 0]$CorrContextAccuracy  <- NA

Encoding.Recall.Old.CuriosityType            <- Encoding.Recall.Old.CuriosityType[CuriosityRatingType != 0]
Encoding.Recall.Old.Interaction              <- Encoding.Recall.Old.Interaction[CuriosityRatingType != 0]             
Encoding.Recall.Old.Order.CuriosityType      <- Encoding.Recall.Old.Order.CuriosityType[CuriosityRatingType != 0]     
Encoding.Recall.Old.Collapsed.CuriosityType  <- Encoding.Recall.Old.Collapsed.CuriosityType[CuriosityRatingType != 0]

Encoding.Recall.Old.CuriosityType$CuriosityRatingType           <- factor(Encoding.Recall.Old.CuriosityType$CuriosityRatingType, levels = c(1:2), labels = c("Low", "High"))
Encoding.Recall.Old.Interaction$CuriosityRatingType             <- factor(Encoding.Recall.Old.Interaction$CuriosityRatingType, levels = c(1:2), labels = c("Low", "High"))
Encoding.Recall.Old.Order.CuriosityType$CuriosityRatingType     <- factor(Encoding.Recall.Old.Order.CuriosityType$CuriosityRatingType, levels = c(1:2), labels = c("Low", "High"))
Encoding.Recall.Old.Collapsed.CuriosityType$CuriosityRatingType <- factor(Encoding.Recall.Old.Collapsed.CuriosityType$CuriosityRatingType, levels = c(1:2), labels = c("Low", "High"))

### Using alternative ways to calculate curiosity rating groups
Encoding.Recall.Old.CuriosityType.MeanSep                 <- Encoding.Recall.Old[, .(Accuracy = mean(Accuracy), SeenHit = mean(SeenHit), CorrSeenHit = mean(CorrSeenHit), ContextAccuracy = mean(ContextAccuracy), CorrContextAccuracy = mean(CorrContextAccuracy)), by = c("SubjectNo", "ItemOrder",  "RatingGroup")]
Encoding.Recall.Old.Interaction.MeanSep                   <- Encoding.Recall.Old[, .(Accuracy = mean(Accuracy), SeenHit = mean(SeenHit), CorrSeenHit = mean(CorrSeenHit), ContextAccuracy = mean(ContextAccuracy), CorrContextAccuracy = mean(CorrContextAccuracy)), by = c("SubjectNo", "Group", "RatingGroup")]
Encoding.Recall.Old.Order.CuriosityType.MeanSep           <- Encoding.Recall.Old[, .(Accuracy = mean(Accuracy), SeenHit = mean(SeenHit), CorrSeenHit = mean(CorrSeenHit), ContextAccuracy = mean(ContextAccuracy), CorrContextAccuracy = mean(CorrContextAccuracy)), by = c("SubjectNo", "ItemOrderType", "RatingGroup")]
Encoding.Recall.Old.Collapsed.CuriosityType.MeanSep       <- Encoding.Recall.Old[, .(Accuracy = mean(Accuracy), SeenHit = mean(SeenHit), CorrSeenHit = mean(CorrSeenHit), ContextAccuracy = mean(ContextAccuracy), CorrContextAccuracy = mean(CorrContextAccuracy)), by = c("SubjectNo", "RatingGroup")]

Encoding.Recall.Old.CuriosityType.MeanSep [CorrSeenHit < 0]$CorrSeenHit          <- NA
Encoding.Recall.Old.Interaction.MeanSep [CorrSeenHit < 0]$CorrSeenHit            <- NA
Encoding.Recall.Old.Order.CuriosityType.MeanSep [CorrSeenHit < 0]$CorrSeenHit    <- NA
Encoding.Recall.Old.Collapsed.CuriosityType.MeanSep[CorrSeenHit < 0]$CorrSeenHit <- NA

Encoding.Recall.Old.CuriosityType.MeanSep[CorrContextAccuracy < 0]$CorrContextAccuracy            <- NA
Encoding.Recall.Old.Interaction.MeanSep[CorrContextAccuracy < 0]$CorrContextAccuracy              <- NA
Encoding.Recall.Old.Order.CuriosityType.MeanSep[CorrContextAccuracy < 0]$CorrContextAccuracy      <- NA
Encoding.Recall.Old.Collapsed.CuriosityType.MeanSep[CorrContextAccuracy < 0]$CorrContextAccuracy  <- NA

Encoding.Recall.Old.CuriosityType.MeanSep            <- Encoding.Recall.Old.CuriosityType.MeanSep[RatingGroup != 0]
Encoding.Recall.Old.Interaction.MeanSep              <- Encoding.Recall.Old.Interaction.MeanSep[RatingGroup != 0]             
Encoding.Recall.Old.Order.CuriosityType.MeanSep      <- Encoding.Recall.Old.Order.CuriosityType.MeanSep[RatingGroup != 0]     
Encoding.Recall.Old.Collapsed.CuriosityType.MeanSep  <- Encoding.Recall.Old.Collapsed.CuriosityType.MeanSep[RatingGroup != 0]

Encoding.Recall.Old.CuriosityType.MeanSep$RatingGroup               <- factor(Encoding.Recall.Old.CuriosityType.MeanSep$RatingGroup,           levels = c(1:2), labels = c("Low", "High"))
Encoding.Recall.Old.Interaction.MeanSep$RatingGroup                 <- factor(Encoding.Recall.Old.Interaction.MeanSep$RatingGroup,             levels = c(1:2), labels = c("Low", "High"))
Encoding.Recall.Old.Order.CuriosityType.MeanSep$RatingGroup         <- factor(Encoding.Recall.Old.Order.CuriosityType.MeanSep$RatingGroup,     levels = c(1:2), labels = c("Low", "High"))
Encoding.Recall.Old.Collapsed.CuriosityType.MeanSep$RatingGroup     <- factor(Encoding.Recall.Old.Collapsed.CuriosityType.MeanSep$RatingGroup, levels = c(1:2), labels = c("Low", "High"))

############################################################################

Encoding.Recall.Old.Collapsed <- Encoding.Recall.Old[CuriosityRating != 4]
Encoding.Recall.Old.Collapsed.Cur <- Encoding.Recall.Old.Collapsed[, .(Accuracy = mean(Accuracy), SeenHit = mean(SeenHit), ContextAccuracy = mean(ContextAccuracy)), by = c("SubjectNo", "CuriosityRatingType")]
Encoding.Recall.Old.Collapsed.Interaction <- Encoding.Recall.Old.Collapsed[, .(Accuracy = mean(Accuracy), SeenHit = mean(SeenHit), ContextAccuracy = mean(ContextAccuracy)), by = c("SubjectNo", "Group", "CuriosityRatingType")]
Encoding.Recall.Old.Collapsed <- Encoding.Recall.Old.Collapsed[, .(Accuracy = mean(Accuracy), SeenHit = mean(SeenHit), ContextAccuracy = mean(ContextAccuracy)), by = c("SubjectNo", "ItemOrderType", "CuriosityRatingType")]
Encoding.Recall.Old.Collapsed.Grp <- Encoding.Recall.Old[, .(Accuracy = mean(Accuracy), SeenHit = mean(SeenHit), ContextAccuracy = mean(ContextAccuracy)), by = c("SubjectNo", "ItemOrderType", "Group")]

### Using alternative ways to calculate curiosity rating groups
Encoding.Recall.Old$RatingGroup <- ""

for (this.p in participant.folders) {
  this.p.ratings <- Encoding.Recall.Old[SubjectNo == this.p]
  this.mean <- mean(this.p.ratings$CuriosityRating )
  Encoding.Recall.Old[SubjectNo == this.p & CuriosityRating < this.mean]$RatingGroup <- "Low"
  Encoding.Recall.Old[SubjectNo == this.p & CuriosityRating > this.mean]$RatingGroup <- "High"
}

Encoding.Recall.Old.Collapsed.Alt <- Encoding.Recall.Old[RatingGroup != ""]
Encoding.Recall.Old.Collapsed.Alt.Cur <- Encoding.Recall.Old.Collapsed.Alt[, .(Accuracy = mean(Accuracy), SeenHit = mean(SeenHit), ContextAccuracy = mean(ContextAccuracy)), by = c("SubjectNo", "RatingGroup")]
Encoding.Recall.Old.Collapsed.Alt.Interaction <- Encoding.Recall.Old.Collapsed.Alt[, .(Accuracy = mean(Accuracy), SeenHit = mean(SeenHit), ContextAccuracy = mean(ContextAccuracy)), by = c("SubjectNo", "Group", "RatingGroup")]
Encoding.Recall.Old.Collapsed.Alt <- Encoding.Recall.Old.Collapsed.Alt[, .(Accuracy = mean(Accuracy), SeenHit = mean(SeenHit), ContextAccuracy = mean(ContextAccuracy)), by = c("SubjectNo", "ItemOrderType", "RatingGroup")]


