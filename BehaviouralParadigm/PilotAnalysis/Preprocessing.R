library(data.table)

all.participant.list <- list.files(path = "./PilotData/IndividualData/", pattern = "^P")

# Ratings
ratings <- NULL

for (this.p in all.participant.list) {
	this.ratings <- read.csv(paste0("./PilotData/IndividualData/", this.p, "/Exploration/Ratings.csv"), header = TRUE)
	this.ratings$SubjectNo <- this.p
	ratings  	 <- rbind(ratings, this.ratings)
}

ratings <- data.table(ratings)
ratings <- ratings[, c(4, 1, 2, 3)]
ratings$Surprise <- ratings$Interest - ratings$Curiosity

rooms <- as.character(unique(ratings$Room))

# Durations
durations <- NULL

for (this.p in all.participant.list) {
	this.durs <- read.csv(paste0("./PilotData/IndividualData/", this.p, "/Exploration/TaskDurations.csv"), header = TRUE)
	this.durs$SubjectNo <- this.p
	durations  	 <- rbind(durations, this.durs)
}

durations <- data.table(durations)
names(durations)[1] <- "Scene"

individual.data <- ratings
individual.data$InsideDuration <- durations$InsideDuration
individual.data$OutsideDuration <- durations$OutsideDuration

individual.data <- data.table(individual.data)

# individual.data[, Surprise := (Interest - Curiosity)]


# Memory performance
if (!is.day1.only) {

  object.recognition <- NULL

  source("./PilotAnalysis/RoomOrder.R")

  for (thisFolder in all.participant.list) {
    this.response <- read.csv(paste0("PilotData", .Platform$file.sep, "IndividualData", .Platform$file.sep, thisFolder, .Platform$file.sep, "TestOrder", .Platform$file.sep, "MemoryTestResponse.csv"), header = T)
    this.response <- data.table(this.response)
    
    this.response$SubjectNo <- thisFolder

    # Read the curiosity rating 
    this.ratings <- ratings[SubjectNo == thisFolder]
    this.room.order <- room.order[SubjectNo == thisFolder]

    # Add the curiosity rating to the response file
    this.response$CurRating <- 0
    this.response$IntRating <- 0
    this.response$RoomOrder <- 0

    this.response$PreCur    <- 0
    this.response$PreInt    <- 0
    this.response$PreSur    <- 0
    
    for (this.room in rooms) {
      this.response[Scene == this.room]$CurRating <- this.ratings[Room == this.room]$Curiosity
      this.response[Scene == this.room]$IntRating <- this.ratings[Room == this.room]$Interest
      this.response[Scene == this.room]$RoomOrder <- this.room.order[Room == this.room]$Order
      
      if ( ! is.na(this.room.order[Room == this.room]$PreRoom)) {
        this.response[Scene == this.room]$PreCur    <- this.ratings[Room == this.room.order[Room == this.room]$PreRoom]$Curiosity
        this.response[Scene == this.room]$PreInt    <- this.ratings[Room == this.room.order[Room == this.room]$PreRoom]$Interest
        this.response[Scene == this.room]$PreSur    <- this.ratings[Room == this.room.order[Room == this.room]$PreRoom]$Surprise
      } 
    }

    this.response[CurRating == 0]$CurRating <- NA
    this.response[IntRating == 0]$IntRating <- NA
    this.response[RoomOrder == 0]$RoomOrder <- NA

    this.response[PreCur == 0]$PreCur <- NA
    this.response[PreInt == 0]$PreInt <- NA
    this.response[PreSur == 0]$PreSur <- NA

    # Add the outside duration 
    this.durations <- individual.data[SubjectNo == thisFolder]

    this.response$OutDur <- 0

    for (this.room in rooms) {
      this.response[Scene == this.room]$OutDur <- this.durations[Room == this.room]$OutsideDuration
    }

    this.response[OutDur == 0]$OutDur <- NA

    # Add the order of items inside each room
    source("./PilotAnalysis/OutsideOrder.r")

    this.response$ItemOrder <- 0

    this.response <- this.response[order(SubjectNo, Scene, Object)]
    this.order    <- outside.orders[SubjectNo == thisFolder]
    this.order	<- this.order[order(SubjectNo, Room, Item)]
    
    this.response[Scene != "None"]$ItemOrder <- this.order$Order
    
    this.response[ItemOrder == 0]$ItemOrder <- NA

    object.recognition <- rbind(object.recognition, this.response)

  }

  object.recognition$Group <- "OldItem"
  object.recognition[is.na(ItemOrder)]$Group <- "Distractor"

  source("./PilotAnalysis/CalFunc.R")

  object.recognition[, SFHit   := mapply(SFHitCal,      Response, Scene)]
  object.recognition[, SHit    := mapply(SHitCal,       Response, Scene)]
  object.recognition[, SFFalse := mapply(SFFalseHitCal, Response, Scene)]
  object.recognition[, SFalse  := mapply(SFalseHitCal,  Response, Scene)]

  idv.false.alarm.rate <- object.recognition[Scene == "None", .(SFFalse = mean(SFFalse), SFalse = mean(SFalse)), by = c("SubjectNo")]

  ## Calculate the hit rates for each individual participant and each room
  outside.hit.rate.per.room <- object.recognition[Scene != "None", .(SFHit = mean(SFHit, na.rm = TRUE), SHit = mean(SHit, na.rm = TRUE)), by = c("SubjectNo", "Scene", "CurRating", "IntRating", "PreInt", "PreSur", "InsideDuration", "OutsideDuration", "MeanDur", "MedianDur")]
  outside.hit.rate.per.room <- outside.hit.rate.per.room[order(SubjectNo, Scene), ]
  outside.hit.rate.per.room <- merge(outside.hit.rate.per.room, idv.false.alarm.rate, all = TRUE)

  outside.hit.rate.per.room[, SFAcc  := (SFHit - SFFalse)]
  outside.hit.rate.per.room[, SAcc   := (SHit  - SFalse)]

  ## Calculate average ratings (median and mean)
  average.curiosity.rating  <- outside.hit.rate.per.room[, .(MeanCur = mean(CurRating, na.rm = TRUE), MedianCur = median(CurRating, na.rm = TRUE), MeanPreInt = mean(PreInt, na.rm = TRUE), MedianPreInt = median(PreInt, na.rm = TRUE), MeanPreSur = mean(PreSur, na.rm = TRUE), MedianPreSur = median(PreSur, na.rm = TRUE)),  by = c("SubjectNo")]
  object.recognition        <- merge(object.recognition, average.curiosity.rating, by = "SubjectNo")

  ## Calculate average exploration time (median and mean)
  average.inside.duration   <- durations[, .(MeanDur = mean(InsideDuration, na.rm = TRUE), MedianDur = median(InsideDuration, na.rm = TRUE)), by = c("SubjectNo") ]
  object.recognition        <- merge(object.recognition, durations, by = c("SubjectNo", "Scene"))
  object.recognition        <- merge(object.recognition, average.inside.duration, by = "SubjectNo")

  ## Calculate the hit rates for each individual participant and each curiosity rating
  outside.hit.rate.per.rating <- object.recognition[Scene != "None", .(SFHit = mean(SFHit, na.rm = TRUE), SHit = mean(SHit, na.rm = TRUE)), by = c("SubjectNo", "CurRating")]
  outside.hit.rate.per.rating <- outside.hit.rate.per.rating[order(SubjectNo, CurRating), ]
  outside.hit.rate.per.rating <- merge(outside.hit.rate.per.rating, idv.false.alarm.rate, all = TRUE)

  outside.hit.rate.per.rating[, SFAcc  := (SFHit - SFFalse)]
  outside.hit.rate.per.rating[, SAcc   := (SHit  - SFalse)]

  ## Calculate the hit rates for each curiosity group
  object.recognition[, CurGrp := mapply(CurGrpSep, CurRating)]

  outside.hit.rate.item.curiosity        <- object.recognition[Scene != "None", .(SFHit = mean(SFHit, na.rm = TRUE), SHit = mean(SHit, na.rm = TRUE)), by = c("SubjectNo", "CurGrp")]
  outside.hit.rate.item.curiosity        <- merge(outside.hit.rate.item.curiosity, idv.false.alarm.rate, all = TRUE)
  outside.hit.rate.item.curiosity[, c("SFAcc", "SAcc") := list( (SFHit - SFFalse), (SHit - SFalse))]

  ## Calculate the hit rates for each curiosity group that is median split
  object.recognition[, CurGrpMd := mapply(CurGrpMedianSep, CurRating, MedianCur)]

  outside.hit.rate.item.curiosity.median   <- object.recognition[!is.na(CurGrpMd), .(SFHit = mean(SFHit, na.rm = TRUE), SHit = mean(SHit, na.rm = TRUE)), by = c("SubjectNo", "CurGrpMd")]
  outside.hit.rate.item.curiosity.median   <- merge(outside.hit.rate.item.curiosity.median, idv.false.alarm.rate, all = TRUE)
  outside.hit.rate.item.curiosity.median[, c("SFAcc", "SAcc") := list( (SFHit - SFFalse), (SHit - SFalse))]

  ## Calculate the hit rates for each curiosity group that is mean split
  object.recognition[, CurGrpMn := mapply(CurGrpMeanSep, CurRating, MeanCur)]

  outside.hit.rate.item.curiosity.mean   <- object.recognition[Scene != "None", .(SFHit = mean(SFHit, na.rm = TRUE), SHit = mean(SHit, na.rm = TRUE)), by = c("SubjectNo", "CurGrpMn")]
  outside.hit.rate.item.curiosity.mean   <- merge(outside.hit.rate.item.curiosity.mean, idv.false.alarm.rate, all = TRUE)
  outside.hit.rate.item.curiosity.mean[, c("SFAcc", "SAcc") := list( (SFHit - SFFalse), (SHit - SFalse))]

  ## Calculate the hit rates for each curiosity group and order group
  object.recognition[, ObjOrdGrp := mapply(ObjOrdGrpSep, ItemOrder) ]

  outside.hit.rate.item.order.curiosity        <- object.recognition[Scene != "None", .(SFHit = mean(SFHit, na.rm = TRUE), SHit = mean(SHit, na.rm = TRUE)), by = c("SubjectNo", "CurGrp", "ObjOrdGrp")]
  outside.hit.rate.item.order.curiosity        <- merge(outside.hit.rate.item.order.curiosity, idv.false.alarm.rate, all = TRUE)
  outside.hit.rate.item.order.curiosity[, c("SFAcc", "SAcc") := list( (SFHit - SFFalse), (SHit - SFalse))]

  ## Calculate the hit rates for each curiosity group (median-splited) and order group
  object.recognition[, ObjOrdGrp := mapply(ObjOrdGrpSep, ItemOrder) ]

  outside.hit.rate.item.order.curiosity.median   <- object.recognition[!is.na(CurGrpMd), .(SFHit = mean(SFHit, na.rm = TRUE), SHit = mean(SHit, na.rm = TRUE)), by = c("SubjectNo", "CurGrpMd", "ObjOrdGrp")]
  outside.hit.rate.item.order.curiosity.median   <- merge(outside.hit.rate.item.order.curiosity.median, idv.false.alarm.rate, all = TRUE)
  outside.hit.rate.item.order.curiosity.median[, c("SFAcc", "SAcc") := list( (SFHit - SFFalse), (SHit - SFalse))]

  ## Calculate the hit rates for each curiosity group (median-splited) and order group
  object.recognition[, ObjOrdGrp := mapply(ObjOrdGrpSep, ItemOrder) ]

  outside.hit.rate.item.order.curiosity.mean   <- object.recognition[Scene != "None", .(SFHit = mean(SFHit, na.rm = TRUE), SHit = mean(SHit, na.rm = TRUE)), by = c("SubjectNo", "CurGrpMn", "ObjOrdGrp")]
  outside.hit.rate.item.order.curiosity.mean   <- merge(outside.hit.rate.item.order.curiosity.mean, idv.false.alarm.rate, all = TRUE)
  outside.hit.rate.item.order.curiosity.mean[, c("SFAcc", "SAcc") := list( (SFHit - SFFalse), (SHit - SFalse))]

  # Calculate the hit rates for each pre interesting rating
  outside.hit.rate.preInt <- object.recognition[PreInt != 0, .(SFHit = mean(SFHit, na.rm = TRUE), SHit = mean(SHit, na.rm = TRUE)), by = c("SubjectNo", "PreInt")]
  outside.hit.rate.preInt <- outside.hit.rate.preInt[order(SubjectNo, PreInt), ]
  outside.hit.rate.preInt <- merge(outside.hit.rate.preInt, idv.false.alarm.rate, all = TRUE)

  outside.hit.rate.preInt[, SFAcc  := (SFHit - SFFalse)]
  outside.hit.rate.preInt[, SAcc   := (SHit  - SFalse)]

  # Calculate the hit rates for each pre surprise rating
  outside.hit.rate.preSur <- object.recognition[PreSur != 0, .(SFHit = mean(SFHit, na.rm = TRUE), SHit = mean(SHit, na.rm = TRUE)), by = c("SubjectNo", "PreSur")]
  outside.hit.rate.preSur <- outside.hit.rate.preSur[order(SubjectNo, PreSur), ]
  outside.hit.rate.preSur <- merge(outside.hit.rate.preSur, idv.false.alarm.rate, all = TRUE)

  outside.hit.rate.preSur[, SFAcc  := (SFHit - SFFalse)]
  outside.hit.rate.preSur[, SAcc   := (SHit  - SFalse)]

  # Calculate the hit rates for each exploration group (median-split based on exploration time)
  object.recognition[, DurGrpMd := mapply(CurGrpMedianSep, InsideDuration, MedianDur)]

  outside.hit.rate.item.exploration.median   <- object.recognition[Scene != "None", .(SFHit = mean(SFHit, na.rm = TRUE), SHit = mean(SHit, na.rm = TRUE)), by = c("SubjectNo", "DurGrpMd")]
  outside.hit.rate.item.exploration.median   <- merge(outside.hit.rate.item.exploration.median, idv.false.alarm.rate, all = TRUE)
  outside.hit.rate.item.exploration.median[, c("SFAcc", "SAcc") := list( (SFHit - SFFalse), (SHit - SFalse))]

  ## Calculate the hit rates for each exploration group (median-splited) and order group
  outside.hit.rate.item.order.exploration.median   <- object.recognition[Scene != "None", .(SFHit = mean(SFHit, na.rm = TRUE), SHit = mean(SHit, na.rm = TRUE)), by = c("SubjectNo", "DurGrpMd", "ObjOrdGrp")]
  outside.hit.rate.item.order.exploration.median   <- merge(outside.hit.rate.item.order.exploration.median, idv.false.alarm.rate, all = TRUE)
  outside.hit.rate.item.order.exploration.median[, c("SFAcc", "SAcc") := list( (SFHit - SFFalse), (SHit - SFalse))]


  save(rooms, ratings, individual.data, object.recognition, 
    outside.hit.rate.per.room, outside.hit.rate.per.rating, 
    outside.hit.rate.item.curiosity, outside.hit.rate.item.curiosity.median, 
    outside.hit.rate.item.order.curiosity, outside.hit.rate.item.order.curiosity.median, 
    outside.hit.rate.preInt, outside.hit.rate.preSur, 
    outside.hit.rate.item.exploration.median, outside.hit.rate.item.order.exploration.median, 
    file = "./PilotData/IndividualData.RData")

}









