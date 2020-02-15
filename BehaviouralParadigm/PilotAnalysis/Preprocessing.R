library(data.table)

# Ratings
ratings <- NULL

for (this.p in participant.list) {
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

for (this.p in participant.list) {
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

source("./PilotAnalysis/RoomOrder.R")

individual.data <- merge(individual.data, room.order, by = c("SubjectNo", "Room"))

individual.data$PreCur <- 0
individual.data$PreInt <- 0
individual.data$PreSur <- 100

for (this.p in participant.list) {
  for (this.room in rooms) {
    this.pre.room <- individual.data[SubjectNo == this.p & Room == this.room]$PreRoom
    if (!is.na(this.pre.room)) {
      individual.data[SubjectNo == this.p & Room == this.room]$PreCur <- individual.data[SubjectNo == this.p & Room == this.pre.room]$Curiosity
      individual.data[SubjectNo == this.p & Room == this.room]$PreInt <- individual.data[SubjectNo == this.p & Room == this.pre.room]$Interest
      individual.data[SubjectNo == this.p & Room == this.room]$PreSur <- individual.data[SubjectNo == this.p & Room == this.pre.room]$Surprise
    }
  }
}

individual.data[PreCur == 0]$PreCur <- NA
individual.data[PreInt == 0]$PreInt <- NA
individual.data[PreSur == 100]$PreSur <- NA

names(individual.data)[2] <- "Scene"

# Memory performance

object.recognition <- NULL

if (!is.day1.only) {
  for (thisFolder in participant.list) {
    this.response <- read.csv(paste0("PilotData", .Platform$file.sep, "IndividualData", .Platform$file.sep, thisFolder, .Platform$file.sep, "TestOrder", .Platform$file.sep, "MemoryTestResponse.csv"), header = T)
    this.response <- data.table(this.response)
    
    this.response$SubjectNo <- thisFolder

    source("./PilotAnalysis/OutsideOrder.r")

    this.response$ItemOrder <- 0

    this.response <- this.response[order(SubjectNo, Scene, Object)]
    this.order    <- outside.orders[SubjectNo == thisFolder]
    this.order	  <- this.order[order(SubjectNo, Room, Item)]
    
    this.response[Scene != "None"]$ItemOrder <- this.order$Order
    
    this.response[ItemOrder == 0]$ItemOrder <- NA

    object.recognition <- rbind(object.recognition, this.response)

  }

  object.recognition$Group <- "OldItem"
  object.recognition[Scene == "None"]$Group <- "Distractor"

  source("./PilotAnalysis/CalFunc.R")

  object.recognition[, c("SFHit", "SHit", "SFFalse", "SFalse") := list(mapply(SFHitCal, Response, Scene), mapply(SHitCal, Response, Scene), mapply(SFFalseHitCal, Response, Scene), mapply(SFalseHitCal,  Response, Scene))]

  idv.false.alarm.rate <- object.recognition[Scene == "None", .(SFFalse = mean(SFFalse), SFalse = mean(SFalse)), by = c("SubjectNo")]
  
  ## Calculate average exploration time (median and mean)
  average.inside.duration   <- durations[, .(MeanDur = mean(InsideDuration, na.rm = TRUE), MedianDur = median(InsideDuration, na.rm = TRUE)), by = c("SubjectNo") ]
  object.recognition.old    <- merge(object.recognition, individual.data, by = c("SubjectNo", "Scene"))
  object.recognition.old    <- merge(object.recognition.old, average.inside.duration, by = "SubjectNo")

  ## Calculate average ratings (median and mean)
  average.ratings           <- individual.data[, .(MeanCur = mean(Curiosity, na.rm = TRUE), MedianCur = median(Curiosity, na.rm = TRUE), MeanInt = mean(Interest, na.rm = TRUE), MedianInt = median(Interest, na.rm = TRUE), MeanSur = mean(Surprise, na.rm = TRUE), MedianSur = median(Surprise, na.rm = TRUE), MeanPreInt = mean(PreInt, na.rm = TRUE), MedianPreInt = median(PreInt, na.rm = TRUE), MeanPreSur = mean(PreSur, na.rm = TRUE), MedianPreSur = median(PreSur, na.rm = TRUE)),  by = c("SubjectNo")]
  object.recognition.old    <- merge(object.recognition.old, average.ratings, by = "SubjectNo")

  ## Calculate the hit rates for each individual participant and each room
  outside.hit.rate.per.room <- object.recognition.old[, .(SFHit = mean(SFHit, na.rm = TRUE), SHit = mean(SHit, na.rm = TRUE)), by = c("SubjectNo", "Scene", "Curiosity", "Interest", "Surprise", "PreInt", "PreSur", "InsideDuration", "OutsideDuration", "MedianDur", "MedianCur", "MedianInt", "MedianSur", "MedianPreInt", "MedianPreSur")]
  outside.hit.rate.per.room <- outside.hit.rate.per.room[order(SubjectNo, Scene), ]
  outside.hit.rate.per.room <- merge(outside.hit.rate.per.room, idv.false.alarm.rate, all = TRUE)

  outside.hit.rate.per.room[, c("SFAcc", "SAcc")  := list((SFHit - SFFalse), (SHit - SFalse)) ]

  ## Calculate the hit rates for each curiosity group
  object.recognition.old[, CurGrp := mapply(GrpSep, Curiosity)]
  outside.hit.rate.item.curiosity <- CorrHitRateCal(object.recognition.old, "CurGrp", idv.false.alarm.rate)

  ## Calculate the hit rates for each curiosity group and order group
  object.recognition.old[, ObjOrdGrp := mapply(ObjOrdGrpSep, ItemOrder) ]
  outside.hit.rate.item.order.curiosity        <- CorrHitRateOrderCal(object.recognition.old, "CurGrp", idv.false.alarm.rate)

  ## Calculate the hit rates for each curiosity group that is median split
  object.recognition.old[, CurGrpMd := mapply(GrpMedianSep, Curiosity, MedianCur)]
  outside.hit.rate.item.curiosity.median   <- CorrHitRateCal(object.recognition.old, "CurGrpMd", idv.false.alarm.rate)

  ## Calculate the hit rates for each curiosity group (median-splited) and order group
  outside.hit.rate.item.order.curiosity.median   <- CorrHitRateOrderCal(object.recognition.old, "CurGrpMd", idv.false.alarm.rate)

  # Calculate the hit rates for each exploration group (median-split based on exploration time)
  object.recognition.old[, DurGrpMd := mapply(GrpMedianSep, InsideDuration, MedianDur)]
  outside.hit.rate.item.exploration.median   <- CorrHitRateCal(object.recognition.old, "DurGrpMd", idv.false.alarm.rate)

  ## Calculate the hit rates for each exploration group (median-splited) and order group
  outside.hit.rate.item.order.exploration.median   <- CorrHitRateOrderCal(object.recognition.old, "DurGrpMd", idv.false.alarm.rate)

  # Calculate the hit rates for each interestingness group (median-split based on the rating)
  object.recognition.old[, IntGrpMd := mapply(GrpMedianSep, Interest, MedianInt)]
  outside.hit.rate.item.interest.median <- CorrHitRateCal(object.recognition.old, "IntGrpMd", idv.false.alarm.rate)

  ## Calculate the hit rates for each interestingness group (median-splited) and order group
  outside.hit.rate.item.order.interest.median   <- CorrHitRateOrderCal(object.recognition.old, "IntGrpMd", idv.false.alarm.rate)

  # Calculate the hit rates for each surprise group (median-split based on the rating)
  object.recognition.old[, SurGrpMd := mapply(GrpMedianSep, Surprise, MedianSur)]
  outside.hit.rate.item.surprise.median <- CorrHitRateCal(object.recognition.old, "SurGrpMd", idv.false.alarm.rate)

  ## Calculate the hit rates for each surpriseingness group (median-splited) and order group
  outside.hit.rate.item.order.surprise.median   <- CorrHitRateOrderCal(object.recognition.old, "SurGrpMd", idv.false.alarm.rate)

  ## Calculate the hit rates for each pre-interest group (median-splited based on the rating)
  object.recognition.old[, PreIntGrpMd := mapply(GrpMedianSep, PreInt, MedianPreInt)]
  outside.hit.rate.item.preint.median  <- CorrHitRateCal(object.recognition.old, "PreIntGrpMd", idv.false.alarm.rate)
  outside.hit.rate.item.order.preint.median <- CorrHitRateOrderCal(object.recognition.old, "PreIntGrpMd", idv.false.alarm.rate)

  ## Calculate the hit rates for each pre-surprise group (median-splited based on the rating)
  object.recognition.old[, PreSurGrpMd := mapply(GrpMedianSep, PreSur, MedianPreSur)]
  outside.hit.rate.item.presur.median  <- CorrHitRateCal(object.recognition.old, "PreSurGrpMd", idv.false.alarm.rate)
  outside.hit.rate.item.order.presur.median <- CorrHitRateOrderCal(object.recognition.old, "PreSurGrpMd", idv.false.alarm.rate)


  save(rooms, ratings, individual.data, object.recognition, object.recognition.old,
    outside.hit.rate.per.room, 
    outside.hit.rate.item.curiosity, outside.hit.rate.item.curiosity.median, 
    outside.hit.rate.item.order.curiosity, outside.hit.rate.item.order.curiosity.median, 
    outside.hit.rate.item.exploration.median, outside.hit.rate.item.order.exploration.median, 
    outside.hit.rate.item.interest.median, outside.hit.rate.item.order.interest.median,
    outside.hit.rate.item.surprise.median, outside.hit.rate.item.order.surprise.median,
    outside.hit.rate.item.preint.median, outside.hit.rate.item.order.preint.median,
    outside.hit.rate.item.presur.median, outside.hit.rate.item.order.presur.median,
    file = "./PilotData/IndividualData.RData")
}











