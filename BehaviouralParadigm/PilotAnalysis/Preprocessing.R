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

individual.data <- ratings
individual.data$InsideDuration <- durations$InsideDuration
individual.data$OutsideDuration <- durations$OutsideDuration

individual.data <- data.table(individual.data)

# individual.data[, Surprise := (Interest - Curiosity)]

# Memory performance
# object.recognition <- NULL

# all.participant.list <- list.files(path = "./PilotData/IndividualData/", pattern = "^P")

# for (thisFolder in all.participant.list) {
#   this.response <- read.csv(paste0("PilotData", .Platform$file.sep, "IndividualData", .Platform$file.sep, thisFolder, .Platform$file.sep, "TestOrder", .Platform$file.sep, "MemoryTestResponse.csv"), header = T)
#   this.response<- data.table(this.response)
  
#   this.response$SubjectNo <- thisFolder

#   # Read the curiosity rating 
#   this.ratings <- ratings[SubjectNo == thisFolder]

#   # Add the curiosity rating to the response file
#   this.response$CurRating <- 0
#   this.response$IntRating <- 0
  
#   for (this.room in rooms) {
#     this.response[Scene == this.room]$CurRating <- this.ratings[Room == this.room]$Curiosity
#     this.response[Scene == this.room]$IntRating <- this.ratings[Room == this.room]$Interest
#   }

#   this.response[CurRating == 0]$CurRating <- NA
#   this.response[IntRating == 0]$IntRating <- NA

#   # Add the outside duration 
#   this.durations <- individual.data[SubjectNo == thisFolder]

#   this.response$OutDur <- 0

#   for (this.room in rooms) {
#     this.response[Scene == this.room]$OutDur <- this.durations[Room == this.room]$OutsideDuration
#   }

#   this.response[OutDur == 0]$OutDur <- NA

#   # Add the order of items inside each room
#   source("./PilotAnalysis/OutsideOrder.r")

#   this.response$ItemOrder <- 0

#   this.response <- this.response[order(SubjectNo, Scene, Object)]
#   this.order    <- outside.orders[SubjectNo == thisFolder]
#   this.order	<- this.order[order(SubjectNo, Room, Item)]
  
#   this.response[Scene != "None"]$ItemOrder <- this.order$Order
  
#   this.response[ItemOrder == 0]$ItemOrder <- NA
  
#   object.recognition <- rbind(object.recognition, this.response)
# }

# object.recognition$Group <- "OldItem"
# object.recognition[is.na(ItemOrder)]$Group <- "Distractor"

# source("./PilotAnalysis/CalFunc.R")

# object.recognition[, SFHit   := mapply(SFHitCal,      Response, Scene)]
# object.recognition[, SHit    := mapply(SHitCal,       Response, Scene)]
# object.recognition[, SFFalse := mapply(SFFalseHitCal, Response, Scene)]
# object.recognition[, SFalse  := mapply(SFalseHitCal,  Response, Scene)]

# idv.false.alarm.rate <- object.recognition[Scene == "None", .(SFFalse = mean(SFFalse), SFalse = mean(SFalse)), by = c("SubjectNo")]

# ## Calculate the hit rates for each individual participant and each room
# outside.hit.rate.per.room <- object.recognition[Scene != "None", .(SFHit = mean(SFHit, na.rm = TRUE), SHit = mean(SHit, na.rm = TRUE)), by = c("SubjectNo", "Scene", "CurRating", "IntRating")]
# outside.hit.rate.per.room <- outside.hit.rate.per.room[order(SubjectNo, Scene), ]
# outside.hit.rate.per.room <- merge(outside.hit.rate.per.room, idv.false.alarm.rate, all = TRUE)

# outside.hit.rate.per.room[, SFAcc  := (SFHit - SFFalse)]
# outside.hit.rate.per.room[, SAcc   := (SHit  - SFalse)]

# ## Calculate the hit rates for each individual participant and each curiosity rating
# outside.hit.rate.per.rating <- object.recognition[Scene != "None", .(SFHit = mean(SFHit, na.rm = TRUE), SHit = mean(SHit, na.rm = TRUE)), by = c("SubjectNo", "CurRating")]
# outside.hit.rate.per.rating <- outside.hit.rate.per.rating[order(SubjectNo, CurRating), ]
# outside.hit.rate.per.rating <- merge(outside.hit.rate.per.rating, idv.false.alarm.rate, all = TRUE)

# outside.hit.rate.per.rating[, SFAcc  := (SFHit - SFFalse)]
# outside.hit.rate.per.rating[, SAcc   := (SHit  - SFalse)]

# ## Calculate the hit rates for each curiosity group
# object.recognition[, CurGrp := mapply(CurGrpSep, CurRating)]

# outside.hit.rate.item.curiosity        <- object.recognition[Scene != "None", .(SFHit = mean(SFHit, na.rm = TRUE), SHit = mean(SHit, na.rm = TRUE)), by = c("SubjectNo", "CurGrp")]
# outside.hit.rate.item.curiosity        <- merge(outside.hit.rate.item.curiosity, idv.false.alarm.rate, all = TRUE)
# outside.hit.rate.item.curiosity[, c("SFAcc", "SAcc") := list( (SFHit - SFFalse), (SHit - SFalse))]

# ## Calculate the hit rates for each curiosity group and order group
# object.recognition[, ObjOrdGrp := mapply(ObjOrdGrpSep, ItemOrder) ]

# outside.hit.rate.item.order.curiosity        <- object.recognition[Scene != "None", .(SFHit = mean(SFHit, na.rm = TRUE), SHit = mean(SHit, na.rm = TRUE)), by = c("SubjectNo", "CurGrp", "ObjOrdGrp")]
# outside.hit.rate.item.order.curiosity        <- merge(outside.hit.rate.item.order.curiosity, idv.false.alarm.rate, all = TRUE)
# outside.hit.rate.item.order.curiosity[, c("SFAcc", "SAcc") := list( (SFHit - SFFalse), (SHit - SFalse))]

# save(rooms, individual.data, object.recognition, outside.hit.rate.per.room, outside.hit.rate.per.rating, outside.hit.rate.item.curiosity, outside.hit.rate.item.order.curiosity, file = "./PilotData/IndividualData.RData")
