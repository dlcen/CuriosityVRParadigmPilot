library(data.table); library(ggplot2)

# Get the participant no.s

participant.list <- list.files(path = "./PilotData/IndividualData/", pattern = "^P")

# First look at the curiosity rating and interesting rating for each participants

## Get the curiosity ratings for each participant
ratings <- NULL

for (this.p in participant.list) {
	this.ratings <- read.csv(paste0("./PilotData/IndividualData/", this.p, "/Exploration/Ratings.csv"), header = TRUE)
	this.ratings$SubjectNo <- this.p
	ratings  	 <- rbind(ratings, this.ratings)
}

ratings <- data.table(ratings)

rooms <- as.character(unique(ratings$Room))

## Plot a histogram for each participant, respectively for curiosity ratings and interesting ratings

### Curiosity ratings
ggplot(ratings, aes(x = Curiosity)) + 
	geom_histogram(binwidth = 0.5) +
	geom_vline(aes(xintercept = median(Curiosity)), color = "red", size = 1) +
	scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6))+
	scale_y_continuous(breaks = c(0:10)) +
	labs(x = "Curiosity Rating", y = "Count") + 
	facet_wrap( ~ SubjectNo) +
	theme(axis.text = element_text(size = 12),
		  strip.text = element_text(size = 12, face = "bold"))
ggsave("./Figures/CuriosityRatings.png", width = 12, height = 4)


### Interesting ratings
ggplot(ratings, aes(x = Interest)) + 
	geom_histogram(binwidth = 0.5) +
	geom_vline(aes(xintercept = median(Interest)), color = "red", size = 1) +
	scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6))+
	scale_y_continuous(breaks = c(0:10)) +
	labs(x = "Interest Rating", y = "Count") + 
	facet_wrap( ~ SubjectNo) +
	theme(axis.text = element_text(size = 12),
		  strip.text = element_text(size = 12, face = "bold"))
ggsave("./Figures/InterestRatings.png", width = 12, height = 4)

# Second, look at the time spent in each room
## Get the duration data
durations <- NULL

for (this.p in participant.list) {
	this.durs <- read.csv(paste0("./PilotData/IndividualData/", this.p, "/Exploration/TaskDurations.csv"), header = TRUE)
	this.durs$SubjectNo <- this.p
	durations  	 <- rbind(durations, this.durs)
}

individual.data <- ratings
individual.data$InsideDuration <- durations$InsideDuration

individual.data <- data.table(individual.data)

individual.data[, Surprise := (Interest - Curiosity)]

## Plot the data
### Curiosity
ggplot(individual.data, aes(x = Curiosity, y = InsideDuration)) + 
	geom_point(size = 3) +
	stat_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Curiosity rating", y = "Time spent inside the room (s)") +
	facet_wrap( ~ SubjectNo) +
  theme(axis.title = element_text(size = 12),
        strip.text = element_text(size = 12, face = "bold"))
  

ggsave("./Figures/CuriosityInsideDurations.png", width = 12, height = 4)

### Interesting
ggplot(individual.data, aes(x = Interest, y = InsideDuration)) + 
  geom_point(size = 3) +
  stat_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Interest rating", y = "Time spent inside the room (s)") +
  facet_wrap( ~ SubjectNo) +
  theme(axis.title = element_text(size = 12),
        strip.text = element_text(size = 12, face = "bold"))

ggsave("./Figures/InterestInsideDurations.png", width = 12, height = 4)

### Surprise (difference between Curiosity and Interest)
ggplot(individual.data, aes(x = Surprise, y = InsideDuration)) + 
  geom_point(size = 3) +
  stat_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Surprise rating", y = "Time spent inside the room (s)") +
  facet_wrap( ~ SubjectNo) +
  theme(axis.title = element_text(size = 12),
        strip.text = element_text(size = 12, face = "bold"))

ggsave("./Figures/SurpriseInsideDurations.png", width = 12, height = 4)


# Third, look at the memory performance

object.recognition <- NULL

for (thisFolder in participant.list) {
  this.response <- read.csv(paste0("PilotData", .Platform$file.sep, "IndividualData", .Platform$file.sep, thisFolder, .Platform$file.sep, "TestOrder", .Platform$file.sep, "MemoryTestResponse.csv"), header = T)
  this.response<- data.table(this.response)
  
  this.response$SubjectNo <- thisFolder

  # Read the curiosity rating 
  this.ratings <- ratings[SubjectNo == thisFolder]

  # Add the curiosity rating to the response file
  this.response$CurRating <- 0
  this.response$IntRating <- 0
  
  for (this.room in rooms) {
    this.response[Scene == this.room]$CurRating <- this.ratings[Room == this.room]$Curiosity
    this.response[Scene == this.room]$IntRating <- this.ratings[Room == this.room]$Interest
  }

  this.response[CurRating == 0]$CurRating <- NA
  this.response[IntRating == 0]$IntRating <- NA

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

source("./PilotAnalysis/CalFunc.R")

object.recognition[, SFHit   := mapply(SFHitCal,      Response, Scene)]
object.recognition[, SHit    := mapply(SHitCal,       Response, Scene)]
object.recognition[, SFFalse := mapply(SFFalseHitCal, Response, Scene)]
object.recognition[, SFalse  := mapply(SFalseHitCal,  Response, Scene)]

idv.false.alarm.rate <- object.recognition[Scene == "None", .(SFFalse = mean(SFFalse), SFalse = mean(SFalse)), by = c("SubjectNo")]

## Calculate the hit rates for each individual participant and each room
outside.hit.rate.per.room <- object.recognition[Scene != "None", .(SFHit = mean(SFHit, na.rm = TRUE), SHit = mean(SHit, na.rm = TRUE)), by = c("SubjectNo", "Scene", "CurRating", "IntRating")]
outside.hit.rate.per.room <- outside.hit.rate.per.room[order(SubjectNo, Scene), ]
outside.hit.rate.per.room <- merge(outside.hit.rate.per.room, idv.false.alarm.rate, all = TRUE)

outside.hit.rate.per.room[, SFAcc  := (SFHit - SFFalse)]
outside.hit.rate.per.room[, SAcc   := (SHit  - SFalse)]

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

## Calculate the hit rates for each curiosity group and order group
object.recognition[, ObjOrdGrp := mapply(ObjOrdGrpSep, ItemOrder) ]

outside.hit.rate.item.order.curiosity        <- object.recognition[Scene != "None", .(SFHit = mean(SFHit, na.rm = TRUE), SHit = mean(SHit, na.rm = TRUE)), by = c("SubjectNo", "CurGrp", "ObjOrdGrp")]
outside.hit.rate.item.order.curiosity        <- merge(outside.hit.rate.item.order.curiosity, idv.false.alarm.rate, all = TRUE)
outside.hit.rate.item.order.curiosity[, c("SFAcc", "SAcc") := list( (SFHit - SFFalse), (SHit - SFalse))]



