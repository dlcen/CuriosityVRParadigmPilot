library(data.table); library(ggplot2); library(Hmisc)

## The familiar and novel groups
grouping <- data.frame(Familiar = c("Bedroom", "Classroom", "Gym"), Novel = c("Library", "LivingRoom", "StorageRoom"))

GroupA <- c(1:12, 72:73, 76:77, 80:81, 84:85)
GroupB <- c(25, 61:71, 74:75, 78:79, 82:83, 86:87)

## Get the list of participant's folders
participant.folders <- dir(pattern = "^P")

## Get the rating in each folder (also need to determine whether the room is familiar or novel)
Curiosity.Ratings <- NULL

for (thisFolder in participant.folders) {
  this.rating <- read.csv(paste0(thisFolder, .Platform$file.sep, "CuriosityRatings.csv"), header = T, stringsAsFactors=F, fileEncoding= "UTF-8-BOM")
  this.rating <- data.table(this.rating)
  
  this.rating$SubjectNo <- thisFolder
  this.rating$Group <- "Familiar"
  
  this.subjectNo <- as.numeric(strsplit(thisFolder, 'P')[[1]][2])
  
  if (this.subjectNo %in% GroupA) {
    this.rating[Room %in% grouping$Novel]$Group <- "Novel"
  } else {
    this.rating[Room %in% grouping$Familiar]$Group <- "Novel"
  }
  
  Curiosity.Ratings <- rbind(Curiosity.Ratings, this.rating)
}


## Visualisation of the overall ratings
Curiosity.Ratings <- data.table(Curiosity.Ratings)

Curiosity.Ratings.GrpSum <- Curiosity.Ratings[, .(GroupRating = mean(CuriosityRating, na.rm = T)), by = c("SubjectNo", "Group")]

Curiosity.Ratings.GrpSum[, .(Mean = mean(GroupRating, na.rm = T), SD = sd(GroupRating, na.rm = T)), by = c("Group")]

ggplot(Curiosity.Ratings.GrpSum, aes(x = Group, y = GroupRating)) + theme_minimal() +
  stat_summary(fun.y = 'mean', geom = "bar", fill = "White", color = "black") +
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.2 ) +
  labs(y = "Curiosity Rating")

## Simple statistics

t.test(GroupRating ~ Group, Curiosity.Ratings.GrpSum, paired = T)

## Draw distribution of curiosity ratings for each individual participant
ggplot(Curiosity.Ratings, aes(SubjectNo, CuriosityRating)) + theme_set(theme_minimal()) +
  geom_count(col = "tomato3", show.legend = F) +
  labs(x = "Participant No.", y = "Curiosity Rating")

ggplot(Curiosity.Ratings, aes(SubjectNo, CuriosityRating)) + theme_minimal() +
  geom_boxplot() + 
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1, fill = 'red') +
  coord_cartesian(ylim = c(0, 7)) +
  labs(x = "Participant No.", y = "Curiosity Rating")

## Get the order of the room choice
even_indexes <- seq(2, nrow(Curiosity.Ratings), 2)
Curiosity.Ratings$RoomOrder <- "First"
Curiosity.Ratings[even_indexes]$RoomOrder <- "Second"

## Calculate the correlation between room order and novelty
Curiosity.Ratings$RoomOrderNumeric <- 1
Curiosity.Ratings[RoomOrder == "Second"]$RoomOrderNumeric <- 2

Curiosity.Ratings$NoveltyNumeric <- 0
Curiosity.Ratings[Group == "Familiar"]$NoveltyNumeric <- 1

baseline.cor <- lme(RoomOrderNumeric ~ 1, random = ~1|SubjectNo, data = Curiosity.Ratings, method = "ML",control = list(msMaxIter = 500, opt = "optim", msVerbose=TRUE))
novelty.order.cor <- update(baseline.cor, .~. + NoveltyNumeric)
novelty.order.cor.random <- update(novelty.order.cor, random = ~ 1 + NoveltyNumeric | SubjectNo)

## Normalising curiosity ratings
Curiosity.Ratings$NRating <- 0

for (this.p in participant.folders) {
  this.p.ratings <- Curiosity.Ratings[SubjectNo == this.p]
  this.mean <- mean(this.p.ratings$CuriosityRating )
  this.std <- sd(this.p.ratings$CuriosityRating)
  this.p.ratings$NRating <- (this.p.ratings$CuriosityRating - this.mean)/this.std
  Curiosity.Ratings[SubjectNo == this.p]$NRating <- this.p.ratings$NRating
}

Curiosity.Ratings$RatingGroup <- 1 # "Low"
Curiosity.Ratings[CuriosityRating >= 4]$RatingGroup <- 2 #"High"

Curiosity.Ratings$RatingGroupMedian <- 0

for (this.p in participant.folders) {
  this.p.ratings <- Curiosity.Ratings[SubjectNo == this.p]
  this.median <- median(this.p.ratings$CuriosityRating )
  Curiosity.Ratings[SubjectNo == this.p & CuriosityRating < this.median]$RatingGroupMedian <- 1 #"Low"
  Curiosity.Ratings[SubjectNo == this.p & CuriosityRating > this.median]$RatingGroupMedian <- 2 #"High"
}

Curiosity.Ratings$RatingGroupMean <- 0

for (this.p in participant.folders) {
  this.p.ratings <- Curiosity.Ratings[SubjectNo == this.p]
  this.mean <- mean(this.p.ratings$CuriosityRating )
  Curiosity.Ratings[SubjectNo == this.p & CuriosityRating < this.mean]$RatingGroupMean <- 1 #"Low"
  Curiosity.Ratings[SubjectNo == this.p & CuriosityRating > this.mean]$RatingGroupMean <- 2 #"High"
}

## Check the results
ggplot(Curiosity.Ratings, aes(SubjectNo, NRating)) + theme_minimal() +
  geom_boxplot() + 
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1, fill = 'red') +
  coord_cartesian(ylim = c(0, 7)) +
  labs(x = "Participant No.", y = "Normalised Curiosity Rating")

## Check the relationships between mean curiosity rating and EC and PC score
mean.curiosity.rating <- Curiosity.Ratings[, .(MeanRating = mean(CuriosityRating)), by = c("SubjectNo")]

mean.curiosity.rating$PC <- questionnaire$PC
mean.curiosity.rating$EC <- questionnaire$EC

ggplot(mean.curiosity.rating, aes(x = PC, y = MeanRating)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  coord_cartesian(xlim = c(1:4), ylim = c(1:7)) +
  scale_y_continuous(breaks = c(1:7)) +
  labs(x = "PC score", y = "Curiosity Rating")

ggplot(mean.curiosity.rating, aes(x = EC, y = MeanRating)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  coord_cartesian(xlim = c(1:4), ylim = c(1:7)) +
  scale_y_continuous(breaks = c(1:7)) +
  labs(x = "EC score", y = "Curiosity Rating")

## Calculating the correlation
cor.test(mean.curiosity.rating$MeanRating, mean.curiosity.rating$PC, method = "pearson")
cor.test(mean.curiosity.rating$MeanRating, mean.curiosity.rating$EC, method = "pearson")

## Add curiosity ratings to the response files
Encoding.Recall.Old$CuriosityRating <- 0
Curiosity.Recall.Old$CuriosityRating <- 0

Encoding.Recall.Old$NCuriosityRating <- 0
Curiosity.Recall.Old$NCuriosityRating <- 0

Encoding.Recall.Old$CuriosityRatingType <- 0
Curiosity.Recall.Old$CuriosityRatingType <- 0

Encoding.Recall.Old$RatingGroup <- 0
Curiosity.Recall.Old$RatingGroup <- 0

Encoding.Recall.Old$RatingGroupMedian <- 0
Curiosity.Recall.Old$RatingGroupMedian <- 0

for (this.p in participant.folders) {
  for (this.room in Rooms) {
    this.rating <- Curiosity.Ratings[SubjectNo == this.p & Room == this.room]$CuriosityRating
    this.n.rating <- Curiosity.Ratings[SubjectNo == this.p & Room == this.room]$NRating
    
    this.curiosity.type        <- Curiosity.Ratings[SubjectNo == this.p & Room == this.room]$RatingGroup
    this.curiosity.type.mean   <- Curiosity.Ratings[SubjectNo == this.p & Room == this.room]$RatingGroupMean
    this.curiosity.type.median <- Curiosity.Ratings[SubjectNo == this.p & Room == this.room]$RatingGroupMedian
    
    Encoding.Recall.Old[SubjectNo == this.p & Context == this.room]$CuriosityRating <- this.rating
    Curiosity.Recall.Old[SubjectNo == this.p & Context == this.room]$CuriosityRating <- this.rating
    
    Encoding.Recall.Old[SubjectNo == this.p & Context == this.room]$NCuriosityRating <- this.n.rating
    Curiosity.Recall.Old[SubjectNo == this.p & Context == this.room]$NCuriosityRating <- this.n.rating
    
    Encoding.Recall.Old[SubjectNo == this.p & Context == this.room]$CuriosityRatingType <- this.curiosity.type
    Curiosity.Recall.Old[SubjectNo == this.p & Context == this.room]$CuriosityRatingType <- this.curiosity.type
    
    Encoding.Recall.Old[SubjectNo == this.p & Context == this.room]$RatingGroup <- this.curiosity.type.mean
    Curiosity.Recall.Old[SubjectNo == this.p & Context == this.room]$RatingGroup <- this.curiosity.type.mean
    
    Encoding.Recall.Old[SubjectNo == this.p & Context == this.room]$RatingGroupMedian <- this.curiosity.type.median
    Curiosity.Recall.Old[SubjectNo == this.p & Context == this.room]$RatingGroupMedian <- this.curiosity.type.median
  }
}
