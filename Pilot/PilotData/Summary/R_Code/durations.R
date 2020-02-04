library(data.table); library(ggplot2)

parent.folder <- "D:/OneDirve_CU/OneDrive - Cardiff University/Projects/VRCuriosityMemory/PilotData/RawData/"

## The familiar and novel groups
grouping <- data.frame(Familiar = c("Bedroom", "Classroom", "Gym"), Novel = c("Library", "LivingRoom", "StorageRoom"))
Rooms 	 <- c("Bedroom", "Classroom", "Gym", "Library", "LivingRoom", "StorageRoom")

GroupA <- c(1:20, 30, 33:34, 37:38, 41:42, 45:46, 49:50, 53:54, 57:58, 72:73, 76:77, 80:81, 84:85)
GroupB <- c(21:29, 31:32, 35:36, 39:40, 43:44, 47:48, 51:52, 55:56, 61:71, 74:75, 78:79, 82:83, 86:87)

Durations <- NULL

# Pilot 1
setwd(paste0(parent.folder, .Platform$file.sep, "Pilot_01"))
participant.folders <- dir(pattern = "^P")

## Check the data
# for (thisFolder in participant.folders) {
# 	setwd(thisFolder)
# 	for (this.r in Rooms) {
# 		this.trj.file <- dir(pattern = paste0(this.r, "_OutsideTrajectory_"))
# 		if (length(this.trj.file) > 1) {
# 			print(thisFolder)
# 			print(this.r)
# 		}
# 	}
# 	setwd(paste0(parent.folder, .Platform$file.sep, "Pilot_01"))
# }

## Collect the data 
for (thisFolder in participant.folders) {

	if (file.exists(paste0(thisFolder, .Platform$file.sep, "Durations.csv"))) {
		this.duration <- read.csv(paste0(thisFolder, .Platform$file.sep, "Durations.csv"), header = T, stringsAsFactors = F, fileEncoding = "UTF-8-BOM")
		this.duration <- data.table(this.duration)
	
  
	  	this.duration$SubjectNo 	<- thisFolder
	  	this.duration$Group 		<- "Familiar"
	  	this.duration$Version 		<- "Immediate"
	  	this.duration$RoomOrder     <- c(1:nrow(this.duration))
	  
	  	this.subjectNo <- as.numeric(strsplit(thisFolder, 'P')[[1]][2])
	  
		if (this.subjectNo %in% GroupA) {
			this.duration[Room %in% grouping$Novel]$Group <- "Novel"
		} else {
			this.duration[Room %in% grouping$Familiar]$Group <- "Novel"
		}
		  
	  	Durations <- rbind(Durations, this.duration)
	 }
}

# Pilot 2
setwd(paste0(parent.folder, .Platform$file.sep, "Pilot_02"))
participant.folders <- dir(pattern = "^P")

## Check the data
# for (thisFolder in participant.folders) {
# 	setwd(thisFolder)
# 	for (this.r in Rooms) {
# 		this.trj.file <- dir(pattern = paste0(this.r, "_OutsideTrajectory_"))
# 		if (length(this.trj.file) > 1) {
# 			print(thisFolder)
# 			print(this.r)
# 		}
# 	}
# 	setwd(paste0(parent.folder, .Platform$file.sep, "Pilot_02"))
# }

## Collect the data
for (thisFolder in participant.folders) {

	if (file.exists(paste0(thisFolder, .Platform$file.sep, "Durations.csv"))) {
		this.duration <- read.csv(paste0(thisFolder, .Platform$file.sep, "Durations.csv"), header = T, stringsAsFactors = F, fileEncoding = "UTF-8-BOM")
		this.duration <- data.table(this.duration)
  
	  	this.duration$SubjectNo 	<- thisFolder
	  	this.duration$Group 		<- "Familiar"
	  	this.duration$Version 		<- "Delayed"
	  	this.duration$RoomOrder     <- c(1:nrow(this.duration))
	  
	  	this.subjectNo <- as.numeric(strsplit(thisFolder, 'P')[[1]][2])
	  
		if (this.subjectNo %in% GroupA) {
			this.duration[Room %in% grouping$Novel]$Group <- "Novel"
		} else {
			this.duration[Room %in% grouping$Familiar]$Group <- "Novel"
		}
		  
	  	Durations <- rbind(Durations, this.duration)
	}
}

length(as.character(unique(Durations$SubjectNo)))
Durations <- Durations[SubjectNo != "P54"]

# Plot the comparison between *familiar* and *novel* rooms
ggplot(Durations[, .(InsideDuration = mean(InsideDuration, na.rm = T)), by = c("SubjectNo", "Group")], aes(x = Group, y = InsideDuration)) +
	stat_summary(fun.y = mean, geom = "bar" ) +
	stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
	labs(x = "Room novelty", y = "Time in the room (s)")
	
t.test(InsideDuration ~ Group, data = Durations[, .(InsideDuration = mean(InsideDuration, na.rm = T)), by = c("SubjectNo", "Group")], paired = T)

# Add the curiosity rating data
Durations$CuriosityRating   <- 0
Durations$NRating 			<- 0
Durations$RatingGroup 		<- 0
Durations$RatingGroupMean   <- 0

load("D:/OneDirve_CU/OneDrive - Cardiff University/Projects/VRCuriosityMemory/PilotData/RawData/Pilot_01/.RData")

for (this.p in participant.folders) {
	for (this.r in Rooms) {
		Durations[SubjectNo == this.p & Room == this.r]$CuriosityRating <- Curiosity.Ratings[SubjectNo == this.p & Room == this.r]$CuriosityRating
		Durations[SubjectNo == this.p & Room == this.r]$NRating			<- Curiosity.Ratings[SubjectNo == this.p & Room == this.r]$NRating
		Durations[SubjectNo == this.p & Room == this.r]$RatingGroup 	<- Curiosity.Ratings[SubjectNo == this.p & Room == this.r]$RatingGroup
		Durations[SubjectNo == this.p & Room == this.r]$RatingGroupMean <- Curiosity.Ratings[SubjectNo == this.p & Room == this.r]$RatingGroupMean
	}
}

load("D:/OneDirve_CU/OneDrive - Cardiff University/Projects/VRCuriosityMemory/PilotData/RawData/Pilot_02/.RData")

for (this.p in participant.folders) {
	for (this.r in Rooms) {
		Durations[SubjectNo == this.p & Room == this.r]$CuriosityRating <- Curiosity.Ratings[SubjectNo == this.p & Room == this.r]$CuriosityRating
		Durations[SubjectNo == this.p & Room == this.r]$NRating			<- Curiosity.Ratings[SubjectNo == this.p & Room == this.r]$NRating
		Durations[SubjectNo == this.p & Room == this.r]$RatingGroup 	<- Curiosity.Ratings[SubjectNo == this.p & Room == this.r]$RatingGroup
		Durations[SubjectNo == this.p & Room == this.r]$RatingGroupMean <- Curiosity.Ratings[SubjectNo == this.p & Room == this.r]$RatingGroupMean
	}
}

# Plot the relationship between duration and curiosity ratings
ggplot(Durations, aes(x = CuriosityRating, y = InsideDuration)) +
	stat_summary(fun.data = mean_cl_normal, geom = "errorbar", alpha = 0.5, width = 0) +
	stat_summary(fun.y = mean, geom = "point") +
	stat_smooth(method = "lm", formula = y ~ x, se = FALSE) +
	scale_x_continuous(breaks = c(1:7)) +
	labs(x = "Curiosity Rating", y = "Time in the room (s)")

# Test the relationship
library(nlme)

baseline  <- lme(InsideDuration ~ 1, random = ~ 1|SubjectNo/Group, data = Durations, method = "ML")

noveltyM  <- update(baseline,        .~. + Group)
rtM 	  <- update(baseline,        .~. + CuriosityRating)

itrM      <- update(rtM,             .~. + Group:CuriosityRating)

anova(baseline, noveltyM, rtM, itrM)

## Collapsed the data
Durations_excluded <- Durations[RatingGroupMean != 0]
Durations_excluded$RatingGroupMean <- factor(Durations_excluded$RatingGroupMean, levels = c(1: 2), labels = c("Low", "High"))

ggplot(Durations_excluded[, .(InsideDuration = mean(InsideDuration, na.rm = T)), by = c("SubjectNo", "RatingGroupMean")], aes(x = RatingGroupMean, y = InsideDuration)) +
	stat_summary(fun.y = mean, geom = "bar" ) +
	stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
	labs(x = "Curiosity rating group (lower or higher than mean)", y = "Time in the room (s)")

t.test(InsideDuration ~ RatingGroupMean, data = Durations_excluded[, .(InsideDuration = mean(InsideDuration, na.rm = T)), by = c("SubjectNo", "RatingGroupMean")], paired = T)

baseline  <- lme(InsideDuration ~ 1, random = ~ 1|SubjectNo/Group/RatingGroupMean, data = Durations_excluded, method = "ML")

noveltyM  <- update(baseline,        .~. + Group)
rtM 	  <- update(noveltyM,        .~. + RatingGroupMean)
itrM      <- update(rtM,             .~. + Group:RatingGroupMean)

anova(baseline, noveltyM, rtM, itrM)

# Add questionnaire scores
questionnaire_01 <- read.table(paste0(parent.folder, .Platform$file.set, "Pilot_01/Questionnaire.csv"), header = T, fileEncoding="UTF-8-BOM")
questionnaire_02 <- read.table(paste0(parent.folder, .Platform$file.set, "Pilot_02/Questionnaire.csv"), header = T, fileEncoding="UTF-8-BOM")

questionnaire    <- rbind(questionnaire_01, questionnaire_02)

questionnaire$PC <- rowMeans(questionnaire[, c(2:13)])
questionnaire$EC <- rowMeans(questionnaire[, c(14:23)])
questionnaire$GN <- rowMeans(questionnaire[, c("PC", "EC")])

questionnaire    <- data.table(questionnaire)

participant.list <- as.character(unique(Durations$SubjectNo))

Durations$PC <- 0
Durations$EC <- 0
Durations$GN <- 0

for (this.p in participant.list) {
	Durations[SubjectNo == this.p]$PC <- questionnaire[SubjectNo == this.p]$PC
	Durations[SubjectNo == this.p]$EC <- questionnaire[SubjectNo == this.p]$EC
	Durations[SubjectNo == this.p]$GN <- questionnaire[SubjectNo == this.p]$GN
}

# Check the relationship between PC and time spent in novel rooms
ggplot(Durations[Group == "Novel", .(InsideDuration = mean(InsideDuration, na.rm = T)), by = c("SubjectNo", "PC")], aes(x = PC, y = InsideDuration)) +
	stat_summary(fun.y = mean, geom = "point") +
	stat_smooth(method = "lm", formula = y ~ x, se = FALSE) +
	labs(x = "PC score", y = "Time in the room (s)")

ggplot(Durations[Group == "Novel", .(InsideDuration = mean(InsideDuration, na.rm = T)), by = c("SubjectNo", "EC")], aes(x = EC, y = InsideDuration)) +
	stat_summary(fun.y = mean, geom = "point") +
	stat_smooth(method = "lm", formula = y ~ x, se = FALSE) +
	labs(x = "EC score", y = "Time in the room (s)")

ggplot(Durations[Group == "Novel", .(InsideDuration = mean(InsideDuration, na.rm = T)), by = c("SubjectNo", "GN")], aes(x = GN, y = InsideDuration)) +
	stat_summary(fun.y = mean, geom = "point") +
	stat_smooth(method = "lm", formula = y ~ x, se = FALSE) +
	labs(x = "GN score", y = "Time in the room (s)")

## Test
baseline 	<- lme(InsideDuration ~ 1, random = ~1|SubjectNo, data = Durations[Group == "Novel", .(InsideDuration = mean(InsideDuration, na.rm = T)), by = c("SubjectNo", "PC")], method = "ML")
pcM 		<- update(baseline,        .~.  + PC)
anova(baseline, pcM)

# Check the relationship between traits and difference betwen Familiar and Novel rooms.
familiarData <- Durations[Group == "Familiar", .(InsideDuration = mean(InsideDuration, na.rm = T)), by = c("SubjectNo", "PC", "EC", "GN")]
novelData    <- Durations[Group == "Novel",    .(InsideDuration = mean(InsideDuration, na.rm = T)), by = c("SubjectNo", "PC", "EC", "GN")]

novelData$dTime <- novelData$InsideDuration - familiarData$InsideDuration

ggplot(novelData, aes(x = PC, y = dTime)) +
	stat_summary(fun.y = mean, geom = "point") +
	stat_smooth(method = "lm", formula = y ~ x, se = FALSE) +
	labs(x = "PC score", y = "Difference in duration (s)")

baseline 	<- lme(dTime ~ 1, random = ~1|SubjectNo, data = novelData, method = "ML")
pcM 		<- update(baseline,        .~.  + PC)
anova(baseline, pcM)

ggplot(novelData, aes(x = EC, y = dTime)) +
	stat_summary(fun.y = mean, geom = "point") +
	stat_smooth(method = "lm", formula = y ~ x, se = FALSE) +
	labs(x = "EC score", y = "Difference in duration (s)")

baseline 	<- lme(dTime ~ 1, random = ~1|SubjectNo, data = novelData, method = "ML")
ecM 		<- update(baseline,        .~.  + EC)
anova(baseline, ecM)

# Test the relationship between traits and curiosity rating effects
baseline  <- lme(InsideDuration ~ 1, random = ~ 1|SubjectNo, data = Durations, method = "ML")

nvM       <- update(baseline,        .~. + Group)
rtM 	  <- update(nvM,             .~. + CuriosityRating)
trM       <- update(rtM,             .~. + PC)
itrM      <- update(trM,             .~. + CuriosityRating : PC)

anova(baseline, nvM, rtM, trM, itrM)

baseline  <- lme(InsideDuration ~ 1, random = ~ 1|SubjectNo, data = Durations, method = "ML")

nvM       <- update(baseline,        .~. + Group)
rtM 	  <- update(nvM,             .~. + CuriosityRating)
trM       <- update(rtM,             .~. + EC)
itrM      <- update(trM,             .~. + CuriosityRating : EC)

anova(baseline, nvM, rtM, trM, itrM)