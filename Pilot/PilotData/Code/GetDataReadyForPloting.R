library(data.table); library(reshape)

# For ploting individual figures

load("Pilot_01_Immediate/GroupData/InsideObjectsResponsePerRoom.RData")
load("Pilot_01_Immediate/GroupData/OutsideObjectsResponsePerRoom.RData")

inside.hit.rate.per.room$Condition  <- "Immediate"
outside.hit.rate.per.room$Condition <- "Immediate"

pilot.1.inside.hit.rate.per.room 	<- inside.hit.rate.per.room
pilot.1.outside.hit.rate.per.room 	<- outside.hit.rate.per.room

load("Pilot_02_Delayed/GroupData/InsideObjectsResponsePerRoom.RData")
load("Pilot_02_Delayed/GroupData/OutsideObjectsResponsePerRoom.RData")

inside.hit.rate.per.room$Condition  <- "Delayed"
outside.hit.rate.per.room$Condition <- "Delayed"

pilot.2.inside.hit.rate.per.room 	<- inside.hit.rate.per.room
pilot.2.outside.hit.rate.per.room 	<- outside.hit.rate.per.room

rm(list = c("inside.hit.rate.per.room", "outside.hit.rate.per.room"))

inside.hit.rate.per.room  			<- rbind(pilot.1.inside.hit.rate.per.room,  pilot.2.inside.hit.rate.per.room)
outside.hit.rate.per.room 			<- rbind(pilot.1.outside.hit.rate.per.room, pilot.2.outside.hit.rate.per.room)

rm(list = c("pilot.1.inside.hit.rate.per.room", "pilot.1.outside.hit.rate.per.room", "pilot.2.inside.hit.rate.per.room", "pilot.2.outside.hit.rate.per.room"))

inside.hit.rate.per.room$phase 		<- "Inside"
outside.hit.rate.per.room$phase 	<- "Outside"

hit.rate.per.room 					<- rbind(inside.hit.rate.per.room, outside.hit.rate.per.room)

rm(list = c("inside.hit.rate.per.room", "outside.hit.rate.per.room"))

save(hit.rate.per.room, file = "Together/Data/ResponsePerRoom.RData")

# For ploting curiosity groups
load("Pilot_01_Immediate/GroupData/InsideObjectResponseCuriosityGroups.RData")
pilot.1.inside.hit.rate.curiosity.median  <- inside.hit.rate.curiosity.group.median

load("Pilot_01_Immediate/GroupData/OutsideObjectResponseCuriosityGroups.RData")
pilot.1.outside.hit.rate.curiosity.group.median <- outside.hit.rate.curiosity.group.median

load("Pilot_02_Delayed/GroupData/InsideObjectResponseCuriosityGroups.RData")
pilot.2.inside.hit.rate.curiosity.group.median  <- inside.hit.rate.curiosity.group.median

load("Pilot_02_Delayed/GroupData/OutsideObjectResponseCuriosityGroups.RData")
pilot.2.outside.hit.rate.curiosity.group.median <- outside.hit.rate.curiosity.group.median

rm(list = c("inside.hit.rate.curiosity.group", "inside.hit.rate.curiosity.group.mean", "inside.hit.rate.curiosity.group.median", "outside.hit.rate.curiosity.group", "outside.hit.rate.curiosity.group.mean", "outside.hit.rate.curiosity.group.median"))

pilot.1.inside.hit.rate.curiosity.group.median[, c("Condition", "phase") := list("Immediate", "Inside")]
pilot.1.outside.hit.rate.curiosity.group.median[, c("Condition", "phase") := list("Immediate", "Outside")]
pilot.2.inside.hit.rate.curiosity.group.median[, c("Condition", "phase") := list("Delayed", "Inside")]
pilot.2.outside.hit.rate.curiosity.group.median[, c("Condition", "phase") := list("Delayed", "Outside")]

hit.rate.curiosity.group.median  <- rbind(pilot.1.inside.hit.rate.curiosity.group.median,  pilot.2.inside.hit.rate.curiosity.group.median, pilot.1.outside.hit.rate.curiosity.group.median, pilot.2.outside.hit.rate.curiosity.group.median)

save(hit.rate.curiosity.group.median, file = "Together/Data/ResponseCuriosityGroups.RData")


# For ploting novelty and temporal orders
load("Pilot_01_Immediate/GroupData/InsideObjectResponseItemOrders.RData")
pilot.1.inside.hit.rate.item.order.novelty  <- inside.hit.rate.item.order.novelty

load("Pilot_01_Immediate/GroupData/OutsideObjectResponseItemOrders.RData")
pilot.1.outside.hit.rate.item.order.novelty <- outside.hit.rate.item.order.novelty

load("Pilot_02_Delayed/GroupData/InsideObjectResponseItemOrders.RData")
pilot.2.inside.hit.rate.item.order.novelty  <- inside.hit.rate.item.order.novelty

load("Pilot_02_Delayed/GroupData/OutsideObjectResponseItemOrders.RData")
pilot.2.outside.hit.rate.item.order.novelty <- outside.hit.rate.item.order.novelty

rm(list = c("inside.hit.rate.item.order.curiosity", "inside.hit.rate.item.order.curiosity.mean", "inside.hit.rate.item.order.group", "inside.hit.rate.item.order.novelty", "outside.hit.rate.item.order.curiosity", "outside.hit.rate.item.order.curiosity.mean", "outside.hit.rate.item.order.group", "outside.hit.rate.item.order.novelty"))

pilot.1.inside.hit.rate.item.order.novelty[, c("Condition", "phase") := list("Immediate", "Inside")]
pilot.1.outside.hit.rate.item.order.novelty[, c("Condition", "phase") := list("Immediate", "Outside")]
pilot.2.inside.hit.rate.item.order.novelty[, c("Condition", "phase") := list("Delayed", "Inside")]
pilot.2.outside.hit.rate.item.order.novelty[, c("Condition", "phase") := list("Delayed", "Outside")]

hit.rate.item.order.novelty <- rbind(pilot.1.inside.hit.rate.item.order.novelty, pilot.2.inside.hit.rate.item.order.novelty, pilot.1.outside.hit.rate.item.order.novelty, pilot.2.outside.hit.rate.item.order.novelty)

rm(list = c("pilot.1.inside.hit.rate.item.order.novelty", "pilot.1.outside.hit.rate.item.order.novelty", "pilot.2.inside.hit.rate.item.order.novelty", "pilot.2.outside.hit.rate.item.order.novelty"))

save(hit.rate.item.order.novelty, file = "Together/Data/ResponseNoveltyItemOrders.RData")

# For ploting curiosity median group and temporal orders
load("Pilot_01_Immediate/GroupData/InsideObjectResponseItemOrders.RData")
pilot.1.inside.hit.rate.item.order.curiosity.median  <- inside.hit.rate.item.order.curiosity.median

load("Pilot_01_Immediate/GroupData/OutsideObjectResponseItemOrders.RData")
pilot.1.outside.hit.rate.item.order.curiosity.median <- outside.hit.rate.item.order.curiosity.median

load("Pilot_02_Delayed/GroupData/InsideObjectResponseItemOrders.RData")
pilot.2.inside.hit.rate.item.order.curiosity.median  <- inside.hit.rate.item.order.curiosity.median

load("Pilot_02_Delayed/GroupData/OutsideObjectResponseItemOrders.RData")
pilot.2.outside.hit.rate.item.order.curiosity.median <- outside.hit.rate.item.order.curiosity.median

rm(list = c("inside.hit.rate.item.order.curiosity", "inside.hit.rate.item.order.curiosity.mean", "inside.hit.rate.item.order.curiosity.median", "inside.hit.rate.item.order.group", "inside.hit.rate.item.order.novelty", "outside.hit.rate.item.order.curiosity", "outside.hit.rate.item.order.curiosity.mean", "outside.hit.rate.item.order.curiosity.median", "outside.hit.rate.item.order.group", "outside.hit.rate.item.order.novelty"))

pilot.1.inside.hit.rate.item.order.curiosity.median[, c("Condition", "phase") := list("Immediate", "Inside")]
pilot.1.outside.hit.rate.item.order.curiosity.median[, c("Condition", "phase") := list("Immediate", "Outside")]
pilot.2.inside.hit.rate.item.order.curiosity.median[, c("Condition", "phase") := list("Delayed", "Inside")]
pilot.2.outside.hit.rate.item.order.curiosity.median[, c("Condition", "phase") := list("Delayed", "Outside")]

hit.rate.item.order.curiosity.median <- rbind(pilot.1.inside.hit.rate.item.order.curiosity.median, pilot.2.inside.hit.rate.item.order.curiosity.median, pilot.1.outside.hit.rate.item.order.curiosity.median, pilot.2.outside.hit.rate.item.order.curiosity.median)

rm(list = c("pilot.1.inside.hit.rate.item.order.curiosity.median", "pilot.1.outside.hit.rate.item.order.curiosity.median", "pilot.2.inside.hit.rate.item.order.curiosity.median", "pilot.2.outside.hit.rate.item.order.curiosity.median"))

save(hit.rate.item.order.curiosity.median, file = "Together/Data/ResponseCuriosityItemOrders.RData")


# For ploting trait and curiosity ratings
load("Pilot_02_Delayed/GroupData/QuestionnaireScores.RData")
load("Pilot_02_Delayed/GroupData/InsideObjectsResponsePerRoom.RData")
load("Pilot_02_Delayed/GroupData/OutsideObjectsResponsePerRoom.RData")

participants.to.be.excluded <- c("P17", "P54")

questionnaire <- questionnaire[!SubjectNo %in% participants.to.be.excluded]
outside.hit.rate.per.room <- outside.hit.rate.per.room[!SubjectNo %in% participants.to.be.excluded]
inside.hit.rate.per.room  <- inside.hit.rate.per.room[! SubjectNo %in% participants.to.be.excluded]

outside.hit.rate.per.room <- merge(outside.hit.rate.per.room, questionnaire[, c("SubjectNo", "PC", "EC", "GN")])
inside.hit.rate.per.room  <- merge(inside.hit.rate.per.room, questionnaire[, c("SubjectNo", "PC", "EC", "GN")])

trait.ratings <- outside.hit.rate.per.room[, .(meanCurRating = mean(CurRating, na.rm = T)), by = c("SubjectNo", "PC", "EC", "GN")]
trait.ratings.novel <- outside.hit.rate.per.room[Group == "Novel", .(meanCurRating = mean(CurRating, na.rm = T)), by = c("SubjectNo", "PC", "EC", "GN")]

save(trait.ratings, trait.ratings.novel, file = "Pilot_02_Delayed/GroupData/TraitAndAverageRatings.RData")


# For ploting trait and general memory performance
outside.hit.rate.trait <- outside.hit.rate.per.room[, .(meanSAcc = mean(SAcc, na.rm = T), meanSrcAcc = mean(SrcAcc, na.rm = T)), by = c("SubjectNo", "PC", "EC", "GN")]
inside.hit.rate.trait  <- inside.hit.rate.per.room[, .(meanSAcc = mean(SAcc, na.rm = T), meanSrcAcc = mean(SrcAcc, na.rm = T)), by = c("SubjectNo", "PC", "EC", "GN")]

outside.hit.rate.trait[meanSAcc < 0]$meanSAcc <- 0; outside.hit.rate.trait[meanSrcAcc < 0]$meanSrcAcc <- 0
inside.hit.rate.trait[meanSAcc < 0]$meanSAcc  <- 0; inside.hit.rate.trait[meanSrcAcc < 0]$meanSrcAcc  <- 0

outside.hit.rate.trait$phase <- "Outside"
inside.hit.rate.trait$phase  <- "Inside"

hit.rate.trait <- rbind(outside.hit.rate.trait, inside.hit.rate.trait)

save(hit.rate.trait, file = "Pilot_02_Delayed/GroupData/TraitAndMemory.RData")


# For plotting trait and difference in memory performance between *novel* and *familiar* rooms
load("Pilot_02_Delayed/GroupData/QuestionnaireScores.RData")
load("Pilot_02_Delayed/GroupData/InsideObjectResponseNoveltyGroups.RData")
load("Pilot_02_Delayed/GroupData/OutsideObjectResponseNoveltyGroups.RData")

participants.to.be.excluded <- c("P17", "P54")

questionnaire <- questionnaire[!SubjectNo %in% participants.to.be.excluded]

inside.hit.rate.novelty.group[, phase := "Inside"]
outside.hit.rate.novelty.group[, phase := "Outside"]

pilot.2.hit.rate.novelty.group  <- rbind(inside.hit.rate.novelty.group, outside.hit.rate.novelty.group)
pilot.2.hit.rate.novelty.group  <- pilot.2.hit.rate.novelty.group[!SubjectNo %in% participants.to.be.excluded]

pilot.2.hit.rate.novelty.group  <- merge(pilot.2.hit.rate.novelty.group, questionnaire[, c("SubjectNo", "PC", "EC", "GN")], by = "SubjectNo")

pilot.2.hit.rate.novelty.group[SAcc < 0]$SAcc <- 0
pilot.2.hit.rate.novelty.group[SrcAcc < 0]$SrcAcc <- 0

hit.rate.novel 		<- pilot.2.hit.rate.novelty.group[Group == "Novel"]
hit.rate.familiar 	<- pilot.2.hit.rate.novelty.group[Group == "Familiar"]

pilot.2.hit.rate.novelty.group.diff <- hit.rate.novel[, c("SubjectNo", "phase", "PC", "EC", "GN", "SAcc", "SrcAcc")]
pilot.2.hit.rate.novelty.group.diff$diffSAcc <- hit.rate.novel$SAcc - hit.rate.familiar$SAcc
pilot.2.hit.rate.novelty.group.diff$diffSrcAcc <- hit.rate.novel$SrcAcc - hit.rate.familiar$SrcAcc

save(pilot.2.hit.rate.novelty.group.diff, file = "Pilot_02_Delayed/GroupData/TraitAndNoveltyGroupDiff.RData")


# For plotting trait and difference in memory performance between *high* and *low* curious rooms
load("Pilot_02_Delayed/GroupData/QuestionnaireScores.RData")
load("Pilot_02_Delayed/GroupData/InsideObjectResponseCuriosityGroups.RData")
load("Pilot_02_Delayed/GroupData/OutsideObjectResponseCuriosityGroups.RData")

participants.to.be.excluded <- c("P17", "P54")

questionnaire <- questionnaire[!SubjectNo %in% participants.to.be.excluded]

inside.hit.rate.curiosity.group.median[, phase := "Inside"]
outside.hit.rate.curiosity.group.median[, phase := "Outside"]

pilot.2.hit.rate.curiosity.group.median  <- rbind(inside.hit.rate.curiosity.group.median, outside.hit.rate.curiosity.group.median)
pilot.2.hit.rate.curiosity.group.median  <- pilot.2.hit.rate.curiosity.group.median[!SubjectNo %in% participants.to.be.excluded]

pilot.2.hit.rate.curiosity.group.median  <- merge(pilot.2.hit.rate.curiosity.group.median, questionnaire[, c("SubjectNo", "PC", "EC", "GN")], by = "SubjectNo")

pilot.2.hit.rate.curiosity.group.median[SAcc < 0]$SAcc <- 0
pilot.2.hit.rate.curiosity.group.median[SrcAcc < 0]$SrcAcc <- 0

hit.rate.high 		<- pilot.2.hit.rate.curiosity.group.median[CurGrpMedian == "High"]
hit.rate.low 	    <- pilot.2.hit.rate.curiosity.group.median[CurGrpMedian == "Low"]
hit.rate.low		<- hit.rate.low[SubjectNo %in% hit.rate.high$SubjectNo]

pilot.2.hit.rate.curiosity.group.diff <- hit.rate.high[, c("SubjectNo", "phase", "PC", "EC", "GN", "SAcc", "SrcAcc")]
pilot.2.hit.rate.curiosity.group.diff$diffSAcc <- hit.rate.high$SAcc - hit.rate.low$SAcc
pilot.2.hit.rate.curiosity.group.diff$diffSrcAcc <- hit.rate.high$SrcAcc - hit.rate.low$SrcAcc

save(pilot.2.hit.rate.curiosity.group.diff, file = "Pilot_02_Delayed/GroupData/TraitAndCuriosityGroupDiff.RData")

# For ploting relationship between curiosity ratings and memory performance
