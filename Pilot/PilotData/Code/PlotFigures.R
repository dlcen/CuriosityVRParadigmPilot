# Plot figures that incorporate all data from Pilot 1 and 2
# 
# Figures include:
# 
# 	- Individual plots: item recollection, source memory and false alarm rates for both as a function of rooms (with curioisty rating and color-coded by novelty group)
#   - Novelty plots: item recollection, source memory in each novelty group, for both Pilot 1 (immediate) and Pilot 2 (delayed) conditions.
#   - Novelty (temporal order) plots: item recollection, source memory respectively for the Early and Later groups.
#   - Curiosity plots
#   - Curiosity (temporal order) plots
  
library(ggplot2); library(data.table); library(cowplot); library(Hmisc); library(reshape)

#  Individual plots
## Load the data
# load("Pilot_01_Immediate/GroupData/InsideObjectsResponsePerRoom.RData")
# load("Pilot_01_Immediate/GroupData/OutsideObjectsResponsePerRoom.RData")

# inside.hit.rate.per.room$Condition  <- "Immediate"
# outside.hit.rate.per.room$Condition <- "Immediate"

# pilot.1.inside.hit.rate.per.room 	<- inside.hit.rate.per.room
# pilot.1.outside.hit.rate.per.room 	<- outside.hit.rate.per.room

# load("Pilot_02_Delayed/GroupData/InsideObjectsResponsePerRoom.RData")
# load("Pilot_02_Delayed/GroupData/OutsideObjectsResponsePerRoom.RData")

# inside.hit.rate.per.room$Condition  <- "Delayed"
# outside.hit.rate.per.room$Condition <- "Delayed"

# pilot.2.inside.hit.rate.per.room 	<- inside.hit.rate.per.room
# pilot.2.outside.hit.rate.per.room 	<- outside.hit.rate.per.room

# rm(list = c("inside.hit.rate.per.room", "outside.hit.rate.per.room"))

# inside.hit.rate.per.room  			<- rbind(pilot.1.inside.hit.rate.per.room,  pilot.2.inside.hit.rate.per.room)
# outside.hit.rate.per.room 			<- rbind(pilot.1.outside.hit.rate.per.room, pilot.2.outside.hit.rate.per.room)

# rm(list = c("pilot.1.inside.hit.rate.per.room", "pilot.1.outside.hit.rate.per.room", "pilot.2.inside.hit.rate.per.room", "pilot.2.outside.hit.rate.per.room"))

# inside.hit.rate.per.room$phase 		<- "Inside"
# outside.hit.rate.per.room$phase 	<- "Outside"

# hit.rate.per.room 					<- rbind(inside.hit.rate.per.room, outside.hit.rate.per.room)

# rm(list = c("inside.hit.rate.per.room", "outside.hit.rate.per.room"))

# save(hit.rate.per.room, file = "Together/Data/ResponsePerRoom.RData")

load("Together/Data/ResponsePerRoom.RData")
hit.rate.per.room$Condition <- factor(hit.rate.per.room$Condition)
hit.rate.per.room$Condition <- factor(hit.rate.per.room$Condition, levels = levels(hit.rate.per.room$Condition)[c(2, 1)])

hit.rate.per.room$phase 	<- factor(hit.rate.per.room$phase)
hit.rate.per.room$phase 	<- factor(hit.rate.per.room$phase, levels = levels(hit.rate.per.room$phase)[c(2, 1)])

hit.rate.per.room$Group 	<- factor(hit.rate.per.room$Group)
hit.rate.per.room$Type 		<- factor(hit.rate.per.room$Type)

## Plot the figure
source("Code/PlotFunc.R")

conditions  <- c("Immediate", "Delayed")
phases 		<- c("Inside", "Outside")
measures    <- c("SHit", "SrcHit")
fas 		<- c("SFalse", "SrcFalse")

labels 		<- data.frame(SHit = "Hit rate for item recollection", SrcHit = "Hit rate for source memory")

for (this.condition in conditions) {
	for (this.phase in phases) {
		for (this.measure in measures) {
			for (this.fa in fas) {
				this.filename <- paste("IndividualPlot", this.condition, this.phase, this.measure, sep = "_")
				this.data <- hit.rate.per.room[Condition == this.condition & phase == this.phase]
				IndividualDataPlot(this.data, this.data$Context, this.data[[this.measure]], this.data[[this.fa]], labels[, this.measure], this.filename )
			}
		}
	}
}

# Plot the relationship between Novelty and Curiosity ratings
## Calculate the mean rating for each group for each individual participant
mean.rating.per.novelty.grp <- hit.rate.per.room[, .(meanCurRating = mean(CurRating, na.rm = T)), by = c("SubjectNo", "Group")]

ggplot(data = mean.rating.per.novelty.grp, aes(x = Group, y = meanCurRating)) + 
	geom_violin(trim = F) +
	geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.75) +
	stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "pointrange", color = "red", size = 1.5, shape = 15) +
	labs(x = "Novelty", y = "Curiosity rating") +
	theme(axis.title = element_text(size = 16),
		  axis.text = element_text(size = 14))
	
ggplot2::ggsave("Together/Figures/NoveltyRatings.png", width = 6.4, height = 5.7)


# Plot the difference in performance depending on novelty
mean.data <- hit.rate.per.room[, .(SAcc = mean(SAcc, na.rm = T), SrcAcc = mean(SrcAcc, na.rm = T)), by = c("SubjectNo", "Group", "Condition", "phase")]
melt.data <- reshape::melt(mean.data, id.vars = c("SubjectNo", "Condition", "phase", "Group"), measure.vars = c("SAcc", "SrcAcc"))
names(melt.data)[c(5, 6)] <- c("Type", "Acc")
melt.data[Acc < 0]$Acc <- 0

ggplot(melt.data, aes(x = Type, y = Acc, fill = Group)) + theme_minimal()+
	stat_summary(fun.y = mean, geom = "bar", position = position_dodge(width = 0.95)) +
	stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, position = position_dodge(width = 0.95)) +
	geom_hline(yintercept = 0) +
	facet_grid(phase ~ Condition) +
	labs(y = "Corrected hit rate") +
	scale_fill_manual(values = c("#cccccc", "#636363")) + 
	scale_x_discrete(labels = c("Item recollection", "Source memory")) +
	theme(axis.title.x = element_blank(), 
		  axis.title.y = element_text(size = 16),
		  axis.text.x = element_text(size = 14),
		  axis.text.y = element_text(size = 12),
		  legend.title = element_blank(),
		  legend.text = element_text(size = 14),
		  strip.text = element_text(face = "bold", size = 16))

ggplot2::ggsave("Together/Figures/Novelty.png", width = 10.8, height = 8.1)


