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
load("Together/Data/ResponsePerRoom.RData")
hit.rate.per.room$Condition <- factor(hit.rate.per.room$Condition)
hit.rate.per.room$Condition <- factor(hit.rate.per.room$Condition, levels = levels(hit.rate.per.room$Condition)[c(2, 1)])

hit.rate.per.room$phase 	<- factor(hit.rate.per.room$phase)
hit.rate.per.room$phase 	<- factor(hit.rate.per.room$phase, levels = levels(hit.rate.per.room$phase)[c(2, 1)])

hit.rate.per.room$Group 	<- factor(hit.rate.per.room$Group)
hit.rate.per.room$Type 		<- factor(hit.rate.per.room$Type)

## Plot the figure (don't exclude the participants at this stage)
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
participants.to.be.excluded <- c("P17", "P54")
hit.rate.per.room			<- hit.rate.per.room[!SubjectNo %in% participants.to.be.excluded] # Exlude the participants 

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


# Plot curiosity groups 
## Load the data
load("Together/Data/ResponseCuriosityGroups.RData")
hit.rate.curiosity.group.median <- hit.rate.curiosity.group.median[!SubjectNo %in% participants.to.be.excluded]

hit.rate.curiosity.group.median$CurGrpMedian <- factor(hit.rate.curiosity.group.median$CurGrpMedian)
hit.rate.curiosity.group.median$CurGrpMedian <- factor(hit.rate.curiosity.group.median$CurGrpMedian, levels = levels(hit.rate.curiosity.group.median$CurGrpMedian)[c(2, 1)])

hit.rate.curiosity.group.median$Condition 	 <- factor(hit.rate.curiosity.group.median$Condition)
hit.rate.curiosity.group.median$Condition 	 <- factor(hit.rate.curiosity.group.median$Condition, levels = levels(hit.rate.curiosity.group.median$Condition)[c(2, 1)])

hit.rate.curiosity.group.median$phase 		 <- factor(hit.rate.curiosity.group.median$phase)
hit.rate.curiosity.group.median$phase 		 <- factor(hit.rate.curiosity.group.median$phase, levels = levels(hit.rate.curiosity.group.median$phase)[c(2, 1)])

melt.data <- reshape::melt(hit.rate.curiosity.group.median, id.vars = c("SubjectNo", "Condition", "phase", "CurGrpMedian"), measure.vars = c("SAcc", "SrcAcc"))
names(melt.data)[c(5, 6)] <- c("Type", "Acc")
melt.data[Acc < 0]$Acc <- 0

ggplot(melt.data, aes(x = Type, y = Acc, fill = CurGrpMedian)) + theme_minimal()+
	stat_summary(fun.y = mean, geom = "bar", position = position_dodge(width = 0.95)) +
	stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, position = position_dodge(width = 0.95)) +
	geom_hline(yintercept = 0) +
	facet_grid(phase ~ Condition) +
	labs(y = "Corrected hit rate") +
	scale_fill_manual(values = c("#fee5d9", "#de2d26")) + 
	scale_x_discrete(labels = c("Item recollection", "Source memory")) +
	theme(axis.title.x = element_blank(), 
		  axis.title.y = element_text(size = 16),
		  axis.text.x = element_text(size = 14),
		  axis.text.y = element_text(size = 12),
		  legend.title = element_blank(),
		  legend.text = element_text(size = 14),
		  strip.text = element_text(face = "bold", size = 16))

ggplot2::ggsave("Together/Figures/CuriosityGroups.png", width = 10.8, height = 8.1)

# Plot novelty and temporal order
load("Together/Data/ResponseNoveltyItemOrders.RData")
hit.rate.item.order.novelty <- hit.rate.item.order.novelty[!SubjectNo %in% participants.to.be.excluded]

hit.rate.item.order.novelty$Group        <- factor(hit.rate.item.order.novelty$Group)
hit.rate.item.order.novelty$ObjOrdGrp 	 <- factor(hit.rate.item.order.novelty$ObjOrdGrp)

hit.rate.item.order.novelty$Condition 	 <- factor(hit.rate.item.order.novelty$Condition)
hit.rate.item.order.novelty$Condition 	 <- factor(hit.rate.item.order.novelty$Condition, levels = levels(hit.rate.item.order.novelty$Condition)[c(2, 1)])

hit.rate.item.order.novelty$phase 		 <- factor(hit.rate.item.order.novelty$phase)
hit.rate.item.order.novelty$phase 		 <- factor(hit.rate.item.order.novelty$phase, levels = levels(hit.rate.item.order.novelty$phase)[c(2, 1)])

melt.data <- reshape::melt(hit.rate.item.order.novelty, id.vars = c("SubjectNo", "Condition", "phase", "Group", "ObjOrdGrp"), measure.vars = c("SAcc", "SrcAcc"))
names(melt.data)[c(6, 7)] <- c("Type", "Acc")
melt.data[Acc < 0]$Acc <- 0

ggplot(melt.data, aes(x = Type, y = Acc, fill = interaction(Group, ObjOrdGrp))) + theme_minimal()+
	stat_summary(fun.y = mean, geom = "bar", position = position_dodge(width = 0.95)) +
	stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, position = position_dodge(width = 0.95)) +
	geom_hline(yintercept = 0) +
	facet_grid(phase ~ Condition) +
	labs(y = "Corrected hit rate") +
	scale_fill_manual(values = c("#d9d9d9", "#737373", "#bdbdbd", "#525252"), labels = c("Familiar-Early", "Novel-Early", "Familiar-Later", "Novel-Later")) + 
	scale_x_discrete(labels = c("Item recollection", "Source memory")) +
	theme(axis.title.x = element_blank(), 
		  axis.title.y = element_text(size = 16),
		  axis.text.x = element_text(size = 14),
		  axis.text.y = element_text(size = 12),
		  legend.title = element_blank(),
		  legend.text = element_text(size = 14),
		  strip.text = element_text(face = "bold", size = 16))

ggplot2::ggsave("Together/Figures/NoveltyItemOrder.png", width = 10.8, height = 8.1)

# Plot curiosity and temporal order
load("Together/Data/ResponseCuriosityItemOrders.RData")
hit.rate.item.order.curiosity.median <- hit.rate.item.order.curiosity.median[!SubjectNo %in% participants.to.be.excluded & !is.na(CurGrpMedian)]

hit.rate.item.order.curiosity.median$CurGrpMedian   <- factor(hit.rate.item.order.curiosity.median$CurGrpMedian)
hit.rate.item.order.curiosity.median$ObjOrdGrp 	 	<- factor(hit.rate.item.order.curiosity.median$ObjOrdGrp)

hit.rate.item.order.curiosity.median$CurGrpMedian   <- factor(hit.rate.item.order.curiosity.median$CurGrpMedian, levels = levels(hit.rate.item.order.curiosity.median$CurGrpMedian)[c(2, 1)])

hit.rate.item.order.curiosity.median$Condition 	 	<- factor(hit.rate.item.order.curiosity.median$Condition)
hit.rate.item.order.curiosity.median$Condition 	 	<- factor(hit.rate.item.order.curiosity.median$Condition, levels = levels(hit.rate.item.order.curiosity.median$Condition)[c(2, 1)])

hit.rate.item.order.curiosity.median$phase 		 	<- factor(hit.rate.item.order.curiosity.median$phase)
hit.rate.item.order.curiosity.median$phase 			<- factor(hit.rate.item.order.curiosity.median$phase, levels = levels(hit.rate.item.order.curiosity.median$phase)[c(2, 1)])

melt.data <- reshape::melt(hit.rate.item.order.curiosity.median, id.vars = c("SubjectNo", "Condition", "phase", "CurGrpMedian", "ObjOrdGrp"), measure.vars = c("SAcc", "SrcAcc"))
names(melt.data)[c(6, 7)] <- c("Type", "Acc")
melt.data[Acc < 0]$Acc <- 0

ggplot(melt.data, aes(x = Type, y = Acc, fill = interaction(CurGrpMedian, ObjOrdGrp))) + theme_minimal()+
	stat_summary(fun.y = mean, geom = "bar", position = position_dodge(width = 0.95)) +
	stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, position = position_dodge(width = 0.95)) +
	geom_hline(yintercept = 0) +
	facet_grid(phase ~ Condition) +
	labs(y = "Corrected hit rate") +
	scale_fill_manual(values = c("#fcbba1", "#ef3b2c", "#fc9272", "#cb181d"), labels = c("Low-Early", "High-Early", "Low-Later", "High-Later")) + 
	scale_x_discrete(labels = c("Item recollection", "Source memory")) +
	theme(axis.title.x = element_blank(), 
		  axis.title.y = element_text(size = 16),
		  axis.text.x = element_text(size = 14),
		  axis.text.y = element_text(size = 12),
		  legend.title = element_blank(),
		  legend.text = element_text(size = 14),
		  strip.text = element_text(face = "bold", size = 16))

ggplot2::ggsave("Together/Figures/CuriosityItemOrder.png", width = 10.8, height = 8.1)

# Plot individual differences (trait) and its relationships with curiosity ratings
load("Pilot_02_Delayed/GroupData/TraitAndAverageRatings.RData")

PC_plot <- ggplot(data = trait.ratings, aes(x = PC, y = meanCurRating)) + theme_bw() +
  geom_point(size = 2, color = "grey70") +
  stat_smooth(method = "lm", se = FALSE, color = "red") +
  ylim(1, 7) +
  labs(x = "PC score", y = "Mean curiosity rating") +
  theme(axis.title = element_text(size = 16),
  		axis.text = element_text(size = 12))

EC_plot <- ggplot(data = trait.ratings, aes(x = EC, y = meanCurRating)) + theme_bw() +
  geom_point(size = 2, color = "grey70") +
  stat_smooth(method = "lm", se = FALSE, color = "red") +
  ylim(1, 7) +
  labs(x = "EC score", y = "") +
  theme(axis.title.y = element_blank()) +
  theme(axis.title = element_text(size = 16),
  		axis.text = element_text(size = 12))

GN_plot <- ggplot(data = trait.ratings, aes(x = GN, y = meanCurRating))  + theme_bw() +
  geom_point(size = 2, color = "grey70") +
  stat_smooth(method = "lm", se = FALSE, color = "red") +
  ylim(1, 7) +
  labs(x = "General score", y = "") +
  theme(axis.title.y = element_blank()) +
  theme(axis.title = element_text(size = 16),
  		axis.text = element_text(size = 12))

plot_grid(PC_plot, EC_plot, GN_plot, nrow = 1)

ggplot2::ggsave("Together/Figures/Pilot_2_TraitAndCuriosityRatings.png", width = 10.8, height = 4.8)

## For novel rooms only
PC_plot <- ggplot(data = trait.ratings.novel, aes(x = PC, y = meanCurRating)) + theme_bw() +
  geom_point(size = 2, color = "grey70") +
  stat_smooth(method = "lm", se = FALSE, color = "red") +
  ylim(1, 7) +
  labs(x = "PC score", y = "Mean curiosity rating") +
  theme(axis.title = element_text(size = 16),
  		axis.text = element_text(size = 12))

EC_plot <- ggplot(data = trait.ratings.novel, aes(x = EC, y = meanCurRating)) + theme_bw() +
  geom_point(size = 2, color = "grey70") +
  stat_smooth(method = "lm", se = FALSE, color = "red") +
  ylim(1, 7) +
  labs(x = "EC score", y = "") +
  theme(axis.title.y = element_blank()) +
  theme(axis.title = element_text(size = 16),
  		axis.text = element_text(size = 12))

GN_plot <- ggplot(data = trait.ratings.novel, aes(x = GN, y = meanCurRating))  + theme_bw() +
  geom_point(size = 2, color = "grey70") +
  stat_smooth(method = "lm", se = FALSE, color = "red") +
  ylim(1, 7) +
  labs(x = "General score", y = "") +
  theme(axis.title.y = element_blank()) +
  theme(axis.title = element_text(size = 16),
  		axis.text = element_text(size = 12))

plot_grid(PC_plot, EC_plot, GN_plot, nrow = 1)

ggplot2::ggsave("Together/Figures/Pilot_2_TraitAndCuriosityRatings_NovelRooms.png", width = 10.8, height = 4.8)

# Plot trait and memory performance
load("Pilot_02_Delayed/GroupData/MemoryAndAverageRatings.RData")

melt.data <- reshape::melt(hit.rate.trait, id.vars = c("SubjectNo", "PC", "EC", "GN", "phase"), measure.vars = c("meanSAcc", "meanSrcAcc"))
names(melt.data)[c(6, 7)] <- c("Type", "Acc")
melt.data[Acc < 0]$Acc <- 0

melt.melt.data <- reshape::melt(melt.data, id.vars = c("SubjectNo", "phase", "Type"), measure.vars = c("PC", "EC", "GN"))
names(melt.melt.data)[c(4, 5)] <- c("Scale", "Score")
melt.data.rep  <- do.call("rbind", replicate(3, melt.data, simplify = FALSE))
melt.melt.data$Acc <- melt.data.rep$Acc

melt.melt.data$phase <- factor(melt.melt.data$phase)
melt.melt.data$phase <- factor(melt.melt.data$phase, levels = levels(melt.melt.data$phase)[c(2, 1)])

melt.melt.data$Scale <- factor(melt.melt.data$Scale, labels = c("PC Score", "EC Score", "General Score"))

melt.melt.data$Type  <- factor(melt.melt.data$Type, labels = c("Item recollection", "Source memory"))

ggplot(data = melt.melt.data, aes(x = Score, y = Acc, group = Type)) + theme_minimal() +
	geom_point(size = 2, aes(color = Type), alpha = 0.5) +
	stat_smooth(method = "lm", se = FALSE, aes(color = Type)) +
	geom_hline(yintercept = 0) +
  	labs(x = "Trait scale score", y = "Mean corrected hit rate") +
  	scale_color_manual(values = c("#b2182b", "#4d4d4d")) +
  	facet_grid(phase ~ Scale) +
  	theme(axis.title = element_text(size = 16),
  		  axis.text = element_text(size = 12),
  		  axis.line.y = element_line(color = "#595959"),
  		  panel.grid = element_blank(),
  		  legend.title = element_blank(),
  		  legend.text = element_text(size = 14),
  		  legend.position = c(.9, .9),
  		  strip.text = element_text(face = "bold", size = 16))

ggplot2::ggsave("Together/Figures/Pilot_2_TraitAndMemory.png", width = 10.8, height = 12)

# Plot trait and difference in memory performance between *novel* and *familiar* rooms
load("Pilot_02_Delayed/GroupData/TraitAndNoveltyGroupDiff.RData")

melt.data <- reshape::melt(pilot.2.hit.rate.novelty.group.diff, id.vars = c("SubjectNo", "PC", "EC", "GN", "phase"), measure.vars = c("diffSAcc", "diffSrcAcc"))
names(melt.data)[c(6, 7)] <- c("Type", "diffAcc")

melt.melt.data <- reshape::melt(melt.data, id.vars = c("SubjectNo", "phase", "Type"), measure.vars = c("PC", "EC", "GN"))
names(melt.melt.data)[c(4, 5)] <- c("Scale", "Score")
melt.data.rep  <- do.call("rbind", replicate(3, melt.data, simplify = FALSE))
melt.melt.data$diffAcc <- melt.data.rep$diffAcc

melt.melt.data$phase <- factor(melt.melt.data$phase)
melt.melt.data$phase <- factor(melt.melt.data$phase, levels = levels(melt.melt.data$phase)[c(2, 1)])

melt.melt.data$Scale <- factor(melt.melt.data$Scale, labels = c("PC Score", "EC Score", "General Score"))

melt.melt.data$Type  <- factor(melt.melt.data$Type, labels = c("Item recollection", "Source memory"))

ggplot(data = melt.melt.data, aes(x = Score, y = diffAcc, group = Type)) + theme_minimal() +
	geom_point(size = 2, aes(color = Type), alpha = 0.5) +
	stat_smooth(method = "lm", se = FALSE, aes(color = Type)) +
	# geom_hline(yintercept = 0) +
  	labs(x = "Trait scale score", y = "Difference in corrected hit rate") +
  	scale_color_manual(values = c("#b2182b", "#4d4d4d")) +
  	facet_grid(phase ~ Scale) +
  	theme(axis.title = element_text(size = 16),
  		  axis.text = element_text(size = 12),
  		  axis.line = element_line(color = "#595959"),
  		  panel.grid = element_blank(),
  		  legend.title = element_blank(),
  		  legend.text = element_text(size = 14),
  		  strip.text = element_text(face = "bold", size = 16))

ggplot2::ggsave("Together/Figures/Pilot_2_TraitAndNoveltyMemoryDifference.png", width = 12, height = 12)

# Plot trait and difference in memory performance between *high* and *low* curiosity groups
load("Pilot_02_Delayed/GroupData/TraitAndCuriosityGroupDiff.RData")

melt.data <- reshape::melt(pilot.2.hit.rate.curiosity.group.diff, id.vars = c("SubjectNo", "PC", "EC", "GN", "phase"), measure.vars = c("diffSAcc", "diffSrcAcc"))
names(melt.data)[c(6, 7)] <- c("Type", "diffAcc")

melt.melt.data <- reshape::melt(melt.data, id.vars = c("SubjectNo", "phase", "Type"), measure.vars = c("PC", "EC", "GN"))
names(melt.melt.data)[c(4, 5)] <- c("Scale", "Score")
melt.data.rep  <- do.call("rbind", replicate(3, melt.data, simplify = FALSE))
melt.melt.data$diffAcc <- melt.data.rep$diffAcc

melt.melt.data$phase <- factor(melt.melt.data$phase)
melt.melt.data$phase <- factor(melt.melt.data$phase, levels = levels(melt.melt.data$phase)[c(2, 1)])

melt.melt.data$Scale <- factor(melt.melt.data$Scale, labels = c("PC Score", "EC Score", "General Score"))

melt.melt.data$Type  <- factor(melt.melt.data$Type, labels = c("Item recollection", "Source memory"))

ggplot(data = melt.melt.data, aes(x = Score, y = diffAcc, group = Type)) + theme_minimal() +
	geom_point(size = 2, aes(color = Type), alpha = 0.5) +
	stat_smooth(method = "lm", se = FALSE, aes(color = Type)) +
	# geom_hline(yintercept = 0) +
  	labs(x = "Trait scale score", y = "Difference in corrected hit rate") +
  	scale_color_manual(values = c("#b2182b", "#4d4d4d")) +
  	facet_grid(phase ~ Scale) +
  	theme(axis.title = element_text(size = 16),
  		  axis.text = element_text(size = 12),
  		  axis.line = element_line(color = "#595959"),
  		  panel.grid = element_blank(),
  		  legend.title = element_blank(),
  		  legend.text = element_text(size = 14),
  		  strip.text = element_text(face = "bold", size = 16))

ggplot2::ggsave("Together/Figures/Pilot_2_TraitAndCuriosityMemoryDifference.png", width = 12, height = 12)

# Plot relationship between curiosity ratings and memory performance
load("Together/Data/ResponsePerRoom.RData")

participants.to.be.excluded <- c("P17", "P54")
hit.rate.per.room			<- hit.rate.per.room[!SubjectNo %in% participants.to.be.excluded] # Exlude the participants 

melt.data <- reshape::melt(hit.rate.per.room, id.vars = c("SubjectNo", "Context", "CurRating", "Group", "Condition", "phase"), measure.vars = c("SAcc", "SrcAcc"))
names(melt.data)[c(7, 8)] <- c("Type", "Acc")
melt.data[Acc < 0]$Acc <- 0

melt.data$phase <- factor(melt.data$phase)
melt.data$phase <- factor(melt.data$phase, levels = levels(melt.data$phase)[c(2, 1)])

melt.data$Condition <- factor(melt.data$Condition)
melt.data$Condition <- factor(melt.data$Condition, levels = levels(melt.data$Condition)[c(2, 1)])

melt.data$Type  <- factor(melt.data$Type, labels = c("Item recollection", "Source memory"))

ggplot(data = melt.data, aes(x = CurRating, y = Acc, group = Type)) + theme_minimal() +
	geom_point(size = 2, aes(color = Type), alpha = 0.25) +
	stat_smooth(method = "lm", se = FALSE, aes(color = Type)) +
	# geom_hline(yintercept = 0) +
  	labs(x = "Curiosity rating", y = "Corrected hit rate") +
  	scale_color_manual(values = c("#b2182b", "#4d4d4d")) +
  	facet_grid(phase ~ Condition) +
  	theme(axis.title = element_text(size = 16),
  		  axis.text = element_text(size = 12),
  		  axis.line = element_line(color = "#595959"),
  		  panel.grid = element_blank(),
  		  legend.title = element_blank(),
  		  legend.text = element_text(size = 14),
  		  strip.text = element_text(face = "bold", size = 16))

ggplot2::ggsave("Together/Figures/RatingAndMemory.png", width = 10.8, height = 7.2)
