# This code is to draw figures for presentation or publication

library(ggplot2); library(data.table); library(cowplot)

# Function for drawing bar figure to compare the difference between conditions
BarComparisonFigure <- function(data, iv, dv, xlabel, ylabel, figureName) {
	ggplot(data, aes(x = iv, y = dv)) + theme_classic() +
  		stat_summary(fun.y = mean, geom = "bar", aes(fill = iv)) +
  		stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  		scale_fill_manual(values = c("grey55", "#cc0033")) +
  		labs(x = xlabel, y = ylabel) +
  		theme(legend.position = "none")

  	ggplot2::ggsave(paste0("Figures/", figureName, ".png"), width=6.4, height=5.7, dpi = 300, units = "cm") 

}

# Function for drawing linear trend respectively for two groups
LinearTrendTwoGroups <- function(data, iv, dv, groupType, group1, group2, xlabel, ylabel, figureName) {
	ggplot(data, aes(x = iv, y = dv)) + theme_classic() +
	  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0, aes(group = groupType, colour = groupType), position = position_dodge(width = 0.25), size = 0.25, alpha = 0.5) +
	  stat_summary(fun.y = mean, geom = "point", aes(group = groupType, colour = groupType), position = position_dodge(width = 0.25), size = 2, alpha = 0.75) +
	  stat_smooth(method = "lm", formula = y ~ x, se = FALSE, aes(group = groupType, colour = groupType)) +
	  labs(x = xlabel, y = ylabel) +
	  scale_x_continuous(breaks = c(1:6)) +
	  scale_color_manual(values = c("grey45", "#cc0033"), labels = c(group1, group2)) +
	  theme(legend.title = element_blank(),
	  		legend.position = c(0.85, 0.9),
	        axis.title = element_text(face = "bold"))

	ggplot2::ggsave(paste0("Figures/", figureName, ".png"), width=10.8, height=9, dpi = 300, units = "cm") 
}

# Drawing
## Outside
### Item
theData <- OutsideObjectsMemory[!SubjectNo %in% bad.ps.less.strict, .(MeanCorrSeenHit = mean(CorrSeenHit)), by = c("SubjectNo", "Group")]
figName <- "OutsideItemCorrSeenHitNovelty"
BarComparisonFigure(theData, theData$Group, theData$MeanCorrSeenHit, "Room Novelty", "Mean corrected hit rate", figName)

theData <- Curiosity.Recall.Old.Collapsed.CuriosityType.MeanSep[!SubjectNo %in% bad.ps.less.strict]
figName <- "OutsideItemCorrSeenHitCuriosityGroup"
BarComparisonFigure(theData, theData$RatingGroup, theData$CorrSeenHit, "Curiosity Rating", "Mean corrected hit rate", figName)

theData <- Curiosity.Recall.Old.Novelty[!SubjectNo %in% bad.ps.less.strict]
figName <- "OutsideItemCorrSeenHitLinearNoveltyGroups"
LinearTrendTwoGroups(theData, theData$ItemOrder, theData$CorrSeenHit, theData$Group, "Familiar Rooms", "Novel Rooms", "Item Order", "Corrected Hit Rate", figName)

theData <- Curiosity.Recall.Old.CuriosityType.MeanSep[!SubjectNo %in% bad.ps.less.strict]
figName <- "OutsideItemCorrSeenHitLinearCuriosityGroups"
LinearTrendTwoGroups(theData, theData$ItemOrder, theData$CorrSeenHit, theData$RatingGroup, "Low Curiosity", "High Curiosity", "Item Order", "Corrected Hit Rate", figName)

### Source
theData <- OutsideObjectsMemory[!SubjectNo %in% bad.ps.less.strict, .(MeanCorrSeenHit = mean(CorrContextAccuracy)), by = c("SubjectNo", "Group")]
figName <- "OutsideSourceCorrSeenHitNovelty"
BarComparisonFigure(theData, theData$Group, theData$MeanCorrSeenHit, "Room Novelty", "Mean corrected hit rate", figName)

theData <- Curiosity.Recall.Old.Collapsed.CuriosityType.MeanSep[!SubjectNo %in% bad.ps.less.strict]
figName <- "OutsideSourceCorrSeenHitCuriosityGroup"
BarComparisonFigure(theData, theData$RatingGroup, theData$CorrContextAccuracy, "Curiosity Rating", "Mean corrected hit rate", figName)

theData <- Curiosity.Recall.Old.Novelty[!SubjectNo %in% bad.ps.less.strict]
figName <- "OutsideSourceCorrSeenHitLinearNoveltyGroups"
LinearTrendTwoGroups(theData, theData$ItemOrder, theData$CorrContextAccuracy, theData$Group, "Familiar Rooms", "Novel Rooms", "Item Order", "Corrected Hit Rate", figName)

theData <- Curiosity.Recall.Old.CuriosityType.MeanSep[!SubjectNo %in% bad.ps.less.strict]
figName <- "OutsideSourceCorrSeenHitLinearCuriosityGroups"
LinearTrendTwoGroups(theData, theData$ItemOrder, theData$CorrContextAccuracy, theData$RatingGroup, "Low Curiosity", "High Curiosity", "Item Order", "Corrected Hit Rate", figName)

## Inside
### Item
theData <- InsideObjectsMemory[!SubjectNo %in% bad.ps.less.strict, .(MeanCorrSeenHit = mean(CorrSeenHit)), by = c("SubjectNo", "Group")]
figName <- "InsideItemCorrSeenHitNovelty"
BarComparisonFigure(theData, theData$Group, theData$MeanCorrSeenHit, "Room Novelty", "Mean corrected hit rate", figName)

theData <- Encoding.Recall.Old.Collapsed.CuriosityType.MeanSep[!SubjectNo %in% bad.ps.less.strict]
figName <- "InsideItemCorrSeenHitCuriosityGroup"
BarComparisonFigure(theData, theData$RatingGroup, theData$CorrSeenHit, "Curiosity Rating", "Mean corrected hit rate", figName)

theData <- Encoding.Recall.Old.Novelty[!SubjectNo %in% bad.ps.less.strict]
figName <- "InsideItemCorrSeenHitLinearNoveltyGroups"
LinearTrendTwoGroups(theData, theData$ItemOrder, theData$CorrSeenHit, theData$Group, "Familiar Rooms", "Novel Rooms", "Item Order", "Corrected Hit Rate", figName)

theData <- Encoding.Recall.Old.CuriosityType.MeanSep[!SubjectNo %in% bad.ps.less.strict]
figName <- "InsideItemCorrSeenHitLinearCuriosityGroups"
LinearTrendTwoGroups(theData, theData$ItemOrder, theData$CorrSeenHit, theData$RatingGroup, "Low Curiosity", "High Curiosity", "Item Order", "Corrected Hit Rate", figName)

### Source
theData <- InsideObjectsMemory[!SubjectNo %in% bad.ps.less.strict, .(MeanCorrSeenHit = mean(CorrContextAccuracy)), by = c("SubjectNo", "Group")]
figName <- "InsideSourceCorrSeenHitNovelty"
BarComparisonFigure(theData, theData$Group, theData$MeanCorrSeenHit, "Room Novelty", "Mean corrected hit rate", figName)

theData <- Encoding.Recall.Old.Collapsed.CuriosityType.MeanSep[!SubjectNo %in% bad.ps.less.strict]
figName <- "InsideSourceCorrSeenHitCuriosityGroup"
BarComparisonFigure(theData, theData$RatingGroup, theData$CorrContextAccuracy, "Curiosity Rating", "Mean corrected hit rate", figName)

theData <- Encoding.Recall.Old.Novelty[!SubjectNo %in% bad.ps.less.strict]
figName <- "InsideSourceCorrSeenHitLinearNoveltyGroups"
LinearTrendTwoGroups(theData, theData$ItemOrder, theData$CorrContextAccuracy, theData$Group, "Familiar Rooms", "Novel Rooms", "Item Order", "Corrected Hit Rate", figName)

theData <- Encoding.Recall.Old.CuriosityType.MeanSep[!SubjectNo %in% bad.ps.less.strict]
figName <- "InsideSourceCorrSeenHitLinearCuriosityGroups"
LinearTrendTwoGroups(theData, theData$ItemOrder, theData$CorrContextAccuracy, theData$RatingGroup, "Low Curiosity", "High Curiosity", "Item Order", "Corrected Hit Rate", figName)