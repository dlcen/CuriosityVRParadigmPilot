# This code is to draw figures for presentation or publication

library(ggplot2); library(data.table); library(cowplot)

setwd("D:/OneDirve_CU/OneDrive - Cardiff University/Projects/VRCuriosityMemory/PilotData/RawData/Summary/")

load("D:/OneDirve_CU/OneDrive - Cardiff University/Projects/VRCuriosityMemory/PilotData/RawData/Summary/Summary.RData")

# Function for drawing bar figure to compare the difference between conditions
BarComparisonFigure <- function(data, iv, dv, xlabel, ylabel, figureName, yUpLim = 0.6) {
	ggplot(data, aes(x = iv, y = dv)) + theme_classic() +
  		stat_summary(fun.y = mean, geom = "bar", aes(fill = iv)) +
  		stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  		scale_fill_manual(values = c("grey55", "#cc0033")) +
  		coord_cartesian(ylim = c(0, yUpLim)) +
  		labs(x = xlabel, y = ylabel) +
  		facet_wrap( ~ Version) +
  		theme(legend.position = "none",
  			  title = element_text(face = "bold", size = 14),
              strip.text = element_text(face = "bold", size = 14),
              strip.background.y = element_rect(size = 12),
              strip.background = element_rect(colour = NA),
              axis.title = element_text(size = 12),
              axis.text = element_text(size = 10))

  	ggplot2::ggsave(paste0("Figures/", figureName, ".png"), width=16, height=12, dpi = 300, units = "cm") 

}

# Function for drawing linear trend respectively for two groups
LinearTrendTwoGroups <- function(data, iv, dv, groupType, group1, group2, xlabel, ylabel, figureName, yUpLim = 0.6) {
	ggplot(data, aes(x = iv, y = dv)) + theme_classic() +
	  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0, aes(group = groupType, colour = groupType), position = position_dodge(width = 0.25), size = 0.25, alpha = 0.5) +
	  stat_summary(fun.y = mean, geom = "point", aes(group = groupType, colour = groupType), position = position_dodge(width = 0.25), size = 2, alpha = 0.75) +
	  stat_smooth(method = "lm", formula = y ~ x, se = FALSE, aes(group = groupType, colour = groupType), size = 1.5) +
	  labs(x = xlabel, y = ylabel) +
	  scale_x_continuous(breaks = c(1:6)) +
	  scale_color_manual(values = c("grey45", "#cc0033"), labels = c(group1, group2)) +
	  coord_cartesian(ylim = c(0, yUpLim)) +
	  facet_wrap( ~ Version) +
	  theme(legend.title = element_blank(),
	  		legend.position = c(0.85, 0.9),
	  		title = element_text(face = "bold", size = 14),
            strip.text = element_text(face = "bold", size = 14),
            strip.background.y = element_rect(size = 12),
            strip.background = element_rect(colour = NA),
	        axis.title = element_text(size = 12),
	        axis.text = element_text(size = 10))

	ggplot2::ggsave(paste0("Figures/", figureName, ".png"), width=16, height=12, dpi = 300, units = "cm") 
}

# Drawing
## Outside
### Item
theData <- OutsideObjectsMemory[!SubjectNo %in% bad.ps.less.strict, .(MeanCorrSeenHit = mean(CorrSeenHit)), by = c("SubjectNo", "Group", "Version")]
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
theData <- OutsideObjectsMemory[!SubjectNo %in% bad.ps.less.strict, .(MeanCorrSeenHit = mean(CorrContextAccuracy)), by = c("SubjectNo", "Group", "Version")]
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
yMax <- 1
### Item
theData <- InsideObjectsMemory[!SubjectNo %in% bad.ps.less.strict, .(MeanCorrSeenHit = mean(CorrSeenHit)), by = c("SubjectNo", "Group", "Version")]
figName <- "InsideItemCorrSeenHitNovelty"
BarComparisonFigure(theData, theData$Group, theData$MeanCorrSeenHit, "Room Novelty", "Mean corrected hit rate", figName, yMax)

theData <- Encoding.Recall.Old.Collapsed.CuriosityType.MeanSep[!SubjectNo %in% bad.ps.less.strict]
figName <- "InsideItemCorrSeenHitCuriosityGroup"
BarComparisonFigure(theData, theData$RatingGroup, theData$CorrSeenHit, "Curiosity Rating", "Mean corrected hit rate", figName, yMax)

theData <- Encoding.Recall.Old.Novelty[!SubjectNo %in% bad.ps.less.strict]
figName <- "InsideItemCorrSeenHitLinearNoveltyGroups"
LinearTrendTwoGroups(theData, theData$ItemOrder, theData$CorrSeenHit, theData$Group, "Familiar Rooms", "Novel Rooms", "Item Order", "Corrected Hit Rate", figName, yMax)

theData <- Encoding.Recall.Old.CuriosityType.MeanSep[!SubjectNo %in% bad.ps.less.strict]
figName <- "InsideItemCorrSeenHitLinearCuriosityGroups"
LinearTrendTwoGroups(theData, theData$ItemOrder, theData$CorrSeenHit, theData$RatingGroup, "Low Curiosity", "High Curiosity", "Item Order", "Corrected Hit Rate", figName, yMax)

### Source
theData <- InsideObjectsMemory[!SubjectNo %in% bad.ps.less.strict, .(MeanCorrSeenHit = mean(CorrContextAccuracy)), by = c("SubjectNo", "Group", "Version")]
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



## Draw collapsed data with participants who had negative hit rates were excluded.
setwd("D:/OneDirve_CU/OneDrive - Cardiff University/Projects/VRCuriosityMemory/PilotData/RawData/Summary")
load("D:/OneDirve_CU/OneDrive - Cardiff University/Projects/VRCuriosityMemory/PilotData/RawData/Summary/Summary.RData")

# Function for drawing bar figure to compare the difference between Early vs. Late and the other two conditions.
BarCollapsedOrderComparisonFigure <- function(data, iv, dv, xlabel, ylabel, figureName, yUpLim = 0.6) {
  ggplot(data, aes_string(x = iv, y = dv)) + theme_classic() +
      geom_hline(yintercept = 0, size = 0.25) +
      stat_summary(fun.y = mean, geom = "bar", aes_string(fill = iv)) +
      stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, position = position_dodge(width = 0.9)) +
      scale_fill_manual(values = c("grey55", "#cc0033")) +
      coord_cartesian(ylim = c(0, yUpLim)) +
      labs(x = xlabel, y = ylabel) +
      facet_grid( Version ~ ItemOrderType ) +
      theme(legend.position = "none",
            title = element_text(face = "bold", size = 14),
            strip.text = element_text(face = "bold", size = 14),
            strip.background.y = element_rect(size = 12),
            strip.background = element_rect(colour = NA),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10))

    ggplot2::ggsave(paste0("Figures/", figureName, ".png"), width=16, height=16, dpi = 300, units = "cm") 

}

## Outside
### Item
theData <- Curiosity.Recall.Old.Order.Novelty[!SubjectNo %in% bad.ps.less.strict]
theData[is.na(CorrSeenHit)]

figName <- "OutsideItemCorrSeenHitNovelty_Excluded"
BarCollapsedOrderComparisonFigure(theData, "Group", "CorrSeenHit", "Room Novelty", "Mean corrected hit rate", figName)

theData <- Curiosity.Recall.Old.Order.CuriosityType.MeanSep[!SubjectNo %in% bad.ps.less.strict]
theData[is.na(CorrSeenHit)]

figName <- "OutsideItemCorrSeenHitCuriosityGroup_Excluded"
BarCollapsedOrderComparisonFigure(theData, "RatingGroup", "CorrSeenHit", "Curiosity Rating", "Mean corrected hit rate", figName)

### Source
theData <- Curiosity.Recall.Old.Order.Novelty[!SubjectNo %in% bad.ps.less.strict]
theData[is.na(CorrContextAccuracy)]

figName <- "OutsideSourceCorrSeenHitNovelty_Excluded"
BarCollapsedOrderComparisonFigure(theData, "Group", "CorrContextAccuracy", "Room Novelty", "Mean corrected hit rate", figName)

theData <- Curiosity.Recall.Old.Order.CuriosityType.MeanSep[!SubjectNo %in% bad.ps.less.strict]
theData[is.na(CorrContextAccuracy)]

figName <- "OutsideSourceCorrSeenHitCuriosityGroup_Excluded"
BarCollapsedOrderComparisonFigure(theData, "RatingGroup", "CorrContextAccuracy", "Curiosity Rating", "Mean corrected hit rate", figName)


## Inside
### Item
yMax <- 1
theData <- Encoding.Recall.Old.Order.Novelty[!SubjectNo %in% bad.ps.less.strict]
theData[is.na(CorrSeenHit)]

figName <- "InsideItemCorrSeenHitNovelty_Excluded"
BarCollapsedOrderComparisonFigure(theData, "Group", "CorrSeenHit", "Room Novelty", "Mean corrected hit rate", figName, yMax)

theData <- Encoding.Recall.Old.Order.CuriosityType.MeanSep[!SubjectNo %in% bad.ps.less.strict]
theData[is.na(CorrSeenHit)]

figName <- "InsideItemCorrSeenHitCuriosityGroup_Excluded"
BarCollapsedOrderComparisonFigure(theData, "RatingGroup", "CorrSeenHit", "Curiosity Rating", "Mean corrected hit rate", figName, yMax)

### Source
theData <- Encoding.Recall.Old.Order.Novelty[!SubjectNo %in% bad.ps.less.strict]
theData[is.na(CorrContextAccuracy)]

figName <- "InsideSourceCorrSeenHitNovelty_Excluded"
BarCollapsedOrderComparisonFigure(theData, "Group", "CorrContextAccuracy", "Room Novelty", "Mean corrected hit rate", figName)

theData <- Encoding.Recall.Old.Order.CuriosityType.MeanSep[!SubjectNo %in% bad.ps.less.strict]
theData[is.na(CorrContextAccuracy)]

figName <- "InsideSourceCorrSeenHitCuriosityGroup_Excluded"
BarCollapsedOrderComparisonFigure(theData, "RatingGroup", "CorrContextAccuracy", "Curiosity Rating", "Mean corrected hit rate", figName)