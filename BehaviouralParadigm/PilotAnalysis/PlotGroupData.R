library(data.table); library(ggplot2); library(ggsci); library(cowplot); library(reshape2)

source("./PilotAnalysis/PlotFuncs.R")

# Plot the comparison between median-split groups
## Plot comparison between curiosity median-split groups
BarGrpPlotTog(data = outside.hit.rate.item.curiosity.median[!SubjectNo %in% excluded.ps], group = "CurGrpMd", xlab = "Curiosity median-split group", figname = "GroupCurMedian")

## Plot comparison between interest median-split groups
BarGrpPlotTog(data = outside.hit.rate.item.interest.median[!SubjectNo %in% excluded.ps], group = "IntGrpMd", xlab = "interest median-split group", figname = "GroupIntMedian")

## Plot comparison between surprise median-split groups
BarGrpPlotTog(data = outside.hit.rate.item.surprise.median[!SubjectNo %in% excluded.ps], group = "SurGrpMd", xlab = "Surprise median-split group", figname = "GroupSurMedian")

## Plot comparison between exploration time median-split groups
BarGrpPlotTog(data = outside.hit.rate.item.exploration.median[!SubjectNo %in% excluded.ps], group = "DurGrpMd", xlab = "Exploration time median-split group", figname = "GroupDurMedian", newlevels = c("Short", "Long"))

## Plot comparison between interest (for previous rooms) median-split groups
BarGrpPlotTog(data = outside.hit.rate.item.preint.median[!SubjectNo %in% excluded.ps], group = "PreIntGrpMd", xlab = "Interest (for the previous room) median-split group", figname = "GroupPreIntMedian")

## Plot comparison between surprise (for previous rooms) median-split groups
BarGrpPlotTog(data = outside.hit.rate.item.presur.median[!SubjectNo %in% excluded.ps], group = "PreSurGrpMd", xlab = "Surprise (for the previous room) median-split group", figname = "GroupPreSurMedian")

# Include the comparison between "Early" and "Later" parts
## Plot comparison between curiosity median-split groups
BarOrdGrpPlotTog(data = outside.hit.rate.item.order.curiosity.median[!SubjectNo %in% excluded.ps], group = "CurGrpMd", legendgroup = "Curiosity", figname = "GroupOrdCurMedian")

## Plot comparison between interest median-split groups
BarOrdGrpPlotTog(data = outside.hit.rate.item.order.interest.median[!SubjectNo %in% excluded.ps], group = "IntGrpMd", legendgroup = "interest", figname = "GroupOrdIntMedian")

## Plot comparison between surprise median-split groups
BarOrdGrpPlotTog(data = outside.hit.rate.item.order.surprise.median[!SubjectNo %in% excluded.ps], group = "SurGrpMd", legendgroup = "Surprise", figname = "GroupOrdSurMedian")

## Plot comparison between exploration time median-split groups
BarOrdGrpPlotTog(data = outside.hit.rate.item.order.exploration.median[!SubjectNo %in% excluded.ps], group = "DurGrpMd", legendgroup = "Exploration time", figname = "GroupOrdDurMedian", newlevels = c("Short", "Long"))

## Plot comparison between interest (for previous rooms) median-split groups
BarOrdGrpPlotTog(data = outside.hit.rate.item.order.preint.median[!SubjectNo %in% excluded.ps], group = "PreIntGrpMd", legendgroup = "Previous Interest", figname = "GroupOrdPreIntMedian")

## Plot comparison between surprise (for previous rooms) median-split groups
BarOrdGrpPlotTog(data = outside.hit.rate.item.order.presur.median[!SubjectNo %in% excluded.ps], group = "PreSurGrpMd", legendgroup = "Previous Surprise", figname = "GroupOrdPreSurMedian")

# Plot the relationship between trait curiosity and ratings as well as exploration times
## Curiosity ratings
PC.cur.plt <- RgLineIdvPlot(individual.average.data[!SubjectNo %in% excluded.ps], iv="PC", dv = "MeanCur", xlab="PC score", ylab="Mean curiosity rating", xtxt=mean(individual.average.data$PC), ytxt=ceiling(max(individual.average.data$MeanCur))+0.5, xbrk=seq(2, 4, 0.25), xlims=c(2, 4), ylims=c(floor(min(individual.average.data$MeanCur)), ceiling(max(individual.average.data$MeanCur)) + 1), is.individual = FALSE)
EC.cur.plt <- RgLineIdvPlot(individual.average.data[!SubjectNo %in% excluded.ps], iv="EC", dv = "MeanCur", xlab="EC score", ylab="Mean curiosity rating", xtxt=mean(individual.average.data$EC), ytxt=ceiling(max(individual.average.data$MeanCur))+0.5, xbrk=seq(2, 4, 0.25), xlims=c(2, 4), ylims=c(floor(min(individual.average.data$MeanCur)), ceiling(max(individual.average.data$MeanCur)) + 1), is.individual = FALSE)
GN.cur.plt <- RgLineIdvPlot(individual.average.data[!SubjectNo %in% excluded.ps], iv="GN", dv = "MeanCur", xlab="GN score", ylab="Mean curiosity rating", xtxt=mean(individual.average.data$GN), ytxt=ceiling(max(individual.average.data$MeanCur))+0.5, xbrk=seq(2, 4, 0.25), xlims=c(2, 4), ylims=c(floor(min(individual.average.data$MeanCur)), ceiling(max(individual.average.data$MeanCur)) + 1), is.individual = FALSE)

plot_grid(PC.cur.plt, EC.cur.plt + theme(axis.title.y = element_blank()), GN.cur.plt + theme(axis.title.y = element_blank()), nrow = 1, rel_widths = c(1.05, 1, 1))

ggsave("./Figures/TraitCuriosityRatingLinear.png", width = 15, height = 5)


## Exploration time
PC.dur.plt <- RgLineIdvPlot(individual.average.data[!SubjectNo %in% excluded.ps], iv="PC", dv = "MeanDur", xlab="PC score", ylab="Mean exploration time", xtxt=mean(individual.average.data$PC), ytxt=ceiling(max(individual.average.data$MeanDur))+0.5, xbrk=seq(2, 4, 0.25), xlims=c(2, 4), ylims=c(floor(min(individual.average.data$MeanDur)), ceiling(max(individual.average.data$MeanDur)) + 1), is.individual = FALSE)
EC.dur.plt <- RgLineIdvPlot(individual.average.data[!SubjectNo %in% excluded.ps], iv="EC", dv = "MeanDur", xlab="EC score", ylab="Mean exploration time", xtxt=mean(individual.average.data$EC), ytxt=ceiling(max(individual.average.data$MeanDur))+0.5, xbrk=seq(2, 4, 0.25), xlims=c(2, 4), ylims=c(floor(min(individual.average.data$MeanDur)), ceiling(max(individual.average.data$MeanDur)) + 1), is.individual = FALSE)
GN.dur.plt <- RgLineIdvPlot(individual.average.data[!SubjectNo %in% excluded.ps], iv="GN", dv = "MeanDur", xlab="GN score", ylab="Mean exploration time", xtxt=mean(individual.average.data$GN), ytxt=ceiling(max(individual.average.data$MeanDur))+0.5, xbrk=seq(2, 4, 0.25), xlims=c(2, 4), ylims=c(floor(min(individual.average.data$MeanDur)), ceiling(max(individual.average.data$MeanDur)) + 1), is.individual = FALSE)

plot_grid(PC.dur.plt, EC.dur.plt + theme(axis.title.y = element_blank()), GN.dur.plt + theme(axis.title.y = element_blank()), nrow = 1, rel_widths = c(1.05, 1, 1))

ggsave("./Figures/TraitExplorationTimeLinear.png", width = 15, height = 5)


## Scores and memory performance in general
PC.memory <- RgLineIdvPlot(individual.average.data[!SubjectNo %in% excluded.ps], iv="PC", dv = "MSAcc", xlab="PC score", ylab="Average corrected hit rate", xtxt=mean(individual.average.data$PC), ytxt=ceiling(max(individual.average.data$MSAcc))+0.5, xbrk=seq(2, 4, 0.25), xlims=c(2, 4), ylims=c(floor(min(individual.average.data$MSAcc)), ceiling(max(individual.average.data$MSAcc)) + 1), is.individual = FALSE)
EC.memory <- RgLineIdvPlot(individual.average.data[!SubjectNo %in% excluded.ps], iv="EC", dv = "MSAcc", xlab="EC score", ylab="Average corrected hit rate", xtxt=mean(individual.average.data$EC), ytxt=ceiling(max(individual.average.data$MSAcc))+0.5, xbrk=seq(2, 4, 0.25), xlims=c(2, 4), ylims=c(floor(min(individual.average.data$MSAcc)), ceiling(max(individual.average.data$MSAcc)) + 1), is.individual = FALSE)
GN.memory <- RgLineIdvPlot(individual.average.data[!SubjectNo %in% excluded.ps], iv="GN", dv = "MSAcc", xlab="GN score", ylab="Average corrected hit rate", xtxt=mean(individual.average.data$GN), ytxt=ceiling(max(individual.average.data$MSAcc))+0.5, xbrk=seq(2, 4, 0.25), xlims=c(2, 4), ylims=c(floor(min(individual.average.data$MSAcc)), ceiling(max(individual.average.data$MSAcc)) + 1), is.individual = FALSE)

plot_grid(PC.memory, EC.memory + theme(axis.title.y = element_blank()), GN.memory + theme(axis.title.y = element_blank()), nrow = 1, rel_widths = c(1.05, 1, 1))

ggsave("./Figures/TraitMemoryLinear.png", width = 15, height = 5)


## Scores and memory enhancement
### PC score
PC.CurME <- RgLineIdvPlot(individual.average.data[!SubjectNo %in% excluded.ps], iv="PC", dv = "CurME", xlab="PC score", ylab="Curiosity-related memory enhancement", xtxt=mean(individual.average.data$PC), ytxt=ceiling(max(individual.average.data$CurME))+0.5, xbrk=seq(2, 4, 0.25), xlims=c(2, 4), ylims=c(floor(min(individual.average.data$CurME)), ceiling(max(individual.average.data$CurME)) + 1), is.individual = FALSE)
PC.IntME <- RgLineIdvPlot(individual.average.data[!SubjectNo %in% excluded.ps], iv="PC", dv = "IntME", xlab="PC score", ylab="Interest-related memory enhancement",  xtxt=mean(individual.average.data$PC), ytxt=ceiling(max(individual.average.data$IntME))+0.5, xbrk=seq(2, 4, 0.25), xlims=c(2, 4), ylims=c(floor(min(individual.average.data$IntME)), ceiling(max(individual.average.data$IntME)) + 1), is.individual = FALSE)
PC.SurME <- RgLineIdvPlot(individual.average.data[!SubjectNo %in% excluded.ps], iv="PC", dv = "SurME", xlab="PC score", ylab="Surprise-related memory enhancement",  xtxt=mean(individual.average.data$PC), ytxt=ceiling(max(individual.average.data$SurME))+0.5, xbrk=seq(2, 4, 0.25), xlims=c(2, 4), ylims=c(floor(min(individual.average.data$SurME)), ceiling(max(individual.average.data$SurME)) + 1), is.individual = FALSE)

plot_grid(PC.CurME, PC.IntME, PC.SurME, nrow = 1, rel_widths = c(1.05, 1, 1))

ggsave("./Figures/PCMELinear.png", width = 15, height = 5)

### EC score
EC.CurME <- RgLineIdvPlot(individual.average.data[!SubjectNo %in% excluded.ps], iv="EC", dv = "CurME", xlab="EC score", ylab="Curiosity-related memory enhancement", xtxt=mean(individual.average.data$EC), ytxt=ceiling(max(individual.average.data$CurME))+0.5, xbrk=seq(2, 4, 0.25), xlims=c(2, 4), ylims=c(floor(min(individual.average.data$CurME)), ceiling(max(individual.average.data$CurME)) + 1), is.individual = FALSE)
EC.IntME <- RgLineIdvPlot(individual.average.data[!SubjectNo %in% excluded.ps], iv="EC", dv = "IntME", xlab="EC score", ylab="Interest-related memory enhancement",  xtxt=mean(individual.average.data$EC), ytxt=ceiling(max(individual.average.data$IntME))+0.5, xbrk=seq(2, 4, 0.25), xlims=c(2, 4), ylims=c(floor(min(individual.average.data$IntME)), ceiling(max(individual.average.data$IntME)) + 1), is.individual = FALSE)
EC.SurME <- RgLineIdvPlot(individual.average.data[!SubjectNo %in% excluded.ps], iv="EC", dv = "SurME", xlab="EC score", ylab="Surprise-related memory enhancement",  xtxt=mean(individual.average.data$EC), ytxt=ceiling(max(individual.average.data$SurME))+0.5, xbrk=seq(2, 4, 0.25), xlims=c(2, 4), ylims=c(floor(min(individual.average.data$SurME)), ceiling(max(individual.average.data$SurME)) + 1), is.individual = FALSE)

plot_grid(EC.CurME, EC.IntME, EC.SurME, nrow = 1, rel_widths = c(1.05, 1, 1))

ggsave("./Figures/ECMELinear.png", width = 15, height = 5)

### GN score
GN.CurME <- RgLineIdvPlot(individual.average.data[!SubjectNo %in% excluded.ps], iv="GN", dv = "CurME", xlab="GN score", ylab="Curiosity-related memory enhancement", xtxt=mean(individual.average.data$GN), ytxt=ceiling(max(individual.average.data$CurME))+0.5, xbrk=seq(2, 4, 0.25), xlims=c(2, 4), ylims=c(floor(min(individual.average.data$CurME)), ceiling(max(individual.average.data$CurME)) + 1), is.individual = FALSE)
GN.IntME <- RgLineIdvPlot(individual.average.data[!SubjectNo %in% excluded.ps], iv="GN", dv = "IntME", xlab="GN score", ylab="Interest-related memory enhancement",  xtxt=mean(individual.average.data$GN), ytxt=ceiling(max(individual.average.data$IntME))+0.5, xbrk=seq(2, 4, 0.25), xlims=c(2, 4), ylims=c(floor(min(individual.average.data$IntME)), ceiling(max(individual.average.data$IntME)) + 1), is.individual = FALSE)
GN.SurME <- RgLineIdvPlot(individual.average.data[!SubjectNo %in% excluded.ps], iv="GN", dv = "SurME", xlab="GN score", ylab="Surprise-related memory enhancement",  xtxt=mean(individual.average.data$GN), ytxt=ceiling(max(individual.average.data$SurME))+0.5, xbrk=seq(2, 4, 0.25), xlims=c(2, 4), ylims=c(floor(min(individual.average.data$SurME)), ceiling(max(individual.average.data$SurME)) + 1), is.individual = FALSE)

plot_grid(GN.CurME, GN.IntME, GN.SurME, nrow = 1, rel_widths = c(1.05, 1, 1))

ggsave("./Figures/GNMELinear.png", width = 15, height = 5)

## Scores and memory enhancement (pre-effects)
### PC score
PC.PreIntME <- RgLineIdvPlot(individual.average.data[!SubjectNo %in% excluded.ps], iv="PC", dv = "PreIntME", xlab="PC score", ylab="Pre-Interest-related memory enhancement",  xtxt=mean(individual.average.data$PC), ytxt=ceiling(max(individual.average.data$PreIntME))+0.5, xbrk=seq(2, 4, 0.25), xlims=c(2, 4), ylims=c(floor(min(individual.average.data$PreIntME)), ceiling(max(individual.average.data$PreIntME)) + 1), is.individual = FALSE)
PC.PreSurME <- RgLineIdvPlot(individual.average.data[!SubjectNo %in% excluded.ps], iv="PC", dv = "PreSurME", xlab="PC score", ylab="Pre-Surprise-related memory enhancement",  xtxt=mean(individual.average.data$PC), ytxt=ceiling(max(individual.average.data$PreSurME, na.rm = T))+0.5, xbrk=seq(2, 4, 0.25), xlims=c(2, 4), ylims=c(floor(min(individual.average.data$PreSurME, na.rm = T)), ceiling(max(individual.average.data$PreSurME, na.rm = T, na.rm = T)) + 1), is.individual = FALSE)

plot_grid(PC.PreIntME, PC.PreSurME, nrow = 1)

ggsave("./Figures/PCMELinear.png", width = 15, height = 5)

### EC score
EC.PreIntME <- RgLineIdvPlot(individual.average.data[!SubjectNo %in% excluded.ps], iv="EC", dv = "PreIntME", xlab="EC score", ylab="Pre-Interest-related memory enhancement",  xtxt=mean(individual.average.data$EC), ytxt=ceiling(max(individual.average.data$PreIntME))+0.5, xbrk=seq(2, 4, 0.25), xlims=c(2, 4), ylims=c(floor(min(individual.average.data$PreIntME)), ceiling(max(individual.average.data$PreIntME)) + 1), is.individual = FALSE)
EC.PreSurME <- RgLineIdvPlot(individual.average.data[!SubjectNo %in% excluded.ps], iv="EC", dv = "PreSurME", xlab="EC score", ylab="Pre-Surprise-related memory enhancement",  xtxt=mean(individual.average.data$EC), ytxt=ceiling(max(individual.average.data$PreSurME, na.rm = T))+0.5, xbrk=seq(2, 4, 0.25), xlims=c(2, 4), ylims=c(floor(min(individual.average.data$PreSurME, na.rm = T)), ceiling(max(individual.average.data$PreSurME, na.rm = T)) + 1), is.individual = FALSE)

plot_grid(EC.PreIntME, EC.PreSurME, nrow = 1)

ggsave("./Figures/ECMELinear.png", width = 15, height = 5)

### GN score
GN.PreIntME <- RgLineIdvPlot(individual.average.data[!SubjectNo %in% excluded.ps], iv="GN", dv = "PreIntME", xlab="GN score", ylab="Pre-Interest-related memory enhancement",  xtxt=mean(individual.average.data$GN), ytxt=ceiling(max(individual.average.data$PreIntME))+0.5, xbrk=seq(2, 4, 0.25), xlims=c(2, 4), ylims=c(floor(min(individual.average.data$PreIntME)), ceiling(max(individual.average.data$PreIntME)) + 1), is.individual = FALSE)
GN.PreSurME <- RgLineIdvPlot(individual.average.data[!SubjectNo %in% excluded.ps], iv="GN", dv = "PreSurME", xlab="GN score", ylab="Pre-Surprise-related memory enhancement",  xtxt=mean(individual.average.data$GN), ytxt=ceiling(max(individual.average.data$PreSurME, na.rm = T))+0.5, xbrk=seq(2, 4, 0.25), xlims=c(2, 4), ylims=c(floor(min(individual.average.data$PreSurME, na.rm = T)), ceiling(max(individual.average.data$PreSurME, na.rm = T)) + 1), is.individual = FALSE)

plot_grid(GN.PreIntME, GN.PreSurME, nrow = 1)

ggsave("./Figures/GNMELinear.png", width = 15, height = 5)

# Plot distribution of time spent on the pathway outside the rooms and exploring the rooms
ggplot(individual.data[!SubjectNo %in% excluded.ps], aes(OutsideDuration, group = SubjectNo)) +
	geom_histogram(binwidth = 1, aes(color = SubjectNo, fill = SubjectNo)) +
	labs(x = "Time on pathway (s)")
 
ggsave("./Figures/PathwayTimeDistri.png", width = 8, height = 6)

ggplot(individual.data[!SubjectNo %in% excluded.ps], aes(InsideDuration, group = SubjectNo)) +
	geom_histogram(binwidth = 5, aes(color = SubjectNo, fill = SubjectNo)) +
	labs(x = "Exploration time (s)")
 
ggsave("./Figures/ExpTimeDistri.png", width = 8, height = 6)

ggplot(individual.average.data[!SubjectNo %in% excluded.ps], aes(MeanDur, group = SubjectNo)) +
	geom_histogram(binwidth = 5, aes(color = SubjectNo, fill = SubjectNo)) +
	labs(x = "Exploration time (s)")
 
ggsave("./Figures/AverageExpTimeDistri.png", width = 8, height = 6)

# Check the corrected hit rate as a function of room order
RgLineIdvPlot(outside.hit.rate.per.room, iv="Order", xlab="Room order", xtxt=8, xbrk=seq(1, 16, 1), xlims=c(1, 16))

ggsave("./Figures/OrderMemory.png", width = 4, height = 24)
