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
PC.cur.plt <- RgLineIdvPlot(individual.average.data, iv="PC", dv = "MeanCur", xlab="PC score", ylab="Mean curiosity rating", xtxt=mean(individual.average.data$PC), ytxt=ceiling(max(individual.average.data$MeanCur))+0.5, xbrk=seq(2, 4, 0.25), xlims=c(2, 4), ylims=c(floor(min(individual.average.data$MeanCur)), ceiling(max(individual.average.data$MeanCur)) + 1), is.individual = FALSE)
EC.cur.plt <- RgLineIdvPlot(individual.average.data, iv="EC", dv = "MeanCur", xlab="EC score", ylab="Mean curiosity rating", xtxt=mean(individual.average.data$EC), ytxt=ceiling(max(individual.average.data$MeanCur))+0.5, xbrk=seq(2, 4, 0.25), xlims=c(2, 4), ylims=c(floor(min(individual.average.data$MeanCur)), ceiling(max(individual.average.data$MeanCur)) + 1), is.individual = FALSE)
GN.cur.plt <- RgLineIdvPlot(individual.average.data, iv="GN", dv = "MeanCur", xlab="GN score", ylab="Mean curiosity rating", xtxt=mean(individual.average.data$GN), ytxt=ceiling(max(individual.average.data$MeanCur))+0.5, xbrk=seq(2, 4, 0.25), xlims=c(2, 4), ylims=c(floor(min(individual.average.data$MeanCur)), ceiling(max(individual.average.data$MeanCur)) + 1), is.individual = FALSE)

plot_grid(PC.cur.plt, EC.cur.plt + theme(axis.title.y = element_blank()), GN.cur.plt + theme(axis.title.y = element_blank()), nrow = 1, rel_widths = c(1.05, 1, 1))

ggsave("./Figures/TraitCuriosityRatingLinear.png", width = 15, height = 5)


## Exploration time
PC.dur.plt <- RgLineIdvPlot(individual.average.data, iv="PC", dv = "MeanDur", xlab="PC score", ylab="Mean exploration time", xtxt=mean(individual.average.data$PC), ytxt=ceiling(max(individual.average.data$MeanDur))+0.5, xbrk=seq(2, 4, 0.25), xlims=c(2, 4), ylims=c(floor(min(individual.average.data$MeanDur)), ceiling(max(individual.average.data$MeanDur)) + 1), is.individual = FALSE)
EC.dur.plt <- RgLineIdvPlot(individual.average.data, iv="EC", dv = "MeanDur", xlab="EC score", ylab="Mean exploration time", xtxt=mean(individual.average.data$EC), ytxt=ceiling(max(individual.average.data$MeanDur))+0.5, xbrk=seq(2, 4, 0.25), xlims=c(2, 4), ylims=c(floor(min(individual.average.data$MeanDur)), ceiling(max(individual.average.data$MeanDur)) + 1), is.individual = FALSE)
GN.dur.plt <- RgLineIdvPlot(individual.average.data, iv="GN", dv = "MeanDur", xlab="GN score", ylab="Mean exploration time", xtxt=mean(individual.average.data$GN), ytxt=ceiling(max(individual.average.data$MeanDur))+0.5, xbrk=seq(2, 4, 0.25), xlims=c(2, 4), ylims=c(floor(min(individual.average.data$MeanDur)), ceiling(max(individual.average.data$MeanDur)) + 1), is.individual = FALSE)

plot_grid(PC.dur.plt, EC.dur.plt + theme(axis.title.y = element_blank()), GN.dur.plt + theme(axis.title.y = element_blank()), nrow = 1, rel_widths = c(1.05, 1, 1))

ggsave("./Figures/TraitExplorationTimeLinear.png", width = 15, height = 5)


