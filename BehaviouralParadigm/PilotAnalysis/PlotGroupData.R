library(data.table); library(ggplot2); library(ggsci); library(cowplot); library(reshape2)

source("./PilotAnalysis/PlotFuncs.R")

excluded.ps <- c("P19", "P20")

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
