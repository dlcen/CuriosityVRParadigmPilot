library(data.table); library(ggplot2); library(ggsci); library(cowplot); library(reshape2)

source("./PilotAnalysis/PlotFuncs.R")

excluded.ps <- c("P19", "P20")

# Plot comparison between curiosity median-split groups
BarGrpPlotTog(data = outside.hit.rate.item.curiosity.median[!SubjectNo %in% excluded.ps], group = "CurGrpMd", xlab = "Curiosity median-split group", figname = "GroupCurMedian")

# Plot comparison between interest median-split groups
BarGrpPlotTog(data = outside.hit.rate.item.interest.median[!SubjectNo %in% excluded.ps], group = "IntGrpMd", xlab = "interest median-split group", figname = "GroupIntMedian")

# Plot comparison between surprise median-split groups
BarGrpPlotTog(data = outside.hit.rate.item.surprise.median[!SubjectNo %in% excluded.ps], group = "SurGrpMd", xlab = "Surprise median-split group", figname = "GroupSurMedian")

# Plot comparison between exploration time median-split groups
BarGrpPlotTog(data = outside.hit.rate.item.exploration.median[!SubjectNo %in% excluded.ps], group = "DurGrpMd", xlab = "Exploration time median-split group", figname = "GroupDurMedian", newlevels = c("Short", "Long"))

