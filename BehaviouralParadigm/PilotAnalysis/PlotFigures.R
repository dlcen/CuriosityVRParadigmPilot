library(data.table); library(ggplot2)

load("./PilotData/IndividualData.RData")

# Plot recognition rate for each participant
RespFreqCal <- function(ObjResp, ... ){
  FreqTable <- as.data.frame(table(ObjResp)/sum(table(ObjResp)))
  names(FreqTable) <- c("Response", "Frequency")
  return(FreqTable)
}

Recall.Freq.Rsp <- object.recognition[, c(RespFreqCal(Response)), by = c("SubjectNo", "Group")]


ggplot(Recall.Freq.Rsp, aes(x = Response, y = Frequency, color = Group, fill = Group)) +
	geom_point() +
	facet_wrap(~ SubjectNo)