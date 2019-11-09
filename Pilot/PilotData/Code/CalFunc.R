# Accuracy can be calculated as choosing "Seen" or "Familiar" to seen objects
AccuracyCal <- function(ActualResp, CorrResp){
	if (CorrResp == "Seen" & ActualResp %in% c("Seen", "Familiar")) {
		Accuracy <- 1
	} else if (CorrResp == "New" & ActualResp == "New") {
		Accuracy <- 1
	} else {
		Accuracy <- 0
	}	

	return(Accuracy)
}

# Accuracy can be calculated as only choosing "Seen" to seen objects
SeenAccuracyCal <- function(ActualResp, CorrResp){
  if (CorrResp == "Seen" & ActualResp == "Seen") {
    Accuracy <- 1
  } else if (CorrResp == "New" & ActualResp == "New") {
    Accuracy <- 1
  } else {
    Accuracy <- 0
  } 
  
  return(Accuracy)
}

FalseAlarmCal <- function(ActualResp, CorrResp){
  if (CorrResp == 'New' & ActualResp %in% c('Seen', 'Familiar')) {
    FalseSeen <- 1
  } else {
    FalseSeen <- 0
  }
}

FalseSeenHitCal <- function(ActualResp, CorrResp){
  if (CorrResp == 'New' & ActualResp == 'Seen') {
    FalseSeen <- 1
  } else {
    FalseSeen <- 0
  }
}

# Context memory
ContextAccuracyCal <- function(ActualResp, CorrResp) {
  if (ActualResp == CorrResp) {
    Accuracy = 1
  } else {
    Accuracy = 0
  }
  
  return(Accuracy)
}

ContextFalseHitCal <- function(ActualResp, CorrResp) {
  if (CorrResp == "None" & ActualResp != "None") {
      FalseHit = 1}
  else {
      FalseHit = 0
  }
}