# Hit can be calculated as choosing "Seen" or "Familiar" to seen objects
SFHitCal <- function(ActualResp, CorrResp){
	if (CorrResp == "Seen" & ActualResp %in% c("Seen", "Familiar")) {
		Hit <- 1
	} else if (CorrResp == "New" & ActualResp == "New") {
		Hit <- 1
	} else {
		Hit <- 0
	}	

	return(Hit)
}

# Hit can be calculated as only choosing "Seen" to seen objects
SHitCal <- function(ActualResp, CorrResp){
  if (CorrResp == "Seen" & ActualResp == "Seen") {
    Hit <- 1
  } else if (CorrResp == "New" & ActualResp == "New") {
    Hit <- 1
  } else {
    Hit <- 0
  } 
  
  return(Hit)
}

SFFalseHitCal <- function(ActualResp, CorrResp){
  if (CorrResp == 'New' & ActualResp %in% c('Seen', 'Familiar')) {
    FalseHit <- 1
  } else {
    FalseHit <- 0
  }
}

SFalseHitCal <- function(ActualResp, CorrResp){
  if (CorrResp == 'New' & ActualResp == 'Seen') {
    FalseHit <- 1
  } else {
    FalseHit <- 0
  }
}

# Source memory
SourceHitCal <- function(ActualResp, CorrResp) {
  if (ActualResp == CorrResp) {
    Hit = 1
  } else {
    Hit = 0
  }
  
  return(Hit)
}

SourceFalseHitCal <- function(ActualResp, CorrResp) {
  if (CorrResp == "None" & ActualResp != "None") {
      FalseHit = 1}
  else {
      FalseHit = 0
  }
}