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

# Label "high" and "low" curiosity groups
## According to medien
CurGrpMedianSep <- function(thisCur, medianCur) {
      
      if (is.na(thisCur)) {
            CurGrp <- NA
      } else {
            if (thisCur < medianCur) {
                  CurGrp <- "Low"
            } else if (thisCur > medianCur) {
                  CurGrp <- "High"
            } else {
                  CurGrp <- NA
            }
      }
}

## According to mean
CurGrpMeanSep <- function(thisCur, meanCur) {
      if (is.na(thisCur)) {
            CurGrp <- NA
      } else {
            if (thisCur < meanCur) {
                  CurGrp <- "Low"
            } else if (thisCur > meanCur) {
                  CurGrp <- "High"
            } else {
                  CurGrp <- NA
            }
      }
}

## According to 4
CurGrpSep <- function(thisCur) {
  if (is.na(thisCur)) {
            CurGrp <- NA
      } else {
            if (thisCur < 4) {
                  CurGrp <- "Low"
            } else if (thisCur > 4) {
                  CurGrp <- "High"
            } else {
                  CurGrp <- NA
            }
      }
}


# Label "Early" and "Later" object groups
ObjOrdGrpSep <- function(thisOrder) {
  if (is.na(thisOrder)) {
    OrdGrp <- NA
  } else {
    if (thisOrder < 4) {
      OrdGrp <- "Early"
    } else {
      OrdGrp <- "Later"
    }
  }
}


