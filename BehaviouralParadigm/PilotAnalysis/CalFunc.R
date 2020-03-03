# Hit can be calculated as choosing "Seen" or "Familiar" to seen objects
SFHitCal <- function(ActualResp, Scene){
	if (Scene != "None" & ActualResp %in% c("Seen", "Familiar")) {
		Hit <- 1
	} else if (Scene == "None" & ActualResp == "New") {
		Hit <- 1
	} else {
		Hit <- 0
	}	

	return(Hit)
}

# Hit can be calculated as only choosing "Seen" to seen objects
SHitCal <- function(ActualResp, Scene){
  if (Scene != "None" & ActualResp == "Seen") {
    Hit <- 1
  } else if (Scene == "None" & ActualResp == "New") {
    Hit <- 1
  } else {
    Hit <- 0
  } 
  
  return(Hit)
}

SFFalseHitCal <- function(ActualResp, Scene){
  if (Scene == 'None' & ActualResp %in% c('Seen', 'Familiar')) {
    FalseHit <- 1
  } else {
    FalseHit <- 0
  }
}

SFalseHitCal <- function(ActualResp, Scene){
  if (Scene == 'None' & ActualResp == 'Seen') {
    FalseHit <- 1
  } else {
    FalseHit <- 0
  }
}


# Label "high" and "low" curiosity groups
## According to medien
GrpMedianSep <- function(thisScor, medianScor) {
      
      if (is.na(thisScor)) {
            ScorGrp <- NA
      } else {
            if (thisScor < medianScor) {   # Used to be <=, now switch to a more extreme median split by removing the rooms with median equal values
                  ScorGrp <- "Low"
            } else if (thisScor > medianScor) {
                  ScorGrp <- "High"
            } else {
                  ScorGrp <- NA
            }
      }
}

## According to mean
GrpMeanSep <- function(thisScor, meanScor) {
      if (is.na(thisScor)) {
            ScorGrp <- NA
      } else {
            if (thisScor < meanScor) {
                  ScorGrp <- "Low"
            } else if (thisScor > meanScor) {
                  ScorGrp <- "High"
            } else {
                  ScorGrp <- NA
            }
      }
}

## According to 4
GrpSep <- function(thisScor) {
  if (is.na(thisScor)) {
            ScorGrp <- NA
      } else {
            if (thisScor < 6) {
                  ScorGrp <- "Low"
            } else if (thisScor >= 6) {
                  ScorGrp <- "High"
            } else {
                  ScorGrp <- NA
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


# Calculate the corrected hit rate given the group
CorrHitRateCal <- function(data, group, false.alarm.rate) {
  results <- data[!is.na(get(group)), .(SFHit = mean(SFHit, na.rm = TRUE), SHit = mean(SHit, na.rm = TRUE)), by = c("SubjectNo", group)]
  results <- merge(results, false.alarm.rate, all = TRUE)
  results[, c("SFAcc", "SAcc") := list( (SFHit - SFFalse), (SHit - SFalse))]

  return(results)
}

# Calculate the corrected hit rate given the group and according to the order ('early' vs 'later')
CorrHitRateOrderCal <- function(data, group, false.alarm.rate) {
  results   <- data[!is.na(get(group)), .(SFHit = mean(SFHit, na.rm = TRUE), SHit = mean(SHit, na.rm = TRUE)), by = c("SubjectNo", group, "ObjOrdGrp")]
  results   <- merge(results, false.alarm.rate, all = TRUE)
  results[, c("SFAcc", "SAcc") := list( (SFHit - SFFalse), (SHit - SFalse))]

  return(results)
}


# Calculate the difference in corrected hit rate between *high* and *low* groups or between *early* or *later* groups
DiffCal <- function(data, comparison, dv, group) {
  grp.h <- data[get(comparison) == group[1], ..dv]
  grp.l <- data[get(comparison) == group[2], ..dv]

  grp.diff <- grp.h - grp.l

  return(grp.diff)
}


# Find out the rooms with a score higher/lower than median also has a score higher/lower than median
# RoomOverlapChecker <- function(data, )

