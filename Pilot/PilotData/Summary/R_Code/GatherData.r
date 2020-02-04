library(data.table)

## Gather the data from the immediate and delayed version.
parent.folder <- "D:/OneDirve_CU/OneDrive - Cardiff University/VRCuriosityMemory/PilotData/RawData"
# parent.folder <- "D:/Danlu.C - CU OneDrive/OneDrive - Cardiff University/VRCuriosityMemory/PilotData/RawData"
# parent.folder <- "/Users/danlucen/Documents/OneDrive - Cardiff University/Projects/VRCuriosityMemory/PilotData/RawData"

setwd(parent.folder)

### From the Immediate task
Pilot1_InsideObjectsMemory <- read.table('Pilot_01/GroupData/InsideObjectsMemory.csv', header = TRUE)

Pilot1_Encoding.Recall.Old <- read.table('Pilot_01/GroupData/Encoding_Recall_Old.csv', header = TRUE)

Pilot1_Encoding.Recall.Old.Collapsed.Grp <- read.table('Pilot_01/GroupData/Encoding_Recall_Old_Collapsed_Grp.csv', header = TRUE)

Pilot1_Encoding.Recall.Old.Collapsed <- read.table('Pilot_01/GroupData/Encoding_Recall_Old_Collapsed.csv', header = TRUE)

Pilot1_OutsideObjectsMemory <- read.table('Pilot_01/GroupData/OutsideObjectsMemory.csv', header = TRUE)

Pilot1_Curiosity.Recall.Old <- read.table('Pilot_01/GroupData/Curiosity_Recall_Old.csv', header = TRUE)

Pilot1_Curiosity.Recall.Old.Collapsed.Grp <- read.table('Pilot_01/GroupData/Curiosity_Recall_Old_Collapsed_Grp.csv', header = TRUE)

Pilot1_Curiosity.Recall.Old.Collapsed <- read.table('Pilot_01/GroupData/Curiosity_Recall_Old_Collapsed.csv', header = TRUE)

Pilot1_InsideObjectsMemory$Version <- 1
Pilot1_Encoding.Recall.Old$Version <- 1
Pilot1_Encoding.Recall.Old.Collapsed$Version <- 1
Pilot1_Encoding.Recall.Old.Collapsed.Grp$Version <- 1
Pilot1_OutsideObjectsMemory$Version <- 1
Pilot1_Curiosity.Recall.Old$Version <- 1
Pilot1_Curiosity.Recall.Old.Collapsed$Version <- 1
Pilot1_Curiosity.Recall.Old.Collapsed.Grp$Version <- 1

### From the Immediate task
Pilot2_InsideObjectsMemory <- read.table('Pilot_02/GroupData/InsideObjectsMemory.csv', header = TRUE)

Pilot2_Encoding.Recall.Old <- read.table('Pilot_02/GroupData/Encoding_Recall_Old.csv', header = TRUE)

Pilot2_Encoding.Recall.Old.Collapsed.Grp <- read.table('Pilot_02/GroupData/Encoding_Recall_Old_Collapsed_Grp.csv', header = TRUE)

Pilot2_Encoding.Recall.Old.Collapsed <- read.table('Pilot_02/GroupData/Encoding_Recall_Old_Collapsed.csv', header = TRUE)

Pilot2_OutsideObjectsMemory <- read.table('Pilot_02/GroupData/OutsideObjectsMemory.csv', header = TRUE)

Pilot2_Curiosity.Recall.Old <- read.table('Pilot_02/GroupData/Curiosity_Recall_Old.csv', header = TRUE)

Pilot2_Curiosity.Recall.Old.Collapsed.Grp <- read.table('Pilot_02/GroupData/Curiosity_Recall_Old_Collapsed_Grp.csv', header = TRUE)

Pilot2_Curiosity.Recall.Old.Collapsed <- read.table('Pilot_02/GroupData/Curiosity_Recall_Old_Collapsed.csv', header = TRUE)

Pilot2_InsideObjectsMemory$Version <- 2
Pilot2_Encoding.Recall.Old$Version <- 2
Pilot2_Encoding.Recall.Old.Collapsed$Version <- 2
Pilot2_Encoding.Recall.Old.Collapsed.Grp$Version <- 2
Pilot2_OutsideObjectsMemory$Version <- 2
Pilot2_Curiosity.Recall.Old$Version <- 2
Pilot2_Curiosity.Recall.Old.Collapsed$Version <- 2
Pilot2_Curiosity.Recall.Old.Collapsed.Grp$Version <- 2

## Put them together
InsideObjectsMemory <- rbind(Pilot1_InsideObjectsMemory, Pilot2_InsideObjectsMemory)
Encoding.Recall.Old <- rbind(Pilot1_Encoding.Recall.Old, Pilot2_Encoding.Recall.Old)
Encoding.Recall.Old.Collapsed <- rbind(Pilot1_Encoding.Recall.Old.Collapsed, Pilot2_Encoding.Recall.Old.Collapsed)
Encoding.Recall.Old.Collapsed.Grp <- rbind(Pilot1_Encoding.Recall.Old.Collapsed.Grp, Pilot2_Encoding.Recall.Old.Collapsed.Grp)

OutsideObjectsMemory <- rbind(Pilot1_OutsideObjectsMemory, Pilot2_OutsideObjectsMemory)
Curiosity.Recall.Old <- rbind(Pilot1_Curiosity.Recall.Old, Pilot2_Curiosity.Recall.Old)
Curiosity.Recall.Old.Collapsed <- rbind(Pilot1_Curiosity.Recall.Old.Collapsed, Pilot2_Curiosity.Recall.Old.Collapsed)
Curiosity.Recall.Old.Collapsed.Grp <- rbind(Pilot1_Curiosity.Recall.Old.Collapsed.Grp, Pilot2_Curiosity.Recall.Old.Collapsed.Grp)

InsideObjectsMemory <- data.table(InsideObjectsMemory)
Encoding.Recall.Old <- data.table(Encoding.Recall.Old)
Encoding.Recall.Old.Collapsed <- data.table(Encoding.Recall.Old.Collapsed)
Encoding.Recall.Old.Collapsed.Grp <- data.table(Encoding.Recall.Old.Collapsed.Grp)

OutsideObjectsMemory <- data.table(OutsideObjectsMemory)
Curiosity.Recall.Old <- data.table(Curiosity.Recall.Old)
Curiosity.Recall.Old.Collapsed <- data.table(Curiosity.Recall.Old.Collapsed)
Curiosity.Recall.Old.Collapsed.Grp <- data.table(Curiosity.Recall.Old.Collapsed.Grp)


## Get the version name right
InsideObjectsMemory$Version 				<- factor(InsideObjectsMemory$Version, levels = c(1, 2), labels = c("Immediate", "Delayed"))
Encoding.Recall.Old$Version 				<- factor(Encoding.Recall.Old$Version, levels = c(1, 2), labels = c("Immediate", "Delayed"))
Encoding.Recall.Old.Collapsed$Version       <- factor(Encoding.Recall.Old.Collapsed$Version, levels = c(1, 2), labels = c("Immediate", "Delayed"))
Encoding.Recall.Old.Collapsed.Grp$Version   <- factor(Encoding.Recall.Old.Collapsed.Grp$Version, levels = c(1, 2), labels = c("Immediate", "Delayed"))

OutsideObjectsMemory$Version 				<- factor(OutsideObjectsMemory$Version, levels = c(1, 2), labels = c("Immediate", "Delayed"))
Curiosity.Recall.Old$Version 				<- factor(Curiosity.Recall.Old$Version, levels = c(1, 2), labels = c("Immediate", "Delayed"))
Curiosity.Recall.Old.Collapsed$Version 		<- factor(Curiosity.Recall.Old.Collapsed$Version, levels = c(1, 2), labels = c("Immediate", "Delayed"))
Curiosity.Recall.Old.Collapsed.Grp$Version  <- factor(Curiosity.Recall.Old.Collapsed.Grp$Version, levels = c(1, 2), labels = c("Immediate", "Delayed"))


## Remove old data
rm("Pilot1_InsideObjectsMemory", "Pilot1_Encoding.Recall.Old", "Pilot1_Encoding.Recall.Old.Collapsed", "Pilot1_Encoding.Recall.Old.Collapsed.Grp", "Pilot1_OutsideObjectsMemory", "Pilot1_Curiosity.Recall.Old", "Pilot1_Curiosity.Recall.Old.Collapsed", "Pilot1_Curiosity.Recall.Old.Collapsed.Grp" )

rm("Pilot2_InsideObjectsMemory", "Pilot2_Encoding.Recall.Old", "Pilot2_Encoding.Recall.Old.Collapsed", "Pilot2_Encoding.Recall.Old.Collapsed.Grp", "Pilot2_OutsideObjectsMemory", "Pilot2_Curiosity.Recall.Old", "Pilot2_Curiosity.Recall.Old.Collapsed", "Pilot2_Curiosity.Recall.Old.Collapsed.Grp" )


# Alternatively
setwd("D:/OneDirve_CU/OneDrive - Cardiff University/Projects/VRCuriosityMemory/PilotData/RawData/Summary/")
setwd("/Users/danlucen/Documents/OneDrive - Cardiff University/Projects/VRCuriosityMemory/PilotData/RawData/Summary")

load("D:/OneDirve_CU/OneDrive - Cardiff University/Projects/VRCuriosityMemory/PilotData/RawData/Summary/Pilot1.RData")

Pilot1_OutsideObjectsMemory  								            <- 	 OutsideObjectsMemory
Pilot1_Curiosity.Recall.Old.Collapsed.CuriosityType.MeanSep    <- 	 Curiosity.Recall.Old.Collapsed.CuriosityType.MeanSep
Pilot1_Curiosity.Recall.Old.Novelty      					         <-  	 Curiosity.Recall.Old.Novelty
Pilot1_Curiosity.Recall.Old.Order.Novelty                      <-     Curiosity.Recall.Old.Order.Novelty
Pilot1_Curiosity.Recall.Old.CuriosityType.MeanSep  			   <- 	 Curiosity.Recall.Old.CuriosityType.MeanSep
Pilot1_Curiosity.Recall.Old.Order.CuriosityType.MeanSep        <-     Curiosity.Recall.Old.Order.CuriosityType.MeanSep  
Pilot1_InsideObjectsMemory  								            <- 	 InsideObjectsMemory
Pilot1_Encoding.Recall.Old.Collapsed.CuriosityType.MeanSep     <- 	 Encoding.Recall.Old.Collapsed.CuriosityType.MeanSep
Pilot1_Encoding.Recall.Old.Novelty                             <-     Encoding.Recall.Old.Novelty
Pilot1_Encoding.Recall.Old.Order.Novelty  						   <- 	 Encoding.Recall.Old.Order.Novelty
Pilot1_Encoding.Recall.Old.CuriosityType.MeanSep  			      <- 	 Encoding.Recall.Old.CuriosityType.MeanSep
Pilot1_Encoding.Recall.Old.Order.CuriosityType.MeanSep         <-     Encoding.Recall.Old.Order.CuriosityType.MeanSep 

Pilot1_OutsideObjectsMemory$Version                                  <- 1
Pilot1_Curiosity.Recall.Old.Collapsed.CuriosityType.MeanSep$Version  <- 1
Pilot1_Curiosity.Recall.Old.Novelty$Version                          <- 1
Pilot1_Curiosity.Recall.Old.Order.Novelty$Version                    <- 1
Pilot1_Curiosity.Recall.Old.CuriosityType.MeanSep$Version            <- 1
Pilot1_Curiosity.Recall.Old.Order.CuriosityType.MeanSep$Version      <- 1
Pilot1_InsideObjectsMemory$Version                                   <- 1
Pilot1_Encoding.Recall.Old.Collapsed.CuriosityType.MeanSep$Version   <- 1
Pilot1_Encoding.Recall.Old.Novelty$Version                           <- 1
Pilot1_Encoding.Recall.Old.Order.Novelty$Version                     <- 1
Pilot1_Encoding.Recall.Old.CuriosityType.MeanSep$Version             <- 1
Pilot1_Encoding.Recall.Old.Order.CuriosityType.MeanSep$Version       <- 1

load("D:/OneDirve_CU/OneDrive - Cardiff University/Projects/VRCuriosityMemory/PilotData/RawData/Summary/Pilot2.RData")

Pilot2_OutsideObjectsMemory                                    <-     OutsideObjectsMemory
Pilot2_Curiosity.Recall.Old.Collapsed.CuriosityType.MeanSep    <-     Curiosity.Recall.Old.Collapsed.CuriosityType.MeanSep
Pilot2_Curiosity.Recall.Old.Novelty                            <-     Curiosity.Recall.Old.Novelty
Pilot2_Curiosity.Recall.Old.Order.Novelty                      <-     Curiosity.Recall.Old.Order.Novelty
Pilot2_Curiosity.Recall.Old.CuriosityType.MeanSep              <-     Curiosity.Recall.Old.CuriosityType.MeanSep
Pilot2_Curiosity.Recall.Old.Order.CuriosityType.MeanSep        <-     Curiosity.Recall.Old.Order.CuriosityType.MeanSep  
Pilot2_InsideObjectsMemory                                     <-     InsideObjectsMemory
Pilot2_Encoding.Recall.Old.Collapsed.CuriosityType.MeanSep     <-     Encoding.Recall.Old.Collapsed.CuriosityType.MeanSep
Pilot2_Encoding.Recall.Old.Novelty                             <-     Encoding.Recall.Old.Novelty
Pilot2_Encoding.Recall.Old.Order.Novelty                       <-     Encoding.Recall.Old.Order.Novelty
Pilot2_Encoding.Recall.Old.CuriosityType.MeanSep               <-     Encoding.Recall.Old.CuriosityType.MeanSep
Pilot2_Encoding.Recall.Old.Order.CuriosityType.MeanSep         <-     Encoding.Recall.Old.Order.CuriosityType.MeanSep 

Pilot2_OutsideObjectsMemory$Version                                  <- 2
Pilot2_Curiosity.Recall.Old.Collapsed.CuriosityType.MeanSep$Version  <- 2
Pilot2_Curiosity.Recall.Old.Novelty$Version                          <- 2
Pilot2_Curiosity.Recall.Old.Order.Novelty$Version                    <- 2
Pilot2_Curiosity.Recall.Old.CuriosityType.MeanSep$Version            <- 2
Pilot2_Curiosity.Recall.Old.Order.CuriosityType.MeanSep$Version      <- 2
Pilot2_InsideObjectsMemory$Version                                   <- 2
Pilot2_Encoding.Recall.Old.Collapsed.CuriosityType.MeanSep$Version   <- 2
Pilot2_Encoding.Recall.Old.Novelty$Version                           <- 2
Pilot2_Encoding.Recall.Old.Order.Novelty$Version                     <- 2
Pilot2_Encoding.Recall.Old.CuriosityType.MeanSep$Version             <- 2
Pilot2_Encoding.Recall.Old.Order.CuriosityType.MeanSep$Version       <- 2

## Put them together
OutsideObjectsMemory                                  <- rbind(Pilot1_OutsideObjectsMemory,                                 Pilot2_OutsideObjectsMemory)
Curiosity.Recall.Old.Collapsed.CuriosityType.MeanSep  <- rbind(Pilot1_Curiosity.Recall.Old.Collapsed.CuriosityType.MeanSep, Pilot2_Curiosity.Recall.Old.Collapsed.CuriosityType.MeanSep)
Curiosity.Recall.Old.Novelty                          <- rbind(Pilot1_Curiosity.Recall.Old.Novelty,                         Pilot2_Curiosity.Recall.Old.Novelty)
Curiosity.Recall.Old.Order.Novelty                    <- rbind(Pilot1_Curiosity.Recall.Old.Order.Novelty,                   Pilot2_Curiosity.Recall.Old.Order.Novelty)
Curiosity.Recall.Old.CuriosityType.MeanSep            <- rbind(Pilot1_Curiosity.Recall.Old.CuriosityType.MeanSep,           Pilot2_Curiosity.Recall.Old.CuriosityType.MeanSep)
Curiosity.Recall.Old.Order.CuriosityType.MeanSep      <- rbind(Pilot1_Curiosity.Recall.Old.Order.CuriosityType.MeanSep,     Pilot2_Curiosity.Recall.Old.Order.CuriosityType.MeanSep  )
InsideObjectsMemory                                   <- rbind(Pilot1_InsideObjectsMemory,                                  Pilot2_InsideObjectsMemory)
Encoding.Recall.Old.Collapsed.CuriosityType.MeanSep   <- rbind(Pilot1_Encoding.Recall.Old.Collapsed.CuriosityType.MeanSep,  Pilot2_Encoding.Recall.Old.Collapsed.CuriosityType.MeanSep)
Encoding.Recall.Old.Novelty                           <- rbind(Pilot1_Encoding.Recall.Old.Novelty,                          Pilot2_Encoding.Recall.Old.Novelty)
Encoding.Recall.Old.Order.Novelty                     <- rbind(Pilot1_Encoding.Recall.Old.Order.Novelty,                    Pilot2_Encoding.Recall.Old.Order.Novelty)
Encoding.Recall.Old.CuriosityType.MeanSep             <- rbind(Pilot1_Encoding.Recall.Old.CuriosityType.MeanSep,            Pilot2_Encoding.Recall.Old.CuriosityType.MeanSep)
Encoding.Recall.Old.Order.CuriosityType.MeanSep       <- rbind(Pilot1_Encoding.Recall.Old.Order.CuriosityType.MeanSep ,     Pilot2_Encoding.Recall.Old.Order.CuriosityType.MeanSep )

OutsideObjectsMemory                                     <- data.table(OutsideObjectsMemory)
Curiosity.Recall.Old.Collapsed.CuriosityType.MeanSep     <- data.table(Curiosity.Recall.Old.Collapsed.CuriosityType.MeanSep)
Curiosity.Recall.Old.Novelty                             <- data.table(Curiosity.Recall.Old.Novelty)
Curiosity.Recall.Old.Order.Novelty                       <- data.table(Curiosity.Recall.Old.Order.Novelty)
Curiosity.Recall.Old.CuriosityType.MeanSep               <- data.table(Curiosity.Recall.Old.CuriosityType.MeanSep)
Curiosity.Recall.Old.Order.CuriosityType.MeanSep         <- data.table(Curiosity.Recall.Old.Order.CuriosityType.MeanSep)  
InsideObjectsMemory                                      <- data.table(InsideObjectsMemory)
Encoding.Recall.Old.Collapsed.CuriosityType.MeanSep      <- data.table(Encoding.Recall.Old.Collapsed.CuriosityType.MeanSep)
Encoding.Recall.Old.Novelty                              <- data.table(Encoding.Recall.Old.Novelty)
Encoding.Recall.Old.Order.Novelty                        <- data.table(Encoding.Recall.Old.Order.Novelty)
Encoding.Recall.Old.CuriosityType.MeanSep                <- data.table(Encoding.Recall.Old.CuriosityType.MeanSep)
Encoding.Recall.Old.Order.CuriosityType.MeanSep          <- data.table(Encoding.Recall.Old.Order.CuriosityType.MeanSep) 

OutsideObjectsMemory$Version                                     <- factor(OutsideObjectsMemory$Version, levels = c(1, 2), labels = c("Immediate", "Delayed"))
Curiosity.Recall.Old.Collapsed.CuriosityType.MeanSep$Version     <- factor(Curiosity.Recall.Old.Collapsed.CuriosityType.MeanSep$Version, levels = c(1, 2), labels = c("Immediate", "Delayed"))
Curiosity.Recall.Old.Novelty$Version                             <- factor(Curiosity.Recall.Old.Novelty$Version, levels = c(1, 2), labels = c("Immediate", "Delayed"))
Curiosity.Recall.Old.Order.Novelty$Version                       <- factor(Curiosity.Recall.Old.Order.Novelty$Version, levels = c(1, 2), labels = c("Immediate", "Delayed"))
Curiosity.Recall.Old.CuriosityType.MeanSep$Version               <- factor(Curiosity.Recall.Old.CuriosityType.MeanSep$Version, levels = c(1, 2), labels = c("Immediate", "Delayed"))
Curiosity.Recall.Old.Order.CuriosityType.MeanSep$Version         <- factor(Curiosity.Recall.Old.Order.CuriosityType.MeanSep  $Version, levels = c(1, 2), labels = c("Immediate", "Delayed"))  
InsideObjectsMemory$Version                                      <- factor(InsideObjectsMemory$Version, levels = c(1, 2), labels = c("Immediate", "Delayed"))
Encoding.Recall.Old.Collapsed.CuriosityType.MeanSep$Version      <- factor(Encoding.Recall.Old.Collapsed.CuriosityType.MeanSep$Version, levels = c(1, 2), labels = c("Immediate", "Delayed"))
Encoding.Recall.Old.Novelty$Version                              <- factor(Encoding.Recall.Old.Novelty$Version, levels = c(1, 2), labels = c("Immediate", "Delayed"))
Encoding.Recall.Old.Order.Novelty$Version                        <- factor(Encoding.Recall.Old.Order.Novelty$Version, levels = c(1, 2), labels = c("Immediate", "Delayed"))
Encoding.Recall.Old.CuriosityType.MeanSep$Version                <- factor(Encoding.Recall.Old.CuriosityType.MeanSep$Version, levels = c(1, 2), labels = c("Immediate", "Delayed"))
Encoding.Recall.Old.Order.CuriosityType.MeanSep$Version          <- factor(Encoding.Recall.Old.Order.CuriosityType.MeanSep $Version, levels = c(1, 2), labels = c("Immediate", "Delayed")) 


rm(Pilot1_OutsideObjectsMemory,
   Pilot1_Curiosity.Recall.Old.Collapsed.CuriosityType.MeanSep,
   Pilot1_Curiosity.Recall.Old.Novelty,
   Pilot1_Curiosity.Recall.Old.Order.Novelty,
   Pilot1_Curiosity.Recall.Old.CuriosityType.MeanSep,
   Pilot1_Curiosity.Recall.Old.Order.CuriosityType.MeanSep,
   Pilot1_InsideObjectsMemory,
   Pilot1_Encoding.Recall.Old.Collapsed.CuriosityType.MeanSep,
   Pilot1_Encoding.Recall.Old.Novelty,
   Pilot1_Encoding.Recall.Old.Order.Novelty,
   Pilot1_Encoding.Recall.Old.CuriosityType.MeanSep,
   Pilot1_Encoding.Recall.Old.Order.CuriosityType.MeanSep)

rm(Pilot2_OutsideObjectsMemory,
   Pilot2_Curiosity.Recall.Old.Collapsed.CuriosityType.MeanSep,
   Pilot2_Curiosity.Recall.Old.Novelty,
   Pilot2_Curiosity.Recall.Old.Order.Novelty,
   Pilot2_Curiosity.Recall.Old.CuriosityType.MeanSep,
   Pilot2_Curiosity.Recall.Old.Order.CuriosityType.MeanSep,
   Pilot2_InsideObjectsMemory,
   Pilot2_Encoding.Recall.Old.Collapsed.CuriosityType.MeanSep,
   Pilot2_Encoding.Recall.Old.Novelty,
   Pilot2_Encoding.Recall.Old.Order.Novelty,
   Pilot2_Encoding.Recall.Old.CuriosityType.MeanSep,
   Pilot2_Encoding.Recall.Old.Order.CuriosityType.MeanSep)

ls()

save(bad.ps.less.strict,
	  OutsideObjectsMemory,
     Curiosity.Recall.Old.Collapsed.CuriosityType.MeanSep,
     Curiosity.Recall.Old.Novelty,
     Curiosity.Recall.Old.Order.Novelty,
     Curiosity.Recall.Old.CuriosityType.MeanSep,
     Curiosity.Recall.Old.Order.CuriosityType.MeanSep,
     InsideObjectsMemory,
     Encoding.Recall.Old.Collapsed.CuriosityType.MeanSep,
     Encoding.Recall.Old.Novelty,
     Encoding.Recall.Old.Order.Novelty,
     Encoding.Recall.Old.CuriosityType.MeanSep,
     Encoding.Recall.Old.Order.CuriosityType.MeanSep,
	  file = "D:/OneDirve_CU/OneDrive - Cardiff University/Projects/VRCuriosityMemory/PilotData/RawData/Summary/Summary.RData")

rm(list=ls())


## Export the data to excel files
load("/Users/danlucen/Documents/OneDrive - Cardiff University/Projects/VRCuriosityMemory/PilotData/RawData/Summary/Summary.RData")
load("D:/OneDirve_CU/OneDrive - Cardiff University/Projects/VRCuriosityMemory/PilotData/RawData/Summary/Summary.RData")

Curiosity.Recall.Old.Order.CuriosityType.MeanSep

## Show participants who had NA values
theData <- Curiosity.Recall.Old.Order.CuriosityType.MeanSep[!SubjectNo %in% bad.ps.less.strict & is.na(CorrSeenHit)]

theData <- Curiosity.Recall.Old.Order.CuriosityType.MeanSep[!SubjectNo %in% bad.ps.less.strict & !is.na(CorrSeenHit)]

theData[, .(Mean = mean(CorrSeenHit), SD = sd(CorrSeenHit)), by = c("ItemOrderType", "RatingGroup", "Version")]

write.csv(theData[, .(Mean = mean(CorrSeenHit), SD = sd(CorrSeenHit)), by = c("ItemOrderType", "RatingGroup", "Version")], 'meanSD_EarlyvsLate.csv')

excluded.subjects <- as.character(unique(Curiosity.Recall.Old.Order.CuriosityType.MeanSep[SubjectNo %in% bad.ps.less.strict | is.na(CorrSeenHit)]$SubjectNo))

Curiosity.Recall.Old.Collapsed.CuriosityType.MeanSep[Version == "Immediate" & !SubjectNo %in% excluded.subjects]

theData <- Curiosity.Recall.Old.Collapsed.CuriosityType.MeanSep[!SubjectNo %in% excluded.subjects]
theData[, .(Mean = mean(CorrSeenHit), SD = sd(CorrSeenHit)), by = c("RatingGroup", "Version")]
write.csv(theData[, .(Mean = mean(CorrSeenHit), SD = sd(CorrSeenHit)), by = c("RatingGroup", "Version")], 'meanSD_acrossAllPositions.csv')
