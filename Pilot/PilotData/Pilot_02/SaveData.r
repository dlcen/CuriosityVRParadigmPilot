
write.table(InsideObjectsMemory, 'GroupData/InsideObjectsMemory.csv', row.names = FALSE)

write.table(Encoding.Recall.Old, 'GroupData/Encoding_Recall_Old.csv', row.names = FALSE)

write.table(Encoding.Recall.Old.Collapsed.Grp, 'GroupData/Encoding_Recall_Old_Collapsed_Grp.csv', row.names = FALSE)

write.table(Encoding.Recall.Old.Collapsed, 'GroupData/Encoding_Recall_Old_Collapsed.csv', row.names = FALSE)

write.table(OutsideObjectsMemory[!SubjectNo %in% bad.ps.less.strict], 'GroupData/OutsideObjectsMemory.csv', row.names = FALSE)

write.table(Curiosity.Recall.Old[!SubjectNo %in% bad.ps.less.strict], 'GroupData/Curiosity_Recall_Old.csv', row.names = FALSE)

write.table(Curiosity.Recall.Old.Collapsed.Grp[!SubjectNo %in% bad.ps.less.strict], 'GroupData/Curiosity_Recall_Old_Collapsed_Grp.csv', row.names = FALSE)

write.table(Curiosity.Recall.Old.Collapsed[!SubjectNo %in% bad.ps.less.strict], 'GroupData/Curiosity_Recall_Old_Collapsed.csv', row.names = FALSE)

## Write corrected data
library(data.table)

setwd("D:/OneDirve_CU/OneDrive - Cardiff University/Projects/VRCuriosityMemory/PilotData/RawData/Pilot_02")
load("D:/OneDirve_CU/OneDrive - Cardiff University/Projects/VRCuriosityMemory/PilotData/RawData/Pilot_02/.RData")

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
	 file = "D:/OneDirve_CU/OneDrive - Cardiff University/Projects/VRCuriosityMemory/PilotData/RawData/Summary/Pilot2.RData")

