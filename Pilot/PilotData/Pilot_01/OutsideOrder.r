setwd("D:\\Danlu.C - CU OneDrive\\OneDrive - Cardiff University\\VRCuriosityMemory\\PilotData\\RawData\\Pilot_01\\P06")

parent.folder <- "D:\\Danlu.C - CU OneDrive\\OneDrive - Cardiff University\\VRCuriosityMemory\\PilotData\\RawData\\Pilot_01\\P06"
outside.order.folder <- "OutsideOrders"

setwd(paste0(parent.folder, .Platform$file.sep, outside.order.folder))

setwd("D:/OneDirve_CU/OneDrive - Cardiff University/VRCuriosityMemory/PilotData/RawData/Pilot_01/P11/OutsideOrders")

order.lists <- dir()

outside.order <- NULL

for (this.list in order.lists) {
	this.order <- as.character( read.csv(this.list, header = FALSE)$V1)
	this.order.no <- c(1:length(this.order))
	this.room  <- strsplit(this.list, ".csv")[[1]]
	this.room  <- rep(this.room, length(this.order))

	this.tmp   <- data.frame(Room = this.room, Object = this.order, Order = this.order.no)
	outside.order <- rbind(outside.order, this.tmp)
}

setwd("D:/OneDirve_CU/OneDrive - Cardiff University/VRCuriosityMemory/PilotData/RawData/Pilot_01/P11")
write.table(outside.order, "OutsideOrders.csv", row.names = FALSE)

## Get the outside orders for all participants
setwd("D:/OneDirve_CU/OneDrive - Cardiff University/VRCuriosityMemory/PilotData/RawData/Pilot_01/")

participant.folders <- dir(pattern = "^P")

outside.orders <- NULL

for (this.p in participant.folders) {
	this.p.no <- as.numeric(strsplit(this.p, "P")[[1]][2])

	if (this.p.no < 6) {
		this.orders <- read.csv(paste0(this.p, .Platform$file.sep, "OutsideOrders.csv"), header = T, fileEncoding="UTF-8-BOM")
	} else {
		this.orders <- read.table(paste0(this.p, .Platform$file.sep, "OutsideOrders.csv"), header = T)
	}

	this.orders$SubjectNo <- this.p

	outside.orders <- rbind(outside.orders, this.orders)
}

write.table(outside.orders, "OutsideOrders.csv", row.names = FALSE)

## Check the orders
setwd(paste0(parent.folder,.Platform$file.sep, "P12"))

dir(pattern = "*_ObjectCollection_*")

read.csv("CuriosityRatings.csv")