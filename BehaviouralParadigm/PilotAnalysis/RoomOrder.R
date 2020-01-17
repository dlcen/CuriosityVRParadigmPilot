library(data.table)

participant.folders <- dir(path = "./PilotData/IndividualData/", pattern = "^P")

room.order <- NULL

for (this.p in participant.folders) {

	this.path <- paste0("./PilotData/IndividualData/", this.p, "/Exploration/Room/")
	
	this.room.order <- readLines(paste0(this.path, .Platform$file.sep, "RoomOrder.csv"), warn = F)
	this.order.list <- strsplit( this.room.order, split = "," )[[1]]
    this.order.no   <- c(1:length(this.order.list))
	this.temp       <- data.frame(Room = this.order.list, Order = this.order.no)

	this.temp$SubjectNo <- this.p

	room.order <- rbind(room.order, this.temp)
	  
}

room.order <- data.table(room.order)