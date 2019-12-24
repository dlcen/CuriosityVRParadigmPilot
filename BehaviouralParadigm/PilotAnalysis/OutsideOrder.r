library(data.table)


participant.folders <- dir(path = "./PilotData/IndividualData/", pattern = "^P")

outside.orders <- NULL

for (this.p in participant.folders) {

	this.path <- paste0("./PilotData/IndividualData/", this.p, "/Exploration/OutsideObjectList/")
	
	outside.item.orders <- list.files(path = this.path, pattern = "\\.csv$", ignore.case = TRUE)
	  
	this.p.orders <- NULL

	for (this.order in outside.item.orders) {
		this.room <- strsplit(this.order, split = '.csv')[[1]]

		this.order.list <- readLines(paste0(this.path, .Platform$file.sep, this.order), warn = F)
		this.order.list <- strsplit( this.order.list[1], split = "," )[[1]]
		this.order.no   <- c(1:6)
		this.temp <- data.frame(Item = this.order.list, Order = this.order.no)

		this.temp$SubjectNo <- this.p
		this.temp$Room <- this.room

		this.p.orders <- rbind(this.p.orders, this.temp)
	}

	outside.orders <- rbind(outside.orders, this.p.orders)
}

outside.orders <- data.table(outside.orders)

# write.table(outside.orders, "OutsideOrders.csv", row.names = FALSE)
