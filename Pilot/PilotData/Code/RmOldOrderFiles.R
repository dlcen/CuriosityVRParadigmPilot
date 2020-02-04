library(data.table)

participant.folders <- dir(path = "IndividualRawData/", pattern = "^P")

for (this.p in participant.folders) {

	this.inside.folder  <- paste(c("IndividualRawData", this.p, "ConditionOrders", "Inside"),  collapse = .Platform$file.sep)
	this.outside.folder <- paste(c("IndividualRawData", this.p, "ConditionOrders", "Outside"), collapse = .Platform$file.sep)
	
	if (file.exists(paste0(this.inside.folder, .Platform$file.sep, "InsideOrders.csv"))) { unlink( paste0(this.inside.folder, .Platform$file.sep, "InsideOrders.csv") ) }

	if (file.exists(paste0(this.outside.folder, .Platform$file.sep, "OutsideOrders.csv"))) { unlink( paste0(this.outside.folder, .Platform$file.sep, "OutsideOrders.csv") ) }
}