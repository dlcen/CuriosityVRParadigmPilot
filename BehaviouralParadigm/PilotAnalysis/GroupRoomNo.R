library(data.table)

GroupRoomNo <- function(this.SubjectNo) {
	this.data <- outside.hit.rate.per.room[SubjectNo == this.SubjectNo]

	this.mean <- mean(this.data$CurRating)
	this.median <- median(this.data$CurRating)

	message("This participant No.: ", this.SubjectNo, "\n")

	message("No. of rooms with a rating > median: ", nrow(this.data[CurRating >  this.median]), "\n")
	message("No. of rooms with a rating = median: ", nrow(this.data[CurRating == this.median]), "\n")
	message("No. of rooms with a rating < median: ", nrow(this.data[CurRating <  this.median]), "\n")
	message("No. of rooms with a rating > mean: ",   nrow(this.data[CurRating >  this.mean]), "\n")
	message("No. of rooms with a rating = mean: ",   nrow(this.data[CurRating == this.mean]), "\n")
	message("No. of rooms with a rating < mean: ",   nrow(this.data[CurRating <  this.mean]), "\n")

}

GroupRoomNo2 <- function(this.SubjectNo) {
	this.data <- object.recognition.old[SubjectNo == this.SubjectNo]

	high.rooms <- as.character(unique(this.data[CurGrpMd == "High"]$Scene))
	high.no    <- length(high.rooms)

	low.rooms <- as.character(unique(this.data[CurGrpMd == "Low"]$Scene))
	low.no    <- length(low.rooms)

	message("This participant No.: ", this.SubjectNo, "\n")

	message("There are ", high.no,  " rooms in the High group. \n")
	message("There are ", low.no,  " rooms in the Low group. \n")
}