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