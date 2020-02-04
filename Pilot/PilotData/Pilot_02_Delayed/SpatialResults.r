library(data.table); library(ggplot2)

## Some preparation work: a function to calcualte euclidean distance between two points on a plane
euc.dist <- function(x1, x2) sqrt(sum(x1 - x2)^2)

## Set the working directory
parent.folder <- getwd()
parent.folder <- "D:\\OneDirve_CU\\OneDrive - Cardiff University\\VRCuriosityMemory\\PilotData\\RawData\\Pilot_02"

setwd(parent.folder)

## Get all the participants' folders
participant.folders <- dir(pattern = "^P")

## Read the data from each participant's folder
cor.pos <- NULL
obj.pos <- NULL

for (this.p in participant.folders) {
	setwd(paste0(parent.folder, .Platform$file.sep, this.p))

	cor.pos.files <- dir(pattern = "*ObjectColle*")
	box.pos.files <- dir(pattern = "*BoxPosition*")
	obj.pos.files <- dir(pattern = "*ObjPosition*")

	if (length(box.pos.files) > 0) {
		this.p.cor.pos <- NULL
		this.p.obj.pos <- NULL

		for (n in c(1:length(cor.pos.files))) {
			this.room <- strsplit(cor.pos.files[n], split = "_")[[1]][1]

			this.corrt.pos <- read.csv( cor.pos.files[n], header = T)
			this.corrt.pos <- data.table(this.corrt.pos)

			## Get the position of the boxes relataive to the room
			this.corrt.pos$BoxPos_Z <-  this.corrt.pos$BoxPos_Z - 42

			this.corrt.pos$SubjectNo <- this.p
			this.corrt.pos$Room <- this.room

			# Get the object data
			if (length(obj.pos.files) > 0 ) {
				this.obj.file <- read.csv( obj.pos.files[n], header = T)
				this.obj.file <- data.table(this.obj.file)
				this.obj.file$SubjectNo <- this.p
			} 

			# Get the box data
			this.box.file <- read.csv( box.pos.files[n], header = T)
			names(this.box.file) <- c("Room", "ObjPos_X", "ObjPos_Y", "ObjPos_Z")
			this.box.file$Object <- "None"
			this.box.file <- this.box.file[, c("Room", "Object", "ObjPos_X", "ObjPos_Y", "ObjPos_Z")]
			this.box.file <- data.table(this.box.file)
			this.box.file$SubjectNo <- this.p

			# If there is object data and some object data is invalid
			if (length(obj.pos.files) > 0) {

				question_data <- this.obj.file[ObjPos_X > 10 | ObjPos_Y < 0]
				if (length(question_data) > 0) {

					# Find out the box that does not have an object placed on.
					# First to calculate the distance between the boxes and the objects.
					for (i in c(1:length(this.box.file))) {
						this.distance <- NULL
						for (j in c(1:length(this.obj.file[Object != question_data$Object]))) {
							this.distance <- c(this.distance, euc.dist(this.obj.file[i, c("ObjPos_X", "ObjPos_Z"), with = FALSE], this.box.file[j, c("ObjPos_X", "ObjPos_Z"), with = FALSE]))		
						}
						
					}
					# Second to find out the one with the largest distances.


				}
			}

			question_data <- this.box.file[ObjPos_X > 10 | ObjPos_Y < 0]
			
			

			this.p.cor.pos <- rbind(this.p.cor.pos, this.corrt.pos)
			this.p.obj.pos <- rbind(this.p.obj.pos, this.file)
		}

		cor.pos <- rbind(cor.pos, this.p.cor.pos)
		obj.pos <- rbind(obj.pos, this.p.obj.pos)
	}

	setwd(parent.folder)
}

write.csv(obj.pos, "Inside_Object_Positions.csv", row.names = F)

cor.box.pos <- unique(cor.pos[, c("Room","BoxPos_X", "BoxPos_Y", "BoxPos_Z")])

obj.pos.corrected <- obj.pos[ObjPos_X < 16 & ObjPos_Y >= 0]

obj.pos.corrected[ObjPos_X > 10]

ggplot(data = cor.box.pos, aes(x = BoxPos_X, y = BoxPos_Z)) + 
  geom_point(color = "red", size = 3) +
  geom_point(data = obj.pos.corrected, aes(x = ObjPos_X, y = ObjPos_Z)) +
  facet_wrap( ~ Room, ncol = 3)