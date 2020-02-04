library(data.table)


# Randomly select objects to allocate to each room
# ------------------------------------------------
# Procedure:
#  1. Randomly order the object list
#  2. The fisrt 16 belongs to the two rooms in the *Familiarisation* task
#  3. The next 96 belong to the rooms in the encoding task
#  4. The rest are distractors
#  5. Then based on the allocation calculate the random order for the mmeory test.
# ------------------------------------------------

ArrangeObjects <- function(this.participant.no, familiarisation.rooms, exploration.rooms, object.list, flipped.objects) {
	## Start randomising

	# this.participant.no <- 1
	set.seed(this.participant.no)

	random.object.list <- sample(as.character(object.list$Object) )

	## Check the first 16 + 96 = 108 objects to see if there is any object that needs to be "flipped"

	for (i in c(1:108)) {
		this.object <- random.object.list[i]

		if (this.object %in% flipped.objects & i %% 2 == 1) {
			random.object.list[i] <- paste(this.object, "FLIPPED", sep = "_")
		}
	}

	## Make a list of rooms and distractor
	alloc.list <- c(familiarisation.rooms, exploration.rooms)
	alloc.list <- rep(alloc.list, each = 6)
	alloc.list <- c(alloc.list, rep("None", each = 48))

	## Generate a table to map the object and allocation type
	obj.tbl <- data.table(Object = random.object.list, Scene = alloc.list)

	# Generate the files for the tasks
	if (this.participant.no < 10) {p.folder <- paste0("P0", this.participant.no)} else {p.folder <- paste0("P", this.participant.no)}
	task.folder <- "Familiarisation" 
	type.folder <- "OutsideObjectList"

	if (!dir.exists(p.folder)) {dir.create(p.folder)}
	if (!dir.exists(file.path(p.folder, task.folder))) {dir.create( file.path(p.folder, task.folder))}
	if (!dir.exists(file.path(p.folder, task.folder, type.folder))) {dir.create(file.path(p.folder, task.folder, type.folder))}

	## Familiarisation Task
	to.folder <- paste(p.folder, task.folder, type.folder, sep = .Platform$file.sep)

	for (this.room in familiarisation.rooms) {
		these.objects <- as.character(obj.tbl[Scene == this.room]$Object)
		these.objects <- paste(these.objects, collapse = ",")
		these.objects <- paste0(these.objects, ',')

		write(noquote( these.objects), paste0(to.folder, .Platform$file.sep, this.room, ".csv"), sep = ",")
	}

	## *Exploration* Task
	task.folder <- "Exploration"
	type.folder <- "OutsideObjectList"

	if (!dir.exists(file.path(p.folder, task.folder))) {dir.create( file.path(p.folder, task.folder))}
	if (!dir.exists(file.path(p.folder, task.folder, type.folder))) {dir.create(file.path(p.folder, task.folder, type.folder))}

	to.folder <- paste(p.folder, task.folder, type.folder, sep = .Platform$file.sep)

	for (this.room in exploration.rooms) {
		these.objects <- as.character(obj.tbl[Scene == this.room]$Object)
		these.objects <- paste(these.objects, collapse = ",")
		these.objects <- paste0(these.objects, ',')

		write(noquote( these.objects), paste0(to.folder, .Platform$file.sep, this.room, ".csv"), sep = ",")
	}


	# Randomise the room order
	## *Familiarisation*
	if (!dir.exists(file.path(p.folder, "Familiarisation", "Room"))) {dir.create(file.path(p.folder, "Familiarisation", "Room"))}
	room.order <- c("Prepractice", sample(familiarisation.rooms))
	room.order <- paste(room.order, collapse = ",")
	room.order <- paste0(room.order, ',')
	write(noquote( room.order), paste( p.folder, "Familiarisation", "Room",  "RoomOrder.csv", sep = .Platform$file.sep), sep = ",")

	## "Exploration"
	if (!dir.exists(file.path(p.folder, "Exploration", "Room"))) {dir.create(file.path(p.folder, "Exploration", "Room"))}
	room.order <- sample(exploration.rooms)
	room.order <- paste(room.order, collapse = ",")
	room.order <- paste0(room.order, ',')
	write(noquote( room.order), paste( p.folder, "Exploration", "Room",  "RoomOrder.csv", sep = .Platform$file.sep), sep = ",")

	# Generate a file for the memory test
	## Exclude those in the *Familiarisation* task
	memory.list <- obj.tbl[! Scene %in% familiarisation.rooms]

	lbls <- c("Flip", "NoFlip")

	lbl.list <- c(rep(lbls, (nrow(memory.list) - 48) /2), rep(c("NoFlip"), 48))

	memory.list$Label <- lbl.list

	random.memory.list <- memory.list[sample(nrow(memory.list)),]

	if (!dir.exists(file.path(p.folder, "TestOrder"))) {dir.create(file.path(p.folder, "TestOrder"))}
	write.table( noquote( random.memory.list), paste( p.folder, "TestOrder", "MemoryTestOrder.csv", sep = .Platform$file.sep), sep = ",", row.names = F, col.names = F, quote = F)

}