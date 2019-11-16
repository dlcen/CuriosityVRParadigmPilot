# Plot figures that incorporate all data from Pilot 1 and 2
# 
# Figures include:
# 
# 	- Individual plots: item recollection, source memory and false alarm rates for both as a function of rooms (with curioisty rating and color-coded by novelty group)
#   - Novelty plots: item recollection, source memory in each novelty group, for both Pilot 1 (immediate) and Pilot 2 (delayed) conditions.
#   - Novelty (temporal order) plots: item recollection, source memory respectively for the Early and Later groups.
#   - Curiosity plots
#   - Curiosity (temporal order) plots
  
library(ggplot2); library(data.table); library(cowplot)

#  Individual plots
## Load the data
load("Pilot_01_Immediate/GroupData/InsideObjectsResponsePerRoom.RData")
load("Pilot_01_Immediate/GroupData/OutsideObjectsResponsePerRoom.RData")

inside.hit.rate.per.room$Condition  <- "Immediate"
outside.hit.rate.per.room$Condition <- "Immediate"

pilot.1.inside.hit.rate.per.room 	<- inside.hit.rate.per.room
pilot.1.outside.hit.rate.per.room 	<- outside.hit.rate.per.room

load("Pilot_02_Delayed/GroupData/InsideObjectsResponsePerRoom.RData")
load("Pilot_02_Delayed/GroupData/OutsideObjectsResponsePerRoom.RData")

inside.hit.rate.per.room$Condition  <- "Delayed"
outside.hit.rate.per.room$Condition <- "Delayed"

pilot.2.inside.hit.rate.per.room 	<- inside.hit.rate.per.room
pilot.2.outside.hit.rate.per.room 	<- outside.hit.rate.per.room

rm(list = c("inside.hit.rate.per.room", "outside.hit.rate.per.room"))

inside.hit.rate.per.room  			<- rbind(pilot.1.inside.hit.rate.per.room,  pilot.2.inside.hit.rate.per.room)
outside.hit.rate.per.room 			<- rbind(pilot.1.outside.hit.rate.per.room, pilot.2.outside.hit.rate.per.room)

rm(list = c("pilot.1.inside.hit.rate.per.room", "pilot.1.outside.hit.rate.per.room", "pilot.2.inside.hit.rate.per.room", "pilot.2.outside.hit.rate.per.room"))

inside.hit.rate.per.room$phase 		<- "Inside"
outside.hit.rate.per.room$phase 	<- "Outside"

hit.rate.per.room 					<- rbind(inside.hit.rate.per.room, outside.hit.rate.per.room)

rm(list = c("inside.hit.rate.per.room", "outside.hit.rate.per.room"))

## Plot the figure
ggplot(data = hit.rate.per.room[Condition == "Delayed" & phase == "Outside"], aes(x = Context, y = SAcc)) + 
	geom_point(size = 2, aes(color = Group)) +
	geom_hline(aes(yintercept = SFalse), size = 0.75, linetype = "dashed") +
	geom_hline(yintercept = 0, colour = "#595959") +
	facet_wrap( ~ SubjectNo, nrow = 8) +
	labs(x = "Room", y = "Corrected hit rate for \"Seen\" response")

