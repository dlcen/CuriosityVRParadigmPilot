library(data.table); library(ggplot2)

load("Together/Data/ResponsePerRoom.RData")

participants.to.be.excluded <- c("P17", "P54")
hit.rate.per.room			<- hit.rate.per.room[!SubjectNo %in% participants.to.be.excluded] # Exlude the participants 

# Calculate the percentage of familiar rooms to be choosen as first and second respectively, and same for the novel rooms

n.familiar.rooms 			<- nrow(hit.rate.per.room[Group == "Familiar"])
n.familiar.rooms.first		<- nrow(hit.rate.per.room[Group == "Familiar" & RoomOrder == "First"])
n.familiar.rooms.second		<- nrow(hit.rate.per.room[Group == "Familiar" & RoomOrder == "Second"])

n.familiar.rooms.first/n.familiar.rooms 	# 0.3655462
n.familiar.rooms.second/n.familiar.rooms 	# 0.6344538

n.novel.rooms 				<- nrow(hit.rate.per.room[Group == "Novel"])
n.novel.rooms.first			<- nrow(hit.rate.per.room[Group == "Novel" & RoomOrder == "First"])
n.novel.rooms.second		<- nrow(hit.rate.per.room[Group == "Novel" & RoomOrder == "Second"])

n.novel.rooms.first/n.novel.rooms 	# 0.6371308
n.novel.rooms.second/n.novel.rooms 	# 0.3628692

# Compare the curiosity rating of the first and second rooms
mean.ratings <- hit.rate.per.room[, .(meanRating = mean(CurRating, na.rm = T)), by = c("SubjectNo", "RoomOrder")]

mean.ratings[, .(meanRating = mean(meanRating, na.rm = T), SD = sd(meanRating)), by = c("RoomOrder")]

#    RoomOrder meanRating       SD
# 1:     First   4.620833 1.371801
# 2:    Second   3.868750 1.502856

t.test(meanRating ~ RoomOrder, data = mean.ratings, paired = T)

#         Paired t-test

# data:  meanRating by RoomOrder
# t = 2.6281, df = 79, p-value = 0.01031
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  0.1824818 1.3216848
# sample estimates:
# mean of the differences 
#               0.7520833 
