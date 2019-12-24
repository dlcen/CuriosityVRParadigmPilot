library(data.table); library(ggplot2)

# Get the participant no.s

participant.list <- list.files(path = "./PilotData/IndividualData/", pattern = "^P")

# First look at the curiosity rating and interesting rating for each participants

## Get the curiosity ratings for each participant
ratings <- NULL

for (this.p in participant.list) {
	this.ratings <- read.csv(paste0("./PilotData/IndividualData/", this.p, "/Exploration/Ratings.csv"), header = TRUE)
	this.ratings$SubjectNo <- this.p
	ratings  	 <- rbind(ratings, this.ratings)
}

## Plot a histogram for each participant, respectively for curiosity ratings and interesting ratings

### Curiosity ratings
ggplot(ratings, aes(x = Curiosity)) + 
	geom_histogram(binwidth = 0.5) +
	geom_vline(aes(xintercept = median(Curiosity)), color = "red", size = 1) +
	scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6))+
	scale_y_continuous(breaks = c(0:10)) +
	labs(x = "Curiosity Rating", y = "Count") + 
	facet_wrap( ~ SubjectNo) +
	theme(axis.text = element_text(size = 12),
		  strip.text = element_text(size = 12, face = "bold"))
ggsave("./Figures/CuriosityRatings.png", width = 12, height = 4)


### Interesting ratings
ggplot(ratings, aes(x = Interest)) + 
	geom_histogram(binwidth = 0.5) +
	geom_vline(aes(xintercept = median(Interest)), color = "red", size = 1) +
	scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6))+
	scale_y_continuous(breaks = c(0:10)) +
	labs(x = "Interest Rating", y = "Count") + 
	facet_wrap( ~ SubjectNo) +
	theme(axis.text = element_text(size = 12),
		  strip.text = element_text(size = 12, face = "bold"))
ggsave("./Figures/InterestRatings.png", width = 12, height = 4)

# Second, look at the time spent in each room
## Get the duration data
durations <- NULL

for (this.p in participant.list) {
	this.durs <- read.csv(paste0("./PilotData/IndividualData/", this.p, "/Exploration/TaskDurations.csv"), header = TRUE)
	this.durs$SubjectNo <- this.p
	durations  	 <- rbind(durations, this.durs)
}

individual.data <- ratings
individual.data$InsideDuration <- durations$InsideDuration

individual.data <- data.table(individual.data)

individual.data[, Surprise := (Interest - Curiosity)]

## Plot the data
ggplot(individual.data, aes(x = Curiosity, y = InsideDuration)) + 
	geom_point(size = 3) +
	stat_smooth(method = "lm", se = FALSE, color = "red") +
	facet_wrap( ~ SubjectNo)



