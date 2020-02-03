library(data.table); library(ggplot2); library(cowplot); library(dplyr)

load("./PilotData/IndividualData.RData")

rating.up <- 10 

# Check the distribution of ratings for each room
ratings %>%
	group_by(Room) %>%
	mutate(CurMed = median(as.numeric(Curiosity))) %>%
	ggplot(aes(x = Curiosity)) + 
		geom_histogram(binwidth = 0.5) +
		geom_vline(aes(xintercept = CurMed), color = "red", size = 1) +
		scale_x_continuous(breaks = c(1:rating.up))+
		scale_y_continuous(breaks = c(0:10)) +
		# xlim(0 ,rating.up + 1) + ylim(0, 8) + 
		facet_wrap( ~ Room, ncol = 1) +
		labs(x = "Curiosity Rating", y = "Count") + 
		theme(axis.text = element_text(size = 12),
		      strip.text = element_text(size = 12, face = "bold"))


ggsave(paste0("./Figures/CurRatingsPerRoom.png"), width = 3, height = 15)

ratings %>%
	group_by(Room) %>%
	mutate(CurMed = median(as.numeric(Interest))) %>%
	ggplot(aes(x = Interest)) + 
		geom_histogram(binwidth = 0.5) +
		geom_vline(aes(xintercept = CurMed), color = "red", size = 1) +
		scale_x_continuous(breaks = c(1:rating.up))+
		scale_y_continuous(breaks = c(0:10)) +
		# xlim(0 ,rating.up + 1) + ylim(0, 8) + 
		facet_wrap( ~ Room, ncol = 1) +
		labs(x = "Interestingness Rating", y = "Count") + 
		theme(axis.text = element_text(size = 12),
		      strip.text = element_text(size = 12, face = "bold"))


ggsave(paste0("./Figures/IntRatingsPerRoom.png"), width = 3, height = 15)