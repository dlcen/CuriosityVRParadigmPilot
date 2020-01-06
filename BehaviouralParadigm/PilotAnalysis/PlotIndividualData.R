library(data.table); library(ggplot2); library(ggsci); library(cowplot); library(reshape2)

# Ratings

## Histogram respectively for *Curiosity* and *Interest* ratings for each participant

for (this.p in participant.list) {

	cur.plot <- ggplot(ratings[SubjectNo == this.p], aes(x = Curiosity)) + 
					geom_histogram(binwidth = 0.5) +
					geom_vline(aes(xintercept = median(Curiosity)), color = "red", size = 1) +
					scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6))+
					scale_y_continuous(breaks = c(0:10)) +
					xlim(0 ,7) + ylim(0, 8) + 
					facet_wrap( ~ SubjectNo) +
					labs(x = "Curiosity Rating", y = "Count") + 
					theme(axis.text = element_text(size = 12),
						  strip.text = element_text(size = 12, face = "bold"))

	int.plot <- ggplot(ratings[SubjectNo == this.p], aes(x = Interest)) + 
					geom_histogram(binwidth = 0.5) +
					geom_vline(aes(xintercept = median(Interest)), color = "red", size = 1) +
					scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6))+
					scale_y_continuous(breaks = c(0:10)) +
					xlim(0, 7) + ylim(0, 8) + 
					facet_wrap( ~ SubjectNo) +
					labs(x = "Interest Rating", y = "Count") + 
					theme(axis.text = element_text(size = 12),
						  axis.text.y = element_blank(),
						  axis.title.y = element_blank(),
						  axis.ticks.y = element_blank(),
						  strip.text = element_text(size = 12, face = "bold"))

	sur.plot <- ggplot(ratings[SubjectNo == this.p], aes(x = Surprise)) + 
					geom_histogram(binwidth = 0.5) +
					geom_vline(aes(xintercept = median(Surprise)), color = "red", size = 1) +
					scale_x_continuous(breaks = c(-4:4)) +
					xlim(-4, 4) + ylim(0, 8) + 
					facet_wrap( ~ SubjectNo) +
					labs(x = "Difference between the two ratings", y = "Count") + 
					theme(axis.text = element_text(size = 12),
						  axis.text.y = element_blank(),
						  axis.title.y = element_blank(),
						  axis.ticks.y = element_blank(),
						  strip.text = element_text(size = 12, face = "bold"))

	plot_grid(cur.plot, int.plot, sur.plot, nrow = 1, rel_widths = c(1.15, 1, 1))

	ggsave(paste0("./Figures/IndividualPlots/", this.p, "_Ratings.png"), width = 12, height = 4)
}


## Ratings for each room
library(ggnewscale)

signToCharacter <- function(data) {
	if (data == -1) {outputChar = "Negative"}
	else if (data == 1) {outputChar = "Posistive"}
	else {outputChar = NA}
}

ratings.long <- melt(data = ratings[, c(1:4)], id.vars = c("SubjectNo", "Room"), measure.vars = c("Curiosity", "Interest"), variable.name = c("RatingType"), value.name = c("RatingVal"))

for (this.p in participant.list) {

	sur.sign <- rep(sign(ratings[SubjectNo == this.p]$Surprise), 2)

	sur.sign.char <- sapply(sur.sign, signToCharacter)

	ggplot(ratings.long[SubjectNo == this.p], aes(x = Room, y = RatingVal)) +
		geom_line(aes(group = ratings.long[SubjectNo == this.p]$Room, color = sur.sign.char)) +
		scale_color_jama(palette = "default", name = "Sign of Difference", na.translate = FALSE) +

		new_scale_color() +

		geom_point(aes(group = RatingType, colour = RatingType, fill = RatingType, size = RatingType)) + 
		scale_color_uchicago("default", name = "Rating") +
		scale_fill_uchicago("default", name = "Rating") +
		scale_size_manual(values = c(3, 2), name = "Rating") +

		facet_wrap( ~ SubjectNo) +

		labs(y = "Rating score") +
		theme( axis.text.x = element_text(angle = 60, vjust = 0.5),
			   strip.text = element_text(face = "bold", size = 12)
			  )

	ggsave(paste0("./Figures/IndividualPlots/", this.p, "_RatingsComp.png"), width = 8, height = 5)
}

# Ratings and durations
lmEqn <- function(data, x) {
	m <- lm(sprintf("InsideDuration ~ %s", x), data = data)

	if(coef(m)[2] > 0) {
		eq <- substitute(italic(y) == a + b%.%italic(x)*","~~italic(adjusted)~italic(r)^2~"="~r2,
						 list(a = format(unname(coef(m)[1]), digits = 4),
						 	 b = format(unname(abs(coef(m)[2])), digits = 3),
						 	r2 = format(summary(m)$r.squared, digits = 3)))
	} else {
		eq <- substitute(italic(y) == a - b%.%italic(x)*","~~italic(adjusted)~italic(r)^2~"="~r2,
						 list(a = format(unname(coef(m)[1]), digits = 4),
						 	 b = format(unname(abs(coef(m)[2])), digits = 3),
						 	r2 = format(summary(m)$r.squared, digits = 3)))
	}

	as.character(as.expression(eq))
}

for (this.p in participant.list) {

	cur.dur <- ggplot(individual.data[SubjectNo == this.p], aes(x = Curiosity, y = InsideDuration)) + 
		geom_point(size = 3) +
		stat_smooth(method = "lm", se = FALSE, color = "red") +
		geom_text(x = 3.5, y = min(individual.data[SubjectNo == this.p]$InsideDuration) - 5, label = lmEqn(individual.data[SubjectNo == this.p], "Curiosity"), parse = T) +
	    xlim(0, 7) + ylim(min(individual.data[SubjectNo == this.p]$InsideDuration) - 7.5, max(individual.data[SubjectNo == this.p]$InsideDuration)) +
	    labs(x = "Curiosity rating", y = "Time spent inside the room (s)") +
		facet_wrap( ~ SubjectNo) +
	    theme(axis.title = element_text(size = 12),
	          strip.text = element_text(size = 12, face = "bold"))
	  
	int.dur <- ggplot(individual.data[SubjectNo == this.p], aes(x = Interest, y = InsideDuration)) + 
	  	geom_point(size = 3) +
	  	stat_smooth(method = "lm", se = FALSE, color = "red") +
	  	geom_text(x = 3.5, y = min(individual.data[SubjectNo == this.p]$InsideDuration) - 5, label = lmEqn(individual.data[SubjectNo == this.p], "Interest"), parse = T) +
	  	xlim(0, 7) + ylim(min(individual.data[SubjectNo == this.p]$InsideDuration) - 7.5, max(individual.data[SubjectNo == this.p]$InsideDuration)) +
	  	labs(x = "Interest rating", y = "Time spent inside the room (s)") +
	  	facet_wrap( ~ SubjectNo) +
	  	theme(axis.title = element_text(size = 12),
	  		  axis.title.y = element_blank(),
	  		  axis.text.y = element_blank(),
	          strip.text = element_text(size = 12, face = "bold"))

	sur.dur <- ggplot(individual.data[SubjectNo == this.p], aes(x = Surprise, y = InsideDuration)) + 
	  	geom_point(size = 3) +
	  	stat_smooth(method = "lm", se = FALSE, color = "red") +
	  	geom_text(x = 0, y = min(individual.data[SubjectNo == this.p]$InsideDuration) - 5, label = lmEqn(individual.data[SubjectNo == this.p], "Surprise"), parse = T) +
	  	xlim(-4, 4) + ylim(min(individual.data[SubjectNo == this.p]$InsideDuration) - 7.5, max(individual.data[SubjectNo == this.p]$InsideDuration)) +
	  	labs(x = "Surprise rating", y = "Time spent inside the room (s)") +
	  	facet_wrap( ~ SubjectNo) +
	  	theme(axis.title = element_text(size = 12),
	  		  axis.title.y = element_blank(),
	  		  axis.text.y = element_blank(),
	          strip.text = element_text(size = 12, face = "bold"))

	plot_grid(cur.dur, int.dur, sur.dur, nrow = 1, rel_widths = c(1.2, 1, 1))

	ggsave(paste0("./Figures/IndividualPlots/", this.p, "_RatingsDurations.png"), width = 12, height = 4)
}

# Memory performance

## Percentage of option selection for "Old", "Familiar" and "New" respectively for old and new items.
RespFreqCal <- function(ObjResp, ... ){
  FreqTable <- as.data.frame(table(ObjResp)/sum(table(ObjResp)))
  names(FreqTable) <- c("Response", "Frequency")
  return(FreqTable)
}

Recall.Freq.Rsp <- object.recognition[, c(RespFreqCal(Response)), by = c("SubjectNo", "Group")]

Recall.Freq.Rsp$Response <- factor(Recall.Freq.Rsp$Response, levels = levels(Recall.Freq.Rsp$Response)[c(3, 1, 2)])


for (this.p in participant.list) {
	this.data <- Recall.Freq.Rsp[SubjectNo == this.p]

	ggplot(this.data, aes(x = Response, y = Frequency, color = Group, fill = Group)) +
		geom_point(aes(group = Group, color = Group, fill = Group), size = 3) +
		geom_line(aes(group = Group, color = Group), size = 0.5) +
		facet_wrap(~ SubjectNo) +
		scale_color_aaas() +
		scale_fill_aaas() +
		theme(strip.text = element_text(face = "bold", size = 12), 
			  legend.position = c(0.1, 0.9))

	ggsave(paste0("./Figures/IndividualPlots/", this.p, "_RespFreq.png"), width = 6, height = 4)
}