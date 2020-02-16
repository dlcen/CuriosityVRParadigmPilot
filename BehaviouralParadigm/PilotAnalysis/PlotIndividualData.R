
library(data.table); library(ggplot2); library(ggsci); library(cowplot); library(reshape2)

# Ratings

rating.up <- 10

source("./PilotAnalysis/PlotFuncs.R")

## Histogram respectively for *Curiosity* and *Interest* ratings for each participant

for (this.p in participant.list) {

	cur.plot <- ggplot(ratings[SubjectNo == this.p], aes(x = Curiosity)) + 
					geom_histogram(binwidth = 0.5) +
					geom_vline(aes(xintercept = median(Curiosity)), color = "red", size = 1) +
					scale_x_continuous(breaks = c(1:rating.up))+
					scale_y_continuous(breaks = c(0:10)) +
					xlim(0 ,rating.up + 1) + ylim(0, 8) + 
					facet_wrap( ~ SubjectNo) +
					labs(x = "Curiosity Rating", y = "Count") + 
					theme(axis.text = element_text(size = 12),
						  strip.text = element_text(size = 12, face = "bold"))

	int.plot <- ggplot(ratings[SubjectNo == this.p], aes(x = Interest)) + 
					geom_histogram(binwidth = 0.5) +
					geom_vline(aes(xintercept = median(Interest)), color = "red", size = 1) +
					scale_x_continuous(breaks = c(1:rating.up))+
					scale_y_continuous(breaks = c(0:10)) +
					xlim(0, rating.up + 1) + ylim(0, 8) + 
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
					scale_x_continuous(breaks = c(-5:5)) +
					xlim(-5, 5) + ylim(0, 8) + 
					facet_wrap( ~ SubjectNo) +
					labs(x = "Difference between the two ratings", y = "Count") + 
					theme(axis.text = element_text(size = 12),
						  axis.text.y = element_blank(),
						  axis.title.y = element_blank(),
						  axis.ticks.y = element_blank(),
						  strip.text = element_text(size = 12, face = "bold"))

	plot_grid(cur.plot, int.plot, sur.plot, nrow = 1, rel_widths = c(1.15, 1, 1))

	
	if (!dir.exists(paste0("./Figures/IndividualPlots/", this.p))) { dir.create(paste0("./Figures/IndividualPlots/", this.p)) }
	 
	ggsave(paste0("./Figures/IndividualPlots/", this.p, "/", this.p, "_Ratings.png"), width = 12, height = 4)

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

	if (!dir.exists(paste0("./Figures/IndividualPlots/", this.p))) { dir.create(paste0("./Figures/IndividualPlots/", this.p)) }

	ggsave(paste0("./Figures/IndividualPlots/", this.p, "/", this.p, "_RatingsComp.png"), width = 8, height = 5)
}

# Ratings and durations

## Outside the rooms
lmEqn <- function(data, x) {
	m <- lm(sprintf("OutsideDuration ~ %s", x), data = data)

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

	ggplot(individual.data[SubjectNo == this.p], aes(x = Curiosity, y = OutsideDuration)) + 
		geom_point(size = 3) +
		stat_smooth(method = "lm", se = FALSE, color = "red") +
		geom_text(x = 5.5, y = min(individual.data[SubjectNo == this.p]$OutsideDuration) - 5, label = lmEqn(individual.data[SubjectNo == this.p], "Curiosity"), parse = T) +
	    xlim(0, rating.up + 1) + ylim(min(individual.data[SubjectNo == this.p]$OutsideDuration) - 7.5, max(individual.data[SubjectNo == this.p]$OutsideDuration) + 5) +
	    labs(x = "Curiosity rating", y = "Time spent on the pathway (s)") +
		facet_wrap( ~ SubjectNo) +
	    theme(axis.title = element_text(size = 12),
	          strip.text = element_text(size = 12, face = "bold"))

	if (!dir.exists(paste0("./Figures/IndividualPlots/", this.p))) { dir.create(paste0("./Figures/IndividualPlots/", this.p)) }
	ggsave(paste0("./Figures/IndividualPlots/", this.p, "/", this.p, "_RatingsOutsideDurations.png"), width = 6, height = 4)
}

## Inside the rooms
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
		geom_text(x = 5.5, y = min(individual.data[SubjectNo == this.p]$InsideDuration) - 5, label = lmEqn(individual.data[SubjectNo == this.p], "Curiosity"), parse = T) +
	    xlim(0, rating.up + 1) + ylim(min(individual.data[SubjectNo == this.p]$InsideDuration) - 7.5, max(individual.data[SubjectNo == this.p]$InsideDuration)) +
	    labs(x = "Curiosity rating", y = "Time spent inside the room (s)") +
		facet_wrap( ~ SubjectNo) +
	    theme(axis.title = element_text(size = 12),
	          strip.text = element_text(size = 12, face = "bold"))
	  
	int.dur <- ggplot(individual.data[SubjectNo == this.p], aes(x = Interest, y = InsideDuration)) + 
	  	geom_point(size = 3) +
	  	stat_smooth(method = "lm", se = FALSE, color = "red") +
	  	geom_text(x = 5.5, y = min(individual.data[SubjectNo == this.p]$InsideDuration) - 5, label = lmEqn(individual.data[SubjectNo == this.p], "Interest"), parse = T) +
	  	xlim(0, rating.up + 1) + ylim(min(individual.data[SubjectNo == this.p]$InsideDuration) - 7.5, max(individual.data[SubjectNo == this.p]$InsideDuration)) +
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
	  	xlim(-5, 5) + ylim(min(individual.data[SubjectNo == this.p]$InsideDuration) - 7.5, max(individual.data[SubjectNo == this.p]$InsideDuration)) +
	  	labs(x = "Surprise rating", y = "Time spent inside the room (s)") +
	  	facet_wrap( ~ SubjectNo) +
	  	theme(axis.title = element_text(size = 12),
	  		  axis.title.y = element_blank(),
	  		  axis.text.y = element_blank(),
	          strip.text = element_text(size = 12, face = "bold"))

	plot_grid(cur.dur, int.dur, sur.dur, nrow = 1, rel_widths = c(1.2, 1, 1))

	if (!dir.exists(paste0("./Figures/IndividualPlots/", this.p))) { dir.create(paste0("./Figures/IndividualPlots/", this.p)) }
	ggsave(paste0("./Figures/IndividualPlots/", this.p, "/", this.p, "_RatingsDurations.png"), width = 12, height = 4)
}


# Memory performance

if (!is.day1.only) {
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

		ggplot(this.data, aes(x = Response, y = Frequency, color = Group, fill = Group)) + theme_gray() +
			geom_point(aes(group = Group, color = Group, fill = Group), size = 3) +
			geom_line(aes(group = Group, color = Group), size = 0.5) +
			facet_wrap(~ SubjectNo) +
			scale_color_aaas() +
			scale_fill_aaas() +
			theme(strip.text = element_text(face = "bold", size = 12), 
				  legend.position = c(0.1, 0.8))

		if (!dir.exists(paste0("./Figures/IndividualPlots/", this.p))) { dir.create(paste0("./Figures/IndividualPlots/", this.p)) }
		ggsave(paste0("./Figures/IndividualPlots/", this.p, "/", this.p, "_RespFreq.png"), width = 6, height = 4)
	}

	## Ratings and memory performance
	for (this.p in participant.list) {
		this.data <- outside.hit.rate.per.room[SubjectNo == this.p]

		this.cur.plt <- RgLineIdvPlot(this.data, iv="Curiosity", xlab="Curiosity rating")
		this.int.plt <- RgLineIdvPlot(this.data, iv="Interest", xlab="Interestingness rating")

		this.mean.sur <- mean(this.data$Surprise, na.rm = TRUE)
		this.max.sur  <- max(this.data$Surprise, na.rm = TRUE)
		this.min.sur  <- min(this.data$Surprise, na.rm = TRUE) 
		this.sur.plt  <- RgLineIdvPlot(this.data, iv="Surprise", xlab="Surprise", xtxt=this.mean.sur, xbrk=seq(this.min.sur, this.max.sur, 1), xlims=c(this.min.sur, this.max.sur))

		this.mean.exp <- mean(this.data$InsideDuration, na.rm = TRUE)
		this.max.exp  <- ceiling(max(this.data$InsideDuration, na.rm = TRUE))
		this.min.exp  <- floor(min(this.data$InsideDuration, na.rm = TRUE))
		this.exp.plt  <- RgLineIdvPlot(this.data, iv="InsideDuration", xlab="Exploration time (s)", xtxt=this.mean.exp, xbrk=seq(this.min.exp, this.max.exp, 5), xlims=c(this.min.exp, this.max.exp))

		plot_grid(this.cur.plt, 
			this.int.plt + theme(axis.title.y = element_blank()), 
			this.sur.plt + theme(axis.title.y = element_blank()), 
			this.exp.plt + theme(axis.title.y = element_blank()), nrow = 1, rel_widths = c(1.05, 1, 1, 1))

		if (!dir.exists(paste0("./Figures/IndividualPlots/", this.p))) { dir.create(paste0("./Figures/IndividualPlots/", this.p)) }
		ggsave(paste0("./Figures/IndividualPlots//", this.p, "/", this.p, "_RatingsHitRateLinear.png"), width = 18, height = 4)
	}
	

	## Pre-ratings and memory performance

	for (this.p in participant.list) {

		this.data <- outside.hit.rate.per.room[SubjectNo == this.p]

		this.pre.int.plt <- RgLineIdvPlot(this.data, iv="PreInt", xlab="Interestingness rating for previous room")

		this.mean.sur <- mean(this.data$PreSur, na.rm = TRUE)
		this.max.sur  <- max(this.data$PreSur, na.rm = TRUE)
		this.min.sur  <- min(this.data$PreSur, na.rm = TRUE) 
		this.pre.sur.plt  <- RgLineIdvPlot(this.data, iv="PreSur", xlab="Surprise for previous room", xtxt=this.mean.sur, xbrk=seq(this.min.sur, this.max.sur, 1), xlims=c(this.min.sur, this.max.sur))

		plot_grid(this.pre.int.plt, this.pre.sur.plt + theme(axis.title.y = element_blank()), nrow = 1, rel_widths = c(1.05, 1))

		if (!dir.exists(paste0("./Figures/IndividualPlots/", this.p))) { dir.create(paste0("./Figures/IndividualPlots/", this.p)) }
		ggsave(paste0("./Figures/IndividualPlots//", this.p, "/", this.p, "_PreRatingsHitRateLinear.png"), width = 10, height = 5)
	}

	## Curiosity groups and memory performance
	### Median split groups
	BarGrpPlotSep(outside.hit.rate.item.curiosity.median, "CurGrpMd", "SAcc", "Curiosity group (median-splitted)", "Corrected hit rate", "CurMedianHitRate")

	#### Curiosity groups, item order and memory performance
	BarOrdGrpPlotSep(outside.hit.rate.item.order.curiosity.median, "CurGrpMd", "SAcc", "Item order", "Corrected hit rate", "CurOrdHitRate", "Curiosity group", NULL)

	## Relationships between exploration time and memory performance
	### Compare the long- and low-exploration groups
	BarGrpPlotSep(outside.hit.rate.item.exploration.median, "DurGrpMd", "SAcc", "Exploration time group (median-splitted)", "Corrected hit rate", "DurMedianHitRate", c("Short", "Long"))

	#### Include item order
	BarOrdGrpPlotSep(outside.hit.rate.item.order.exploration.median, "DurGrpMd", "SAcc", "Item order", "Corrected hit rate", "DurOrdHitRate", "Exploration time", c("Short", "Long"))

	## Relationiships between interest ratings (median-split groups) and memory performance
	BarGrpPlotSep(outside.hit.rate.item.interest.median, "IntGrpMd", "SAcc", "Interestingness group (median-splitted)", "Corrected hit rate", "IntMedianHitRate")
	BarOrdGrpPlotSep(outside.hit.rate.item.order.interest.median, "IntGrpMd", "SAcc", "Item order", "Corrected hit rate", "IntOrdHitRate", "Interestingness", NULL)

	## Relationiships between surprise (median-split groups) and memory performance
	BarGrpPlotSep(outside.hit.rate.item.surprise.median, "SurGrpMd", "SAcc", "Surprise group (median-splitted)", "Corrected hit rate", "SurMedianHitRate")
	BarOrdGrpPlotSep(outside.hit.rate.item.order.surprise.median, "SurGrpMd", "SAcc", "Item order", "Corrected hit rate", "SurOrdHitRate", "Surprise", NULL)

	## Relationiships between pre-Surerest ratings (median-split groups) and memory performance
	BarGrpPlotSep(outside.hit.rate.item.preint.median, "PreIntGrpMd", "SAcc", "Interest (for previous room) group (median-splitted)", "Corrected hit rate", "PreIntMedianHitRate")
	BarOrdGrpPlotSep(outside.hit.rate.item.order.preint.median, "PreIntGrpMd", "SAcc", "Item order", "Corrected hit rate", "PreIntOrdHitRate", "Pre Interest", NULL)

	## Relationiships between pre-surprise ratings (median-split groups) and memory performance
	BarGrpPlotSep(outside.hit.rate.item.presur.median, "PreSurGrpMd", "SAcc", "Surprise (for previous room) group (median-splitted)", "Corrected hit rate", "PreSurMedianHitRate")
	BarOrdGrpPlotSep(outside.hit.rate.item.order.presur.median, "PreSurGrpMd", "SAcc", "Item order", "Corrected hit rate", "PreSurOrdHitRate", "Pre Surprise", NULL)


}