library(data.table); library(ggplot2); library(ggsci); library(cowplot); library(reshape2); library(dplyr)

# Plot bar graphs to compare group means for individual participant
BarIdvPlot <- function(data, comparison, dv, xlab, ylab){
	ggplot(data, aes(get(comparison), get(dv))) + 
			geom_bar( stat="identity", aes(color = get(comparison), fill = get(comparison))) +
			labs(x = xlab, y = ylab) + 
			scale_fill_jama(name = "") +
			scale_color_jama(name = "") +
			facet_wrap(~ SubjectNo) +
			theme( strip.text = element_text(face = "bold", size = 12),
				   legend.position = "none")
}

# Plot bar graphs to compare group means and order groups for individual participant
BarOrdIdvPlot <- function(data, comparison, dv, xlab, ylab, legendgroup){
	ggplot(data, aes(ObjOrdGrp, SAcc)) +
			geom_bar(stat="identity", aes(group = get(comparison), color = get(comparison), fill = get(comparison)), position = position_dodge(width = 0.9)) +
			labs(x = xlab, y = ylab) + 
			scale_fill_jama(name = legendgroup) +
			scale_color_jama(name = legendgroup) +
			facet_wrap(~ SubjectNo) +
			theme( strip.text = element_text(face = "bold", size = 12))
}

# Plot bar graphs to compare group means for a group of participants
BarGrpPlot <- function(data, group, dv, xlab, ylab, figname, newlevels = NULL){
	if (is.data.table(data)) { data <- as.data.frame(data) }

	data[, eval(group)] <- with(data, factor(get(group)))

	if (is.null(newlevels)) {
		data[, eval(group)] <- with(data, factor(get(group), levels = levels(get(group))[c(2, 1)]))
	} else {
		data[, eval(group)] <- with(data, factor(get(group), levels = levels(get(group))[c(2, 1)], labels = newlevels))
	}

	for (this.p in participant.list) {

		this.data <- data[data$SubjectNo == this.p, ]

		BarIdvPlot(this.data, group, dv, xlab, ylab)

		if (!dir.exists(paste0("./Figures/IndividualPlots/", this.p))) { dir.create(paste0("./Figures/IndividualPlots/", this.p)) }
		ggsave(paste0("./Figures/IndividualPlots/", this.p, "/", this.p, "_", figname, ".png"), width = 5, height = 4)

	}

	data <- data.table(data)
}

# Plot bar graphs to compare group and order group means for a group of participants
BarOrdGrpPlot <- function(data, group, dv, xlab, ylab, figname, legendgroup, newlevels = NULL){
	if (is.data.table(data)) { data <- as.data.frame(data) }

	data[, eval(group)] <- with(data, factor(get(group)))

	if (is.null(newlevels)) {
		data[, eval(group)] <- with(data, factor(get(group), levels = levels(get(group))[c(2, 1)]))
	} else {
		data[, eval(group)] <- with(data, factor(get(group), levels = levels(get(group))[c(2, 1)], labels = newlevels))
	}

	for (this.p in participant.list) {

		this.data <- data[data$SubjectNo == this.p, ]

		BarOrdIdvPlot(this.data, group, dv, xlab, ylab, legendgroup)

		if (!dir.exists(paste0("./Figures/IndividualPlots/", this.p))) { dir.create(paste0("./Figures/IndividualPlots/", this.p)) }
		ggsave(paste0("./Figures/IndividualPlots/", this.p, "/", this.p, "_", figname, ".png"), width = 5, height = 4)

	}

	data <- data.table(data)
}