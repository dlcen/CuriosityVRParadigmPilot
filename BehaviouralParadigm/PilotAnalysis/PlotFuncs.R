library(data.table); library(ggplot2); library(ggsci); library(cowplot); library(reshape2); library(dplyr); library(Hmisc)

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
BarGrpPlotSep <- function(data, group, dv, xlab, ylab, figname, newlevels = NULL){
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

# Plot bar graphs to compare group and order group means for a group of individual participants
BarOrdGrpPlotSep <- function(data, group, dv, xlab, ylab, figname, legendgroup, newlevels = NULL){
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

# Plot linear relationships
## Function to calculate the linear regression parameters
lmEqnFunc <- function(data, iv, dv = "SAcc") {

	m <- lm(get(dv) ~ get(iv), data = data, na.action=na.omit)

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

## Function to plot individual regression lines
RgLineIdvPlot <- function(data, iv, dv = "SAcc", xlab, ylab = "Corrected hit rate", xtxt = 5.5, ytxt = 0.7, xbrk = seq(1, 10, 1), xlims = c(1, 10), ylims = c(-0.25, 0.75), is.individual = TRUE) {
	this.plt <- ggplot(data, aes(get(iv), get(dv))) + theme_gray() +
					geom_point(size = 2) +
					stat_smooth(method = "lm", se = FALSE, color = "red") +
					geom_text(x = xtxt, y = ytxt, aes(label = lmEqnFunc(data, iv, dv)), parse = T) +
					scale_x_continuous(breaks = xbrk, limits = xlims) +
					scale_y_continuous(limits = ylims) +
					labs(x = xlab, y = ylab)

	if (is.individual) {
		this.plt <- this.plt + facet_wrap( ~ SubjectNo, ncol = 1) + theme( strip.text = element_text(face = "bold", size = 12))
	}		

	return(this.plt)
}

# Plot bar comparison graphs for group data (together)
BarGrpPlotTog <- function(data, group, dv = "SAcc", xlab, ylab = "Corrected hit rate", figname, newlevels = NULL) {
	if (is.data.table(data)) { data <- as.data.frame(data) }

	data[, eval(group)] <- with(data, factor(get(group)))

	if (is.null(newlevels)) {
		data[, eval(group)] <- with(data, factor(get(group), levels = levels(get(group))[c(2, 1)]))
	} else {
		data[, eval(group)] <- with(data, factor(get(group), levels = levels(get(group))[c(2, 1)], labels = newlevels))
	}

	ggplot(data, aes(get(group), get(dv))) +
		stat_summary(fun.y = "mean", geom = "bar", aes(color = get(group), fill = get(group))) +
		stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, size = 1, color = "grey50") +
		labs(x = xlab, y = ylab) +
		scale_fill_jama(name = "") +
		scale_colour_jama(name = "") +
		theme(legend.position = "none")

	ggsave(paste0("./Figures/", figname, ".png"), width = 5, height = 4)
}

# Plot bar graphs to compare group and order group means for a group as a whole
BarOrdGrpPlotTog <- function(data, group, dv = "SAcc", xlab = "Item order", ylab = "Corrected hit rate", figname, legendgroup, newlevels = NULL) {
	if (is.data.table(data)) { data <- as.data.frame(data) }

	data[, eval(group)] <- with(data, factor(get(group)))

	if (is.null(newlevels)) {
		data[, eval(group)] <- with(data, factor(get(group), levels = levels(get(group))[c(2, 1)]))
	} else {
		data[, eval(group)] <- with(data, factor(get(group), levels = levels(get(group))[c(2, 1)], labels = newlevels))
	}

	ggplot(data, aes(ObjOrdGrp, get(dv))) +
		stat_summary(fun.y = "mean", geom = "bar", aes(group = get(group), color = get(group), fill = get(group)), position = position_dodge(width = 0.95)) +
		stat_summary(fun.data = mean_cl_normal, geom = "errorbar", aes(group = get(group)), width = 0.2, size = 1, color = "grey50", position = position_dodge(width = 0.95)) +
		geom_hline(yintercept = 0, size = 0.5, color = "grey25") +
		labs(x = xlab, y = ylab) +
		scale_fill_jama(name = legendgroup) +
		scale_colour_jama(name = legendgroup) 

	ggsave(paste0("./Figures/", figname, ".png"), width = 5, height = 4)
}


