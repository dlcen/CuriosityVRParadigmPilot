library(data.table); library(ggplot2)

# The function to plot indiividual data

IndividualDataPlot <- function(dat, iv, dv, fa, ylab, figName) {
	this.plot <- ggplot(data = dat, aes(x = iv, y = dv)) + 
		geom_point(size = 2, aes(color = Group)) +
		geom_hline(aes(yintercept = fa), size = 0.75, linetype = "dashed") +
		geom_hline(yintercept = 0, colour = "#595959") +
		facet_wrap( ~ SubjectNo, nrow = 8) +
		labs(x = "Room", y = "Hit rate for \"Seen\" response") +
		scale_shape_discrete(name = "source") +
		theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

	ggplot2::ggsave(paste0("Together/Figures/Indiividual plots", figName, ".png"), plot = this.plot, width=18, height=18, dpi = 300, units = "cm")
}
