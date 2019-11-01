library(data.table); library(ggplot2); library(extrafont); library(cowplot); library(showtext); library(Hmisc); library(gridExtra)

## Gather the data from the immediate and delayed version.
parent.folder <- "D:/OneDirve_CU/OneDrive - Cardiff University/VRCuriosityMemory/PilotData/RawData"
parent.folder <- "D:/Danlu.C - CU OneDrive/OneDrive - Cardiff University/VRCuriosityMemory/PilotData/RawData"

setwd(paste0(parent.folder, .Platform$file.sep, "Summary"))

load(".RData")

figure.folder <- "Figures"

font_add_google("Open Sans", "Sans")
showtext_auto()

# Outside the rooms
## Items
### Plot Novelty effects
ggplot(OutsideObjectsMemory, aes(x = Group, y = SeenHit)) + theme_minimal() +
	stat_summary(fun.y = mean, geom = "bar", fill = "grey82", color = "black", size = 0.5) +
	stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 0.5) +
	facet_wrap(~ Version) +
	labs(x = "Room novelty", y = "Mean hit rate for \"Seen\" response") +
	theme(axis.line = element_line(colour = "#595959"),
	      axis.title = element_text(family = "Sans", size = 12/0.35),
	      axis.title.x = element_text(vjust = -1),
	      axis.title.y = element_text(vjust = 1),
	      axis.text = element_text(family = "Sans", size = 9/0.35),
	      strip.text = element_text(family = "Sans", size = 16/0.35, face = "bold"))
ggplot2::ggsave(paste(figure.folder, "Outside_Item_Recollection_Novelty.png", sep = .Platform$file.sep), width = 4.8, height = 3.6)

#### Plot Novelty effects and item order
ggplot(Curiosity.Recall.Old, aes(x = ItemOrder, y = SeenHit)) + theme_minimal() +
      stat_summary(fun.y = mean, geom = "point", aes(group = Group, colour = Group), position = position_dodge(width = 0.75), size = 2) +
      stat_summary(fun.data = mean_cl_boot, geom = "errorbar", aes(group = Group, colour = Group), position = position_dodge(width = 0.75), width = 0.2, size = 0.5) +
      stat_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, aes(group = Group, colour = Group), position = position_dodge(width = 0.75)) +
      facet_wrap(~ Version) +
      scale_color_brewer(palette = "Set1", labels = c("Familiar rooms", "Novel rooms")) +
      labs(x = "Item Order", y = "Mean hit rate for \"Seen\" response") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "#595959"),
            axis.title = element_text(family = "Sans", size = 12/0.35),
            axis.title.x = element_text(vjust = -1),
            axis.title.y = element_text(vjust = 1),
            axis.text = element_text(family = "Sans", size = 9/0.35),
            strip.text = element_text(family = "Sans", size = 16/0.35, face = "bold"),
            legend.title = element_blank(),
            legend.text = element_text(family = "Sans", size = 9/0.35, colour = "#595959"),
            legend.position = c(0.85, 0.9))
ggplot2::ggsave(paste(figure.folder, "Outside_Item_Recollection_Novelty_Order.png", sep = .Platform$file.sep), width = 4.8, height = 3.6)

#### Collapsed
ggplot(Curiosity.Recall.Old.Collapsed.Grp, aes(x = ItemOrderType, y = SeenHit)) + theme_minimal() +
      stat_summary(fun.y = mean, geom = "bar", aes(group = Group, fill = Group), position = position_dodge(width = 0.9), size = 2) +
      stat_summary(fun.data = mean_cl_boot, geom = "errorbar", aes(group = Group), position = position_dodge(width = 0.75), width = 0.2, size = 0.5) +
      facet_wrap(~ Version) +
      scale_color_brewer(palette = "Set1", labels = c("Familiar rooms", "Novel rooms")) +
      scale_fill_brewer(palette = "Set1", labels = c("Familiar rooms", "Novel rooms")) +
      labs(x = "Item Order", y = "Mean hit rate for \"Seen\" response") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "#595959"),
            axis.title = element_text(family = "Sans", size = 12/0.35),
            axis.title.x = element_text(vjust = -1),
            axis.title.y = element_text(vjust = 1),
            axis.text = element_text(family = "Sans", size = 9/0.35),
            strip.text = element_text(family = "Sans", size = 16/0.35, face = "bold"),
            legend.title = element_blank(),
            legend.text = element_text(family = "Sans", size = 9/0.35, colour = "#595959"),
            legend.position = c(0.85, 0.9))
ggplot2::ggsave(paste(figure.folder, "Outside_Item_Recollection_Novelty_Order_Collapsed.png", sep = .Platform$file.sep), width = 4.8, height = 3.6)

### Plot Curiosity effects
ggplot(OutsideObjectsMemory, aes(x = CuriosityRating, y = SeenHit)) + theme_minimal() +
      stat_summary(fun.data = mean_cl_boot, na.rm = T, geom = "errorbar", width = 0.2) +
      stat_summary(fun.y = mean, na.rm = T, geom = "point", size = 2) +
      stat_smooth(method = "lm", se = FALSE) +
      scale_x_continuous(breaks = c(1:7)) +
      facet_wrap(~ Version) +
      labs(x = "Curiosity Rating", y = "Mean hit rate for \"Seen\" response") +
      theme(axis.line = element_line(colour = "#595959"),
            axis.title = element_text(family = "Sans", size = 12/0.35),
            axis.title.x = element_text(vjust = -1),
            axis.title.y = element_text(vjust = 1),
            axis.text = element_text(family = "Sans", size = 9/0.35),
            strip.text = element_text(family = "Sans", size = 16/0.35, face = "bold"))
ggplot2::ggsave(paste(figure.folder, "Outside_Item_Recollection_Curiosity.png", sep = .Platform$file.sep), width = 4.8, height = 3.6)

#### Plot Curiosity effects and item order
ggplot(Curiosity.Recall.Old, aes(x = CuriosityRating, y = SeenHit)) + theme_grey() +
      stat_summary(fun.y = mean, geom = "point", size = 2) +
      stat_smooth(method = "lm", se = FALSE) +
      labs(x = "Curiosity rating", y = "Hit rate for \"Seen\" response") +
      scale_x_continuous(breaks = c(1:7)) +
      coord_cartesian(ylim = c(0, 0.8)) +
      facet_grid( Version ~ ItemOrder) +
      theme(title = element_text(family = "Sans", face = "bold", size = 14/0.35),
            strip.text = element_text(family = "Sans", face = "bold", size = 10/0.35),
            strip.background.y = element_rect(size = 10/0.35),
            axis.title = element_text(family = "Sans", size = 11/0.35),
            axis.text = element_text(family = "Sans", size = 9/0.35))

ggplot2::ggsave(paste(figure.folder, "Outside_Item_Recollection_Curiosity_Order.png", sep = .Platform$file.sep), width = 8.1, height = 3.6)

#### Collapsed
ggplot(Curiosity.Recall.Old.Collapsed, aes(x = ItemOrderType, y = SeenHit)) + theme_minimal() +
      stat_summary(fun.y = mean, geom = "bar", aes(group = CuriosityRatingType, fill = CuriosityRatingType), position = position_dodge(width = 0.9), size = 2) +
      stat_summary(fun.data = mean_cl_boot, geom = "errorbar", aes(group = CuriosityRatingType), position = position_dodge(width = 0.9), width = 0.2, size = 0.5) +
      facet_wrap(~ Version) +
      scale_color_brewer(palette = "Set1", labels = c("High Curiosity", "Low Curiosity")) +
      scale_fill_brewer(palette = "Set1", labels = c("High Curiosity", "Low Curiosity")) +
      labs(x = "Item Order", y = "Mean hit rate for \"Seen\" response") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "#595959"),
            axis.title = element_text(family = "Sans", size = 12/0.35),
            axis.title.x = element_text(vjust = -1),
            axis.title.y = element_text(vjust = 1),
            axis.text = element_text(family = "Sans", size = 9/0.35),
            strip.text = element_text(family = "Sans", size = 16/0.35, face = "bold"),
            legend.title = element_blank(),
            legend.text = element_text(family = "Sans", size = 9/0.35, colour = "#595959"),
            legend.position = c(0.85, 0.9))
ggplot2::ggsave(paste(figure.folder, "Outside_Item_Recollection_Curiosity_Order_Collapsed.png", sep = .Platform$file.sep), width = 4.8, height = 3.6)

## Context
### Plot Novelty effects
ggplot(OutsideObjectsMemory, aes(x = Group, y = ContextAccuracy)) + theme_minimal() +
      stat_summary(fun.y = mean, geom = "bar", fill = "grey82", color = "black", size = 0.5) +
      stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 0.5) +
      facet_wrap(~ Version) +
      labs(x = "Room novelty", y = "Mean accuracy for context memory") +
      theme(axis.line = element_line(colour = "#595959"),
            axis.title = element_text(family = "Sans", size = 12/0.35),
            axis.title.x = element_text(vjust = -1),
            axis.title.y = element_text(vjust = 1),
            axis.text = element_text(family = "Sans", size = 9/0.35),
            strip.text = element_text(family = "Sans", size = 16/0.35, face = "bold"))
ggplot2::ggsave(paste(figure.folder, "Outside_Context_Recollection_Novelty.png", sep = .Platform$file.sep), width = 4.8, height = 3.6)

#### Plot Novelty effects and item order
ggplot(Curiosity.Recall.Old, aes(x = ItemOrder, y = ContextAccuracy)) + theme_minimal() +
      stat_summary(fun.y = mean, geom = "point", aes(group = Group, colour = Group), position = position_dodge(width = 0.75), size = 2) +
      stat_summary(fun.data = mean_cl_boot, geom = "errorbar", aes(group = Group, colour = Group), position = position_dodge(width = 0.75), width = 0.2, size = 0.5) +
      stat_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, aes(group = Group, colour = Group), position = position_dodge(width = 0.75)) +
      facet_wrap(~ Version) +
      scale_color_brewer(palette = "Set1", labels = c("Familiar rooms", "Novel rooms")) +
      labs(x = "Item Order", y = "Mean accuracy for context memory") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "#595959"),
            axis.title = element_text(family = "Sans", size = 12/0.35),
            axis.title.x = element_text(vjust = -1),
            axis.title.y = element_text(vjust = 1),
            axis.text = element_text(family = "Sans", size = 9/0.35),
            strip.text = element_text(family = "Sans", size = 16/0.35, face = "bold"),
            legend.title = element_blank(),
            legend.text = element_text(family = "Sans", size = 9/0.35, colour = "#595959"),
            legend.position = c(0.85, 0.9))
ggplot2::ggsave(paste(figure.folder, "Outside_Context_Recollection_Novelty_Order.png", sep = .Platform$file.sep), width = 4.8, height = 3.6)

#### Collapsed
ggplot(Curiosity.Recall.Old.Collapsed.Grp, aes(x = ItemOrderType, y = ContextAccuracy)) + theme_minimal() +
      stat_summary(fun.y = mean, geom = "bar", aes(group = Group, fill = Group), position = position_dodge(width = 0.9), size = 2) +
      stat_summary(fun.data = mean_cl_boot, geom = "errorbar", aes(group = Group), position = position_dodge(width = 0.75), width = 0.2, size = 0.5) +
      facet_wrap(~ Version) +
      scale_color_brewer(palette = "Set1", labels = c("Familiar rooms", "Novel rooms")) +
      scale_fill_brewer(palette = "Set1", labels = c("Familiar rooms", "Novel rooms")) +
      labs(x = "Item Order", y = "Mean accuracy for context memory") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "#595959"),
            axis.title = element_text(family = "Sans", size = 12/0.35),
            axis.title.x = element_text(vjust = -1),
            axis.title.y = element_text(vjust = 1),
            axis.text = element_text(family = "Sans", size = 9/0.35),
            strip.text = element_text(family = "Sans", size = 16/0.35, face = "bold"),
            legend.title = element_blank(),
            legend.text = element_text(family = "Sans", size = 9/0.35, colour = "#595959"),
            legend.position = c(0.85, 0.9))
ggplot2::ggsave(paste(figure.folder, "Outside_Context_Recollection_Novelty_Order_Collapsed.png", sep = .Platform$file.sep), width = 4.8, height = 3.6)

### Plot Curiosity effects
ggplot(OutsideObjectsMemory, aes(x = CuriosityRating, y = ContextAccuracy)) + theme_minimal() +
      stat_summary(fun.data = mean_cl_boot, na.rm = T, geom = "errorbar", width = 0.2) +
      stat_summary(fun.y = mean, na.rm = T, geom = "point", size = 2) +
      stat_smooth(method = "lm", se = FALSE) +
      scale_x_continuous(breaks = c(1:7)) +
      facet_wrap(~ Version) +
      labs(x = "Curiosity Rating", y = "Mean accuracy for context memory") +
      theme(axis.line = element_line(colour = "#595959"),
            axis.title = element_text(family = "Sans", size = 12/0.35),
            axis.title.x = element_text(vjust = -1),
            axis.title.y = element_text(vjust = 1),
            axis.text = element_text(family = "Sans", size = 9/0.35),
            strip.text = element_text(family = "Sans", size = 16/0.35, face = "bold"))
ggplot2::ggsave(paste(figure.folder, "Outside_Context_Recollection_Curiosity.png", sep = .Platform$file.sep), width = 4.8, height = 3.6)

#### Plot Curiosity effects and item order
ggplot(Curiosity.Recall.Old, aes(x = CuriosityRating, y = ContextAccuracy)) + theme_grey() +
      stat_summary(fun.y = mean, geom = "point", size = 2) +
      stat_smooth(method = "lm", se = FALSE) +
      labs(x = "Curiosity rating", y = "Accuracy for context memory") +
      scale_x_continuous(breaks = c(1:7)) +
      coord_cartesian(ylim = c(0, 0.8)) +
      facet_grid( Version ~ ItemOrder) +
      theme(title = element_text(family = "Sans", face = "bold", size = 14/0.35),
            strip.text = element_text(family = "Sans", face = "bold", size = 10/0.35),
            strip.background.y = element_rect(size = 10/0.35),
            axis.title = element_text(family = "Sans", size = 11/0.35),
            axis.text = element_text(family = "Sans", size = 9/0.35))

ggplot2::ggsave(paste(figure.folder, "Outside_Context_Recollection_Curiosity_Order.png", sep = .Platform$file.sep), width = 8.1, height = 3.6)

#### Collapsed
ggplot(Curiosity.Recall.Old.Collapsed, aes(x = ItemOrderType, y = ContextAccuracy)) + theme_minimal() +
      stat_summary(fun.y = mean, geom = "bar", aes(group = CuriosityRatingType, fill = CuriosityRatingType), position = position_dodge(width = 0.9), size = 2) +
      stat_summary(fun.data = mean_cl_boot, geom = "errorbar", aes(group = CuriosityRatingType), position = position_dodge(width = 0.9), width = 0.2, size = 0.5) +
      facet_wrap(~ Version) +
      scale_color_brewer(palette = "Set1", labels = c("High Curiosity", "Low Curiosity")) +
      scale_fill_brewer(palette = "Set1", labels = c("High Curiosity", "Low Curiosity")) +
      labs(x = "Item Order", y = "Mean accuracy for context memory") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "#595959"),
            axis.title = element_text(family = "Sans", size = 12/0.35),
            axis.title.x = element_text(vjust = -1),
            axis.title.y = element_text(vjust = 1),
            axis.text = element_text(family = "Sans", size = 9/0.35),
            strip.text = element_text(family = "Sans", size = 16/0.35, face = "bold"),
            legend.title = element_blank(),
            legend.text = element_text(family = "Sans", size = 9/0.35, colour = "#595959"),
            legend.position = c(0.85, 0.9))
ggplot2::ggsave(paste(figure.folder, "Outside_Context_Recollection_Curiosity_Order_Collapsed.png", sep = .Platform$file.sep), width = 4.8, height = 3.6)


# Inside the rooms
## Items
### Plot Novelty effects
ggplot(InsideObjectsMemory, aes(x = Group, y = SeenHit)) + theme_minimal() +
      stat_summary(fun.y = mean, geom = "bar", fill = "grey82", color = "black", size = 0.5) +
      stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 0.5) +
      facet_wrap(~ Version) +
      labs(x = "Room novelty", y = "Mean hit rate for \"Seen\" response") +
      theme(axis.line = element_line(colour = "#595959"),
            axis.title = element_text(family = "Sans", size = 12/0.35),
            axis.title.x = element_text(vjust = -1),
            axis.title.y = element_text(vjust = 1),
            axis.text = element_text(family = "Sans", size = 9/0.35),
            strip.text = element_text(family = "Sans", size = 16/0.35, face = "bold"))
ggplot2::ggsave(paste(figure.folder, "Inside_Item_Recollection_Novelty.png", sep = .Platform$file.sep), width = 4.8, height = 3.6)

#### Plot Novelty effects and item order
ggplot(Encoding.Recall.Old, aes(x = ItemOrder, y = SeenHit)) + theme_minimal() +
      stat_summary(fun.y = mean, geom = "point", aes(group = Group, colour = Group), position = position_dodge(width = 0.75), size = 2) +
      stat_summary(fun.data = mean_cl_boot, geom = "errorbar", aes(group = Group, colour = Group), position = position_dodge(width = 0.75), width = 0.2, size = 0.5) +
      stat_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, aes(group = Group, colour = Group), position = position_dodge(width = 0.75)) +
      facet_wrap(~ Version) +
      scale_color_brewer(palette = "Set1", labels = c("Familiar rooms", "Novel rooms")) +
      labs(x = "Item Order", y = "Mean hit rate for \"Seen\" response") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "#595959"),
            axis.title = element_text(family = "Sans", size = 12/0.35),
            axis.title.x = element_text(vjust = -1),
            axis.title.y = element_text(vjust = 1),
            axis.text = element_text(family = "Sans", size = 9/0.35),
            strip.text = element_text(family = "Sans", size = 16/0.35, face = "bold"),
            legend.title = element_blank(),
            legend.text = element_text(family = "Sans", size = 9/0.35, colour = "#595959"),
            legend.position = c(0.85, 0.9))
ggplot2::ggsave(paste(figure.folder, "Inside_Item_Recollection_Novelty_Order.png", sep = .Platform$file.sep), width = 4.8, height = 3.6)

#### Collapsed
ggplot(Encoding.Recall.Old.Collapsed.Grp, aes(x = ItemOrderType, y = SeenHit)) + theme_minimal() +
      stat_summary(fun.y = mean, geom = "bar", aes(group = Group, fill = Group), position = position_dodge(width = 0.9), size = 2) +
      stat_summary(fun.data = mean_cl_boot, geom = "errorbar", aes(group = Group), position = position_dodge(width = 0.75), width = 0.2, size = 0.5) +
      facet_wrap(~ Version) +
      scale_color_brewer(palette = "Set1", labels = c("Familiar rooms", "Novel rooms")) +
      scale_fill_brewer(palette = "Set1", labels = c("Familiar rooms", "Novel rooms")) +
      labs(x = "Item Order", y = "Mean hit rate for \"Seen\" response") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "#595959"),
            axis.title = element_text(family = "Sans", size = 12/0.35),
            axis.title.x = element_text(vjust = -1),
            axis.title.y = element_text(vjust = 1),
            axis.text = element_text(family = "Sans", size = 9/0.35),
            strip.text = element_text(family = "Sans", size = 16/0.35, face = "bold"),
            legend.title = element_blank(),
            legend.text = element_text(family = "Sans", size = 9/0.35, colour = "#595959"),
            legend.position = c(0.85, 0.9))
ggplot2::ggsave(paste(figure.folder, "Inside_Item_Recollection_Novelty_Order_Collapsed.png", sep = .Platform$file.sep), width = 4.8, height = 3.6)

### Plot Curiosity effects
ggplot(InsideObjectsMemory, aes(x = CuriosityRating, y = SeenHit)) + theme_minimal() +
      stat_summary(fun.data = mean_cl_boot, na.rm = T, geom = "errorbar", width = 0.2) +
      stat_summary(fun.y = mean, na.rm = T, geom = "point", size = 2) +
      stat_smooth(method = "lm", se = FALSE) +
      scale_x_continuous(breaks = c(1:7)) +
      facet_wrap(~ Version) +
      labs(x = "Curiosity Rating", y = "Mean hit rate for \"Seen\" response") +
      theme(axis.line = element_line(colour = "#595959"),
            axis.title = element_text(family = "Sans", size = 12/0.35),
            axis.title.x = element_text(vjust = -1),
            axis.title.y = element_text(vjust = 1),
            axis.text = element_text(family = "Sans", size = 9/0.35),
            strip.text = element_text(family = "Sans", size = 16/0.35, face = "bold"))
ggplot2::ggsave(paste(figure.folder, "Inside_Item_Recollection_Curiosity.png", sep = .Platform$file.sep), width = 4.8, height = 3.6)

#### Plot Curiosity effects and item order
ggplot(Encoding.Recall.Old, aes(x = CuriosityRating, y = SeenHit)) + theme_grey() +
      stat_summary(fun.y = mean, geom = "point", size = 2) +
      stat_smooth(method = "lm", se = FALSE) +
      labs(x = "Curiosity rating", y = "Hit rate for \"Seen\" response") +
      scale_x_continuous(breaks = c(1:7)) +
      coord_cartesian(ylim = c(0, 1)) +
      facet_grid( Version ~ ItemOrder) +
      theme(title = element_text(family = "Sans", face = "bold", size = 14/0.35),
            strip.text = element_text(family = "Sans", face = "bold", size = 10/0.35),
            strip.background.y = element_rect(size = 10/0.35),
            axis.title = element_text(family = "Sans", size = 11/0.35),
            axis.text = element_text(family = "Sans", size = 9/0.35))

ggplot2::ggsave(paste(figure.folder, "Inside_Item_Recollection_Curiosity_Order.png", sep = .Platform$file.sep), width = 8.1, height = 3.6)

#### Collapsed
ggplot(Encoding.Recall.Old.Collapsed, aes(x = ItemOrderType, y = SeenHit)) + theme_minimal() +
      stat_summary(fun.y = mean, geom = "bar", aes(group = CuriosityRatingType, fill = CuriosityRatingType), position = position_dodge(width = 0.9), size = 2) +
      stat_summary(fun.data = mean_cl_boot, geom = "errorbar", aes(group = CuriosityRatingType), position = position_dodge(width = 0.9), width = 0.2, size = 0.5) +
      facet_wrap(~ Version) +
      scale_color_brewer(palette = "Set1", labels = c("High Curiosity", "Low Curiosity")) +
      scale_fill_brewer(palette = "Set1", labels = c("High Curiosity", "Low Curiosity")) +
      labs(x = "Item Order", y = "Mean hit rate for \"Seen\" response") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "#595959"),
            axis.title = element_text(family = "Sans", size = 12/0.35),
            axis.title.x = element_text(vjust = -1),
            axis.title.y = element_text(vjust = 1),
            axis.text = element_text(family = "Sans", size = 9/0.35),
            strip.text = element_text(family = "Sans", size = 16/0.35, face = "bold"),
            legend.title = element_blank(),
            legend.text = element_text(family = "Sans", size = 9/0.35, colour = "#595959"),
            legend.position = c(0.85, 0.9))
ggplot2::ggsave(paste(figure.folder, "Inside_Item_Recollection_Curiosity_Order_Collapsed.png", sep = .Platform$file.sep), width = 4.8, height = 3.6)

## Context
### Plot Novelty effects
ggplot(InsideObjectsMemory, aes(x = Group, y = ContextAccuracy)) + theme_minimal() +
      stat_summary(fun.y = mean, geom = "bar", fill = "grey82", color = "black", size = 0.5) +
      stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 0.5) +
      facet_wrap(~ Version) +
      labs(x = "Room novelty", y = "Mean accuracy for context memory") +
      theme(axis.line = element_line(colour = "#595959"),
            axis.title = element_text(family = "Sans", size = 12/0.35),
            axis.title.x = element_text(vjust = -1),
            axis.title.y = element_text(vjust = 1),
            axis.text = element_text(family = "Sans", size = 9/0.35),
            strip.text = element_text(family = "Sans", size = 16/0.35, face = "bold"))
ggplot2::ggsave(paste(figure.folder, "Inside_Context_Recollection_Novelty.png", sep = .Platform$file.sep), width = 4.8, height = 3.6)

#### Plot Novelty effects and item order
ggplot(Encoding.Recall.Old, aes(x = ItemOrder, y = ContextAccuracy)) + theme_minimal() +
      stat_summary(fun.y = mean, geom = "point", aes(group = Group, colour = Group), position = position_dodge(width = 0.75), size = 2) +
      stat_summary(fun.data = mean_cl_boot, geom = "errorbar", aes(group = Group, colour = Group), position = position_dodge(width = 0.75), width = 0.2, size = 0.5) +
      stat_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, aes(group = Group, colour = Group), position = position_dodge(width = 0.75)) +
      facet_wrap(~ Version) +
      scale_color_brewer(palette = "Set1", labels = c("Familiar rooms", "Novel rooms")) +
      labs(x = "Item Order", y = "Mean accuracy for context memory") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "#595959"),
            axis.title = element_text(family = "Sans", size = 12/0.35),
            axis.title.x = element_text(vjust = -1),
            axis.title.y = element_text(vjust = 1),
            axis.text = element_text(family = "Sans", size = 9/0.35),
            strip.text = element_text(family = "Sans", size = 16/0.35, face = "bold"),
            legend.title = element_blank(),
            legend.text = element_text(family = "Sans", size = 9/0.35, colour = "#595959"),
            legend.position = c(0.85, 0.9))
ggplot2::ggsave(paste(figure.folder, "Inside_Context_Recollection_Novelty_Order.png", sep = .Platform$file.sep), width = 4.8, height = 3.6)

#### Collapsed
ggplot(Encoding.Recall.Old.Collapsed.Grp, aes(x = ItemOrderType, y = ContextAccuracy)) + theme_minimal() +
      stat_summary(fun.y = mean, geom = "bar", aes(group = Group, fill = Group), position = position_dodge(width = 0.9), size = 2) +
      stat_summary(fun.data = mean_cl_boot, geom = "errorbar", aes(group = Group), position = position_dodge(width = 0.75), width = 0.2, size = 0.5) +
      facet_wrap(~ Version) +
      scale_color_brewer(palette = "Set1", labels = c("Familiar rooms", "Novel rooms")) +
      scale_fill_brewer(palette = "Set1", labels = c("Familiar rooms", "Novel rooms")) +
      labs(x = "Item Order", y = "Mean accuracy for context memory") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "#595959"),
            axis.title = element_text(family = "Sans", size = 12/0.35),
            axis.title.x = element_text(vjust = -1),
            axis.title.y = element_text(vjust = 1),
            axis.text = element_text(family = "Sans", size = 9/0.35),
            strip.text = element_text(family = "Sans", size = 16/0.35, face = "bold"),
            legend.title = element_blank(),
            legend.text = element_text(family = "Sans", size = 9/0.35, colour = "#595959"),
            legend.position = c(0.85, 0.9))
ggplot2::ggsave(paste(figure.folder, "Inside_Context_Recollection_Novelty_Order_Collapsed.png", sep = .Platform$file.sep), width = 4.8, height = 3.6)

### Plot Curiosity effects
ggplot(InsideObjectsMemory, aes(x = CuriosityRating, y = ContextAccuracy)) + theme_minimal() +
      stat_summary(fun.data = mean_cl_boot, na.rm = T, geom = "errorbar", width = 0.2) +
      stat_summary(fun.y = mean, na.rm = T, geom = "point", size = 2) +
      stat_smooth(method = "lm", se = FALSE) +
      scale_x_continuous(breaks = c(1:7)) +
      facet_wrap(~ Version) +
      labs(x = "Curiosity Rating", y = "Mean accuracy for context memory") +
      theme(axis.line = element_line(colour = "#595959"),
            axis.title = element_text(family = "Sans", size = 12/0.35),
            axis.title.x = element_text(vjust = -1),
            axis.title.y = element_text(vjust = 1),
            axis.text = element_text(family = "Sans", size = 9/0.35),
            strip.text = element_text(family = "Sans", size = 16/0.35, face = "bold"))
ggplot2::ggsave(paste(figure.folder, "Inside_Context_Recollection_Curiosity.png", sep = .Platform$file.sep), width = 4.8, height = 3.6)

#### Plot Curiosity effects and item order
ggplot(Encoding.Recall.Old, aes(x = CuriosityRating, y = ContextAccuracy)) + theme_grey() +
      stat_summary(fun.y = mean, geom = "point", size = 2) +
      stat_smooth(method = "lm", se = FALSE) +
      labs(x = "Curiosity rating", y = "Accuracy for context memory") +
      scale_x_continuous(breaks = c(1:7)) +
      coord_cartesian(ylim = c(0, 0.8)) +
      facet_grid( Version ~ ItemOrder) +
      theme(title = element_text(family = "Sans", face = "bold", size = 14/0.35),
            strip.text = element_text(family = "Sans", face = "bold", size = 10/0.35),
            strip.background.y = element_rect(size = 10/0.35),
            axis.title = element_text(family = "Sans", size = 11/0.35),
            axis.text = element_text(family = "Sans", size = 9/0.35))

ggplot2::ggsave(paste(figure.folder, "Inside_Context_Recollection_Curiosity_Order.png", sep = .Platform$file.sep), width = 8.1, height = 3.6)

#### Collapsed
ggplot(Encoding.Recall.Old.Collapsed, aes(x = ItemOrderType, y = ContextAccuracy)) + theme_minimal() +
      stat_summary(fun.y = mean, geom = "bar", aes(group = CuriosityRatingType, fill = CuriosityRatingType), position = position_dodge(width = 0.9), size = 2) +
      stat_summary(fun.data = mean_cl_boot, geom = "errorbar", aes(group = CuriosityRatingType), position = position_dodge(width = 0.9), width = 0.2, size = 0.5) +
      facet_wrap(~ Version) +
      scale_color_brewer(palette = "Set1", labels = c("High Curiosity", "Low Curiosity")) +
      scale_fill_brewer(palette = "Set1", labels = c("High Curiosity", "Low Curiosity")) +
      labs(x = "Item Order", y = "Mean accuracy for context memory") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "#595959"),
            axis.title = element_text(family = "Sans", size = 12/0.35),
            axis.title.x = element_text(vjust = -1),
            axis.title.y = element_text(vjust = 1),
            axis.text = element_text(family = "Sans", size = 9/0.35),
            strip.text = element_text(family = "Sans", size = 16/0.35, face = "bold"),
            legend.title = element_blank(),
            legend.text = element_text(family = "Sans", size = 9/0.35, colour = "#595959"),
            legend.position = c(0.85, 0.9))
ggplot2::ggsave(paste(figure.folder, "Inside_Context_Recollection_Curiosity_Order_Collapsed.png", sep = .Platform$file.sep), width = 4.8, height = 3.6)
