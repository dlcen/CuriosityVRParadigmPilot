library(ggplot2); library(data.table)

questionnaire <- read.table("Questionnaire.csv", header = T, fileEncoding="UTF-8-BOM")

questionnaire$PC <- rowMeans(questionnaire[, c(2:13)])
questionnaire$EC <- rowMeans(questionnaire[, c(14:23)])
questionnaire$GN <- rowMeans(questionnaire[, c("PC", "EC")])

questionnaire <- data.table(questionnaire)

save(questionnaire, file = "GroupData/QuestionnaireScores.RData")

# Examine individual personality
ggplot(questionnaire, aes(x = SubjectNo, y = PC)) +
	geom_point(size = 2) +
	scale_y_continuous(limits = c(0, 4))
ggsave('Figures/PC_distrib.jpg', width = 3.6, height = 2.4)

ggplot(questionnaire, aes(x = SubjectNo, y = EC)) +
	geom_point(size = 2) +
	scale_y_continuous(limits = c(0, 4))
ggsave('Figures/EC_distrib.jpg', width = 3.6, height = 2.4)