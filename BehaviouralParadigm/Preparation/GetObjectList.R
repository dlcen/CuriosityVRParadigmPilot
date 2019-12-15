library(data.table)

setwd("C:/Users/c1254825/Desktop/Git/CuriosityVRBehaviouralParadigmEncodingTask/EncodingTask/Assets/Prefabs/Objects")

setwd("C:/Users/c1254825/Desktop/Git/CuriosityVRBehaviouralParadigmEncodingTask/EncodingTask/Assets/Resources/OutsideObjects")

setwd("C:/Users/c1254825/Desktop/Git/CuriosityVRBehaviouralParadigmEncodingTask/EncodingTask/Assets/Resources/OutsideObjects")

present.objects <- list.files(pattern = "\\.prefab$")
present.object.list <- data.table(Object = sapply(present.objects, function(x) strsplit(x, ".prefab")[[1]], USE.NAMES = FALSE))

write.csv(present.object.list, "C:/Users/c1254825/Desktop/Git/CuriosityVRParadigm/BehaviouralParadigm/Preparation/AllObjectList.csv", row.names = FALSE)

inside.objects <- list.files(pattern = "\\.prefab$")
inside.object.list <- data.table(Object = sapply(inside.objects, function(x) strsplit(x, ".prefab")[[1]], USE.NAMES = FALSE))

outside.objects <- list.files(pattern = "\\.prefab$")
outside.object.list <- data.table(Object = sapply(outside.objects, function(x) strsplit(x, ".prefab")[[1]], USE.NAMES = FALSE))

object.list <- rbind(inside.object.list, outside.object.list)

