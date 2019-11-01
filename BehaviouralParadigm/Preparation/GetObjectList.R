inside.objects <- list.files(pattern = "\\.prefab$")
inside.object.list <- data.table(Object = sapply(inside.objects, function(x) strsplit(x, ".prefab")[[1]], USE.NAMES = FALSE))

outside.objects <- list.files(pattern = "\\.prefab$")
outside.object.list <- data.table(Object = sapply(outside.objects, function(x) strsplit(x, ".prefab")[[1]], USE.NAMES = FALSE))

object.list <- rbind(inside.object.list, outside.object.list)