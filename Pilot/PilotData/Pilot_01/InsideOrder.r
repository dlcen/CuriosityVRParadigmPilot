library(data.table)

# Read the response files for inside objects in each participant folder

parent.folder <- getwd()

participant.folders <- dir(pattern = "^P")

for (p in participant.folders) {
  
  setwd(p)
  
  inside.order <- NULL
  
  rsp.files <- dir(pattern = "*ObjectCollection")
  
  for (f in rsp.files) {
    this.rsp <- read.csv(f)
    this.cdt <- strsplit(f, "_")
    this.rmm <- this.cdt[[1]][1]
    rm.list  <- rep(this.rmm, 6)
    this.tmp <- data.table( Room = rm.list, Object = this.rsp$Object, Order = this.rsp$BoxNo)
    inside.order <- rbind(inside.order, this.tmp)
  }
  
  write.table(inside.order, "InsideOrders.csv", sep = ",", row.names = F)
  
  setwd(parent.folder)
}

