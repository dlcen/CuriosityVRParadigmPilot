library(data.table)

# Inside item orders

parent.folder <- "D:\\OneDirve_CU\\OneDrive - Cardiff University\\VRCuriosityMemory\\PilotData\\RawData\\Pilot_01"
parent.folder <- "D:/Danlu.C - CU OneDrive/OneDrive - Cardiff University/Projects/VRCuriosityMemory/PilotData/RawData/Pilot_01"
parent.folder <- "~/Desktop/VRCuriosityMemory/PilotData/RawData/Pilot_01"
parent.folder <- "~/Documents/OneDrive - Cardiff University/Projects/VRCuriosityMemory/PilotData/RawData/Pilot_01/"

setwd(parent.folder)

participant.folders <- dir(pattern = "^P")

for (this.p in participant.folders) {

	this.inside.folder <- paste(c(this.p, "ConditionOrders", "Inside"), collapse = .Platform$file.sep)

	setwd(this.inside.folder)
	
	if (! (file.exists("InsideOrders.csv"))) {

        	inside.item.orders <- dir(pattern = "\\.csv$")
        
        	this.p.orders <- NULL
        
        	for (this.order in inside.item.orders) {
        		this.room <- strsplit(this.order, split = '.csv')[[1]]
        
        		this.order.list <- readLines(this.order, warn = F)
        		this.order.list <- strsplit( this.order.list[1], split = "," )[[1]]
        		this.order.no   <- c(1:6)
        		this.temp <- data.frame(Item = this.order.list, Order = this.order.no)
        
        		this.temp$SubjectNo <- this.p
        		this.temp$Room <- this.room
        
        		this.p.orders <- rbind(this.p.orders, this.temp)
        	}
        
        	write.table(this.p.orders, "InsideOrders.csv", row.names = FALSE)
	}
	setwd(parent.folder)
}

# Outside item orders
parent.folder <- "D:\\OneDirve_CU\\OneDrive - Cardiff University\\VRCuriosityMemory\\PilotData\\RawData\\Pilot_01"
parent.folder <- "~/Desktop/VRCuriosityMemory/PilotData/RawData/Pilot_01"

setwd(parent.folder)

participant.folders <- dir(pattern = "^P")

for (this.p in participant.folders) {

	this.outside.folder <- paste(c(this.p, "ConditionOrders", "Outside"), collapse = .Platform$file.sep)

	setwd(this.outside.folder)
  
	if (! (file.exists("OutsideOrders.csv"))) {
	  outside.item.orders <- list.files(pattern = "\\.csv$", ignore.case = TRUE)
	  
	  this.p.orders <- NULL
	  
	  for (this.order in outside.item.orders) {
	    this.room <- strsplit(this.order, split = '.csv')[[1]]
	    
	    this.order.list <- readLines(this.order, warn = F)
	    this.order.list <- strsplit( this.order.list[1], split = "," )[[1]]
	    this.order.no   <- c(1:6)
	    this.temp <- data.frame(Item = this.order.list, Order = this.order.no)
	    
	    this.temp$SubjectNo <- this.p
	    this.temp$Room <- this.room
	    
	    this.p.orders <- rbind(this.p.orders, this.temp)
	  }
	  
	  write.table(this.p.orders, "OutsideOrders.csv", row.names = FALSE)
	}
	

	setwd(parent.folder)
}

# Group order lists into an integrated file

participant.folders <- dir(pattern = "^P")

## Inside items

inside.orders <- NULL

for (this.p in participant.folders) {
      
      this.p.no <- as.numeric(strsplit(this.p, "P")[[1]][2])
      
      if (this.p.no < 13) {
            this.inside.order <- read.csv( paste(c(this.p, "ConditionOrders", "Inside", "InsideOrders.csv"), collapse = .Platform$file.sep), header = TRUE)
            this.inside.order$SubjectNo <- this.p
            names(this.inside.order) <- c("Room", "Item", "Order", "SubjectNo")
      } else {
            this.inside.order <- read.table( paste(c(this.p, "ConditionOrders", "Inside", "InsideOrders.csv"), collapse = .Platform$file.sep), header = TRUE)
            this.inside.order$SubjectNo <- this.p
            setcolorder( this.inside.order, c("Room", "Item", "Order", "SubjectNo"))
      }

	inside.orders <- rbind(inside.orders, this.inside.order)
}

getwd()

write.table(inside.orders, "InsideOrders.csv", row.names = FALSE)

## Outside items

outside.orders <- NULL

for (this.p in participant.folders) {

      this.p.no <- as.numeric( strsplit(this.p, "P")[[1]][2])
      
      if (this.p.no < 6) {
            this.outside.order <- read.csv( paste(c(this.p, "ConditionOrders", "Outside", "OutsideOrders.csv"), collapse = .Platform$file.sep), header = TRUE, fileEncoding = "UTF-8-BOM")
            this.outside.order$SubjectNo <- this.p
            names(this.outside.order) <- c("Room", "Item", "Order", "SubjectNo")
      } else {
            this.outside.order <- read.table(paste(c(this.p, "ConditionOrders", "Outside", "OutsideOrders.csv"), collapse = .Platform$file.sep), header = TRUE, fileEncoding = "UTF-8-BOM")
            this.outside.order$SubjectNo <- this.p
            if (this.p.no > 20) {
                  setcolorder( this.outside.order, c("Room", "Item", "Order", "SubjectNo"))
            } else {
                  names(this.outside.order) <- c("Room", "Item", "Order", "SubjectNo")
            }
      }
	outside.orders <- rbind(outside.orders, this.outside.order)
}

getwd()

write.table(outside.orders, "OutsideOrders.csv", row.names = FALSE)

# Add item order to response files
Encoding.Recall.Old <- Encoding.Recall[Context != "None"]
Encoding.Recall.Old <- Encoding.Recall.Old[order(SubjectNo, Context, Object)]

inside.orders <- read.table("InsideOrders.csv", header = T)
inside.orders <- data.table(inside.orders)
inside.orders.ordered <- inside.orders[order(SubjectNo, Room, Item)]

Encoding.Recall.Old$ItemOrder <- inside.orders.ordered$Order

Curiosity.Recall.Old <- Curiosity.Recall[Context != "None"]
Curiosity.Recall.Old <- Curiosity.Recall.Old[order(SubjectNo, Context, Object)]

outside.orders <- read.table("OutsideOrders.csv", header = T)
outside.orders <- data.table(outside.orders)
outside.orders.ordered <- outside.orders[order(SubjectNo, Room, Item)]

Curiosity.Recall.Old$ItemOrder <- outside.orders.ordered$Order