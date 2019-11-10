library(data.table)

# Inside item orders

participant.folders <- dir(path = "IndividualRawData/", pattern = "^P")

for (this.p in participant.folders) {

	this.inside.folder <- paste(c("IndividualRawData", this.p, "ConditionOrders", "Inside"), collapse = .Platform$file.sep)

	this.outside.folder <- paste(c("IndividualRawData", this.p, "ConditionOrders", "Outside"), collapse = .Platform$file.sep)
	
	if (! (file.exists(paste0(this.inside.folder, .Platform$file.sep, "InsideOrders.csv")))) {

        	inside.item.orders <- dir(path = this.inside.folder, pattern = "\\.csv$")
        
        	this.p.orders <- NULL
        
        	for (this.order in inside.item.orders) {
        		this.room <- strsplit(this.order, split = '.csv')[[1]]
        
        		this.order.list <- readLines( paste0( this.inside.folder, .Platform$file.sep, this.order), warn = F)
        		this.order.list <- strsplit( this.order.list[1], split = "," )[[1]]
        		this.order.no   <- c(1:length(this.order.list))
        		this.temp <- data.frame(Item = this.order.list, Order = this.order.no)
        
        		this.temp$SubjectNo <- this.p
        		this.temp$Room <- this.room
        
        		this.p.orders <- rbind(this.p.orders, this.temp)
        	}
  
  	      write.table(this.p.orders, paste0(this.inside.folder, .Platform$file.sep, "InsideOrders.csv"), row.names = FALSE)
	}

	if (! (file.exists(paste0( this.outside.folder, .Platform$file.sep, "OutsideOrders.csv")))) {

	  outside.item.orders <- list.files(path = this.outside.folder, pattern = "\\.csv$", ignore.case = TRUE)
	  
	  this.p.orders <- NULL
	  
	  for (this.order in outside.item.orders) {
	    this.room <- strsplit(this.order, split = '.csv')[[1]]
	    
	    this.order.list <- readLines(paste0( this.outside.folder, .Platform$file.sep, this.order), warn = F)
	    this.order.list <- strsplit( this.order.list[1], split = "," )[[1]]
	    this.order.no   <- c(1:length(this.order.list))
	    this.temp <- data.frame(Item = this.order.list, Order = this.order.no)
	    
	    this.temp$SubjectNo <- this.p
	    this.temp$Room <- this.room
	    
	    this.p.orders <- rbind(this.p.orders, this.temp)
	  }
	  
	  write.table(this.p.orders, paste0(this.outside.folder, .Platform$file.sep, "OutsideOrders.csv"), row.names = FALSE)
	}
}

# Group order lists into an integrated file

participant.folders <- dir(path = "IndividualRawData/", pattern = "^P")

## Inside items

inside.orders <- NULL

for (this.p in participant.folders) {

	this.inside.order <- read.table( paste(c("IndividualRawData", this.p, "ConditionOrders", "Inside", "InsideOrders.csv"), collapse = .Platform$file.sep), header = TRUE)

	inside.orders <- rbind(inside.orders, this.inside.order)
}

getwd()

write.table(inside.orders, "InsideOrders.csv", row.names = FALSE)

## Outside items

outside.orders <- NULL

for (this.p in participant.folders) {

	this.outside.order <- read.table( paste(c("IndividualRawData", this.p, "ConditionOrders", "Outside", "OutsideOrders.csv"), collapse = .Platform$file.sep), header = TRUE)

	outside.orders <- rbind(outside.orders, this.outside.order)
}

getwd()

write.table(outside.orders, "OutsideOrders.csv", row.names = FALSE)

# Add item order to response files
# Encoding.Recall.Old <- Encoding.Recall[Context != "None"]
# Encoding.Recall.Old <- Encoding.Recall.Old[order(SubjectNo, Context, Object)]
# 
# inside.orders <- read.table("InsideOrders.csv", header = T)
# inside.orders <- data.table(inside.orders)
# inside.orders.ordered <- inside.orders[order(SubjectNo, Room, Item)]
# 
# Encoding.Recall.Old$ItemOrder <- inside.orders.ordered$Order
# 
# Curiosity.Recall.Old <- Curiosity.Recall[Context != "None"]
# Curiosity.Recall.Old <- Curiosity.Recall.Old[order(SubjectNo, Context, Object)]
# 
# outside.orders <- read.table("OutsideOrders.csv", header = T)
# outside.orders <- data.table(outside.orders)
# outside.orders.ordered <- outside.orders[order(SubjectNo, Room, Item)]
# 
# Curiosity.Recall.Old$ItemOrder <- outside.orders.ordered$Order