library(data.table); library(nlme); library(ggplot2)

# Get the list of participants

parent.folder <- getwd()
participant.folder <- dir(pattern = "^P")

# Inside each participant folder - get the response and object order

ordered.response <- NULL

for (p in participant.folder) {
  
  this.rsp <- data.table(read.csv(paste0(p, .Platform$file.sep, "Encoding_Memory_Test_Response.csv"), header = T))
  this.ord <- data.table(read.csv(paste0(p, .Platform$file.sep, "InsideOrders.csv"), header = T))
  
  # Get the responses for the seen objects
  this.seen.rsp <- this.rsp[Context != "None"]
  
  this.id <- rep(p, nrow(this.seen.rsp))
  
  this.seen.rsp$ItemResp <- 1
  this.seen.rsp[ObjectResponse == "New"]$ItemResp <- 0
  
  this.seen.rsp$Context <- factor(this.seen.rsp$Context, levels = levels(this.seen.rsp$ContextResponse))
  
  this.seen.rsp[, ContextResp := (Context == ContextResponse)]
  this.seen.rsp$ContextResp <- factor(this.seen.rsp$ContextResp, levels = c(TRUE, FALSE), labels = c(1, 0))
  
  this.seen.rsp.ordered <- this.seen.rsp[ order(Context, Object), ]
  this.ord.ordered <- this.ord[ order(Room, Object), ]
  
  this.seen.rsp.ordered$Order <- this.ord.ordered$Order
  this.seen.rsp.ordered$SubjectNo <- this.id
  
  ordered.response <- rbind(ordered.response, this.seen.rsp.ordered)
}

# Starting modelling

# Plot the data for each individual participant
ggplot(ordered.response, aes(x = Order, y = ItemResp)) +
  geom_point() +
  scale_x_continuous(breaks = c(1:6), limits = c(0.9, 6.1)) +
  scale_y_continuous(breaks = c(0, 1), limits = c(-0.1, 1.1)) +
  labs(x = "Item order", y = "Response (0 - new, 1 - old or familiar)") +
  facet_grid( SubjectNo ~ Context )

# Context memory
ggplot(ordered.response, aes(x = Order, y = ContextResp)) +
  geom_point() +
  scale_x_continuous(breaks = c(1:6), limits = c(0.9, 6.1)) +
  labs(x = "Item order", y = "Response (0 - wrong, 1 - correct)") +
  facet_grid( SubjectNo ~ Context )

# Get the curiosity ratings for each participant
