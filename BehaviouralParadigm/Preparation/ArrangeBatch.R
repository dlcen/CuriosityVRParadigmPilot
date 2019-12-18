library(data.table)

# Create a list of rooms

## Rooms for *Familiarisation*
familiarisation.rooms <- c("Cinema", "BridalShop")

## Rooms for *Encoding Task"
exploration.rooms <- c("ArtStudio", "Bedroom", "Cafe", "Classroom", "Gym", "Kindergarten", "Kitchen", "KTV", "Library", "LivingRoom", "Lounge", "Marmite", "MeetingRoom", "Museum", "Office", "SPA", "StorageRoom")

# Read the list of objects
object.list <- read.csv("AllObjectList.csv", header = TRUE)

# Make a list for those objects that need to be flipped if placed at an odd-number place along the pathway
flipped.objects <- c("AmazonEcho", "Buiscuits", "Cake", "Calculator", "Canister", "Car01", "Cereal", "Clock", "CokeBottle", "Downy", "FacialCream", "FireExtinguisher", "FirstAidKit", "Honey", "Ketchup", "Laptop", "Lotion", "Marmite", "MilkCarton", "MobilePhone", "NikonCamera", "OrangeJuice", "PeanutButter", "Perrier", "Printer", "RasberryJam", "Scent", "Suitcase", "Tide", "Vanish", "Weimeng")


source("ArrangeObjects.R")

for (p in c(1:15)) {
	ArrangeObjects(p, familiarisation.rooms, exploration.rooms, object.list, flipped.objects)
}
