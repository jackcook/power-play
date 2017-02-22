setwd("/Users/v-jacco/Desktop/power-play")

library(XML)
source("lost-points.r")
source("scatterplot.r")

xml_data <- xmlTreeParse("in.game")
raw_data <- xmlToList(xml_data)
data <- raw_data[seq(2, length(raw_data) - 1, 2)] # remove plist keys
names(data) <- raw_data[seq(1, length(raw_data) - 1, 2)] # copy plist keys to names

points <- data$points
results <- unlist(points[seq(2, length(points) - 8, 8)])
servers <- unlist(points[seq(4, length(points) - 8, 8)])

raw_shots <- points[seq(6, length(points) - 8, 8)]
shots <- c()

for (i in 1:length(raw_shots)) {
  coordinates <- as.numeric(raw_shots[[i]][seq(2, length(raw_shots[[i]]), 2)])
  coordinates_str <- ""
  
  for (j in seq(1, length(coordinates), 2)) {
    coordinates_str <- paste(coordinates_str, paste(coordinates[j], ",", coordinates[j+1], sep = ""))
  }
  
  shots <- c(shots, substring(coordinates_str, 2))
}

winners <- unlist(points[seq(8, length(points) - 8, 8)])
points_data <- data.frame(results, servers, shots, winners)

levels(points_data$results) <- c("Couldn't reach", "Hit net", "Hit out", "Left in")
levels(points_data$servers) <- c("P1", "P2")
levels(points_data$winners) <- c("P1", "P2")

generate_court_scatterplot(points_data)
find_lost_points(points_data)