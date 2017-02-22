setwd("/Users/v-jacco/Desktop/power-play")

library(XML)
source("lost-points.r")
source("scatterplot.r")

xml_data <- xmlTreeParse("in.game")
data <- xmlToList(xml_data)
generate_court_scatterplot(data)
find_lost_points(data)