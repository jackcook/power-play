setwd("/Users/v-jacco/Desktop/power-play")

require(XML)
source("lost-points.r")
source("scatterplot.r")

data <- xmlParse("~/Desktop/power-play/in.game")
generate_court_scatterplot(data)
find_lost_points(data)