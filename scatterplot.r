library(grid)
library(png)
library(tidyverse)

require(XML)

data <- xmlParse("~/Desktop/power-play/in.game")
names <- xpathSApply(data, "//string", xmlValue)
player_one <- names[1]
player_two <- names[2]

coordinates <- as.numeric(xpathSApply(data, "//real", xmlValue)[-c(1, 2)])
x <- coordinates[seq(1, length(coordinates), 2)]
y <- coordinates[seq(1, length(coordinates), 2) + 1] * -1
dat <- data.frame(x = x, y = y)

img <- readPNG("~/Desktop/power-play/court.png")
g <- rasterGrob(img, interpolate = TRUE)

ggplot(dat, aes(x = x, y = y)) +
  annotation_custom(g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  theme(panel.background = element_rect(fill = "#2e9726"),
        panel.grid.major = element_line(color = "#2e9726"),
        panel.grid.minor = element_line(color = "#2e9726")) +
  coord_fixed(ratio = 2.3764) +
  xlim(0, 1) +
  ylim(-1, 0) +
  geom_point(shape = 1)