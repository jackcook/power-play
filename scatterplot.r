library(grid)
library(png)
library(tidyverse)
library(XML)

generate_court_scatterplot <- function(data) {
  x <- c()
  y <- c()
  
  for (i in 1:(length(data[[10]]) / 8)) {
    shots <- data[[10]][,i][6]
    
    if (!is.null(shots$array)) {
      coordinates <- as.numeric(unlist(shots$array[seq(2, length(shots$array), 2)]))
      x <- c(x, coordinates[seq(1, length(coordinates), 2)])
      y <- c(y, coordinates[seq(1, length(coordinates), 2) + 1] * -1)
    }
  }
  
  dat <- data.frame(x = x, y = y)

  img <- readPNG("court.png")
  g <- rasterGrob(img, interpolate = TRUE)

  court_plot <- ggplot(dat, aes(x = x, y = y)) +
    annotation_custom(g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    theme(panel.background = element_rect(fill = "#2e9726"),
          panel.grid.major = element_line(color = "#2e9726"),
          panel.grid.minor = element_line(color = "#2e9726"),
          plot.margin = unit(c(0, 0, 0, 0), "mm"),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank()) +
    coord_fixed(ratio = 2.1739) +
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-1, 0), expand = c(0, 0)) +
    geom_point(color = "#444444", size = 1)

  ggsave("court_export.png",
         plot = court_plot,
         width = 7.01,
         height = 15.24,
         units = "cm")
}
