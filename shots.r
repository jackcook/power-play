analyze_points <- function(data) {
  all_shots <- list()

  for (i in 1:length(data$shots)) {
    raw_coordinates <- c()
    coordinates_strs <- strsplit(levels(data$shots)[i], " ")

    for (j in 1:length(coordinates_strs[[1]])) {
      parts <- unlist(strsplit(coordinates_strs[[1]][j], ","))
      raw_coordinates <- c(raw_coordinates, as.numeric(parts[1]), as.numeric(parts[2]))
    }

    coordinates <- matrix(raw_coordinates, ncol = 2, byrow = TRUE)
    shots <- c()

    for (j in 1:(length(coordinates) / 2 - 1)) {
      shots <- c(shots, analyze_shot(coordinates[j,], coordinates[j + 1,]))
    }

    all_shots <- c(all_shots, list(shots))
  }
}

analyze_shot <- function(shot1, shot2) {
  x1 <- shot1[1]
  y1 <- ifelse(shot1[2] > 0.5, shot1[2] - 0.5, shot1[2])
  x2 <- shot2[1]
  y2 <- ifelse(shot2[2] > 0.5, shot2[2] - 0.5, shot2[2])

  if (y2 <= 0.2) {
    if (y1 >= 0.3) {
      return("drop")
    } else {
      return("net")
    }
  } else if (y2 <= 0.35) {
    return("drive")
  } else {
    return("clear")
  }
}
