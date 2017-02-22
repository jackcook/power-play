find_lost_points <- function(data) {
  results <- data$results
  
  slices <- c(sum(results == levels(results)[1]),
              sum(results == levels(results)[2]),
              sum(results == levels(results)[3]),
              sum(results == levels(results)[4]))
  reasons <- levels(results)
  labels <- paste(reasons, " (", slices, ")")
  
  png("lost_points_export.png")
  pie(slices, labels)
  dev.off()
  
  ###
  
  given_shots <- c()
  received_shots <- c()
  
  for (i in 1:nrow(data)) {
    raw_coordinates <- c()
    coordinates_strs <- strsplit(levels(data$shots)[i], " ")
    
    for (j in 1:length(coordinates_strs[[1]])) {
      parts <- unlist(strsplit(coordinates_strs[[1]][j], ","))
      raw_coordinates <- c(raw_coordinates, as.numeric(parts[1]), as.numeric(parts[2]))
    }
    
    coordinates <- matrix(raw_coordinates, ncol = 2, byrow = TRUE)
    last_three <- tail(coordinates, 3)
    
    given_shots <- c(given_shots, analyze_shot(last_three[1,], last_three[2,]))
    received_shots <- c(received_shots, analyze_shot(last_three[2,], last_three[3,]))
  }
  
  labels <- c("drop", "net", "drive", "clear")
  
  given_shots_slices <- c(sum(given_shots == "drop"),
                          sum(given_shots == "net"),
                          sum(given_shots == "drive"),
                          sum(given_shots == "clear"))
  given_shots_labels <- paste(labels, " (", given_shots_slices, ")", sep = "")
  
  received_shots_slices <- c(sum(received_shots == "drop"),
                             sum(received_shots == "net"),
                             sum(received_shots == "drive"),
                             sum(received_shots == "clear"))
  received_shots_labels <- paste(labels, " (", received_shots_slices, ")", sep = "")
  
  png("lost_shots_given_export.png")
  pie(given_shots_slices, given_shots_labels)
  dev.off()
  
  png("lost_shots_received_export.png")
  pie(received_shots_slices, received_shots_labels)
  dev.off()
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
