find_lost_points <- function(data) {
  results <- c()
  
  for (i in 1:(length(data[[10]]) / 8)) {
    result <- data[[10]][,i][2]
    
    if (!is.null(result$integer)) {
      results <- c(results, result)
    }
  }
  
  slices <- c(sum(results == 0),
              sum(results == 1),
              sum(results == 2),
              sum(results == 3))
  reasons <- c("Couldn't reach", "Hit net", "Hit out", "Left in")
  labels <- paste(reasons, " (", slices, ")")
  
  png("lost_points_export.png")
  pie(slices, labels)
  dev.off()
  
  given_shots <- c()
  received_shots <- c()
  
  for (i in 1:(length(data[[10]]) / 8)) {
    shots <- data[[10]][,i][6]
    
    if (!is.null(shots$array)) {
      raw_coordinates <- as.numeric(unlist(shots$array[seq(2, length(shots$array), 2)]))
      coordinates <- matrix(raw_coordinates, ncol = 2, byrow = TRUE)
      last_three <- tail(coordinates, 3)
      
      given_shots <- c(given_shots, analyze_shot(last_three[1,], last_three[2,]))
      received_shots <- c(received_shots, analyze_shot(last_three[2,], last_three[3,]))
    }
  }
    
  labels <- c("drop", "net", "drive", "clear")
  given_shots_slices <- c(sum(given_shots == "drop"),
                          sum(given_shots == "net"),
                          sum(given_shots == "drive"),
                          sum(given_shots == "clear"))
  
  received_shots_slices <- c(sum(received_shots == "drop"),
                             sum(received_shots == "net"),
                             sum(received_shots == "drive"),
                             sum(received_shots == "clear"))
  
  png("lost_shots_given_export.png")
  pie(given_shots_slices, labels)
  dev.off()
  
  png("lost_shots_received_export.png")
  pie(received_shots_slices, labels)
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
