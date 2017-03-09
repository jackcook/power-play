find_lost_points <- function(data) {
  results <- c()
  
  for (i in 1:nrow(data)) {
    if (data$winners[i] == "P2") {
      results <- c(results, data$results[i])
    }
  }

  slices <- c(sum(results == 1),
              sum(results == 2),
              sum(results == 3),
              sum(results == 4))
  reasons <- levels(data$results)
  labels <- paste(reasons, " (", slices, ")")

  png("lost_points_export.png")
  pie(slices, labels)
  dev.off()

  ###

  given_shots <- c()
  received_shots <- c()

  for (i in 1:nrow(data)) {
    if (data$winners[i] == "P1") {
      raw_coordinates <- c()
      coordinates_strs <- strsplit(as.character(data$shots[i]), " ")

      for (j in 1:length(coordinates_strs[[1]])) {
        parts <- unlist(strsplit(coordinates_strs[[1]][j], ","))
        raw_coordinates <- c(raw_coordinates, as.numeric(parts[1]), as.numeric(parts[2]))
      }

      coordinates <- matrix(raw_coordinates, ncol = 2, byrow = TRUE)

      if (nrow(coordinates) >= 3) {
        last_three <- tail(coordinates, 3)
        
        shot1 <- last_three[1,]
        shot2 <- last_three[2,]
        
        x1 <- shot1[1]
        y1 <- ifelse(shot1[2] > 0.5, shot1[2] - 0.5, -1 * (shot1[2] - 0.5))
        x2 <- shot2[1]
        y2 <- ifelse(shot2[2] > 0.5, shot2[2] - 0.5, -1 * (shot2[2] - 0.5))
        
        analysis <- ""
        
        if (y2 <= 0.15) {
          if (y1 >= 0.15) {
            analysis <- "drop"
          } else {
            analysis <- "net"
          }
        } else if (y2 <= 0.35) {
          analysis <- "drive"
        } else {
          analysis <- "clear"
        }

        given_shots <- c(given_shots, analysis)
        
        shot1 <- last_three[2,]
        shot2 <- last_three[3,]
        
        x1 <- shot1[1]
        y1 <- ifelse(shot1[2] > 0.5, shot1[2] - 0.5, -1 * (shot1[2] - 0.5))
        x2 <- shot2[1]
        y2 <- ifelse(shot2[2] > 0.5, shot2[2] - 0.5, -1 * (shot2[2] - 0.5))
        
        analysis <- ""
        
        if (y2 <= 0.15) {
          if (y1 >= 0.15) {
            analysis <- "drop"
          } else {
            analysis <- "net"
          }
        } else if (y2 <= 0.35) {
          analysis <- "drive"
        } else {
          analysis <- "clear"
        }
        
        received_shots <- c(received_shots, analysis)
      }
    }
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
