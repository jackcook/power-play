find_lost_points <- function(data) {
  integers <- as.numeric(xpathSApply(data, "//integer", xmlValue))
  results <- integers[seq(1, length(integers), 3)]
  slices <- c(sum(results == 0),
              sum(results == 1),
              sum(results == 2),
              sum(results == 3))
  reasons <- c("Couldn't reach", "Hit net", "Hit out", "Left in")
  labels <- paste(reasons, " (", slices, ")")
  
  png("~/Desktop/power-play/lost_points_export.png")
  pie(slices, labels)
  dev.off()
}