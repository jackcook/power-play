library(readr)
dataset <- read_csv("~/Desktop/badminton/data.csv")

last_shot <- c()
second_last_shot <- c()
combo_shots <- c()

for (i in seq(1, 20)) {
  if (dataset[i,]$winner == "O") {
    last <- tail(strsplit(dataset[i,]$shots, " ")[[1]], n=1)
    
    if (is.null(last_shot[last]) || is.na(last_shot[last])) {
      last_shot[last] <- 0
    }
    
    last_shot[last] <- last_shot[last] + 1
    
    
    second_last <- tail(strsplit(dataset[i,]$shots, " ")[[1]], n=2)[1]
    
    if (is.null(second_last_shot[second_last]) || is.na(second_last_shot[second_last])) {
      second_last_shot[second_last] <- 0
    }
    
    second_last_shot[second_last] <- second_last_shot[second_last] + 1
    
    
    combo <- paste(second_last, last, sep = " ")
    
    if (is.null(combo_shots[combo]) || is.na(combo_shots[combo])) {
      combo_shots[combo] <- 0
    }
    
    combo_shots[combo] <- combo_shots[combo] + 1
  }
}

print(last_shot)
print(second_last_shot)
print(combo_shots)