require(XML)

data <- xmlParse("~/Desktop/power-play/in.game")
names <- xpathSApply(data, "//string", xmlValue)
player_one <- names[1]
player_two <- names[2]

coordinates <- xpathSApply(data, "//real", xmlValue)[-c(1, 2)]
x <- coordinates[seq(1, length(coordinates), 2)]
y <- coordinates[seq(1, length(coordinates), 2) + 1]

dat <- matrix(rep(0, 121), ncol=11)

for (i in seq(length(x))) {
  x_index <- round(as.numeric(x[i]) / 0.1) + 1
  y_index <- round(as.numeric(y[i]) / 0.1) + 1
  print(y_index)
  dat[x_index,y_index] <- dat[x_index,y_index] + 1
}

dat2 <- dat %>%
  tbl_df() %>%
  rownames_to_column('Var1') %>%
  gather(Var2, value, -Var1) %>%
  mutate(
    Var1 = factor(Var1, levels=1:11),
    Var2 = factor(gsub("V", "", Var2), levels=11:1)
  )

ggplot(dat2, aes(Var1, Var2)) +
  geom_tile(aes(fill = value)) + 
  geom_text(aes(label = round(value, 1))) +
  scale_fill_gradient(low = "white", high = "red")