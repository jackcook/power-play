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
  dat[x_index,y_index] <- dat[x_index,y_index] + 1
}

dat2 <- dat %>%
  tbl_df() %>%
  rownames_to_column('X') %>%
  gather(Y, value, -X) %>%
  mutate(
    X = factor(X, levels=1:11),
    Y = factor(gsub("V", "", Y), levels=11:1)
  )

tile <- aes(fill = value)
label <- aes(label = ifelse(value > 0, as.character(value), ""))

ggplot(dat2, aes(X, Y)) +
  geom_tile(tile) +
  geom_text(label) +
  scale_fill_gradient(low = "white", high = "steelblue")