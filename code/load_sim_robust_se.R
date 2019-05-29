cover <- beta > ci[, 1] & beta < ci[, 2]
names(cover) <- c("Model", "Sandwich")