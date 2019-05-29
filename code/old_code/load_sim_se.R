#########################################################################
##
## FILE: load_sim_se.R
##
## CREATED: 17 November 2016 by Brian Williamson
##
## PURPOSE: Robust SE example for BIOST 561
##
## UPDATES:
## DDMMYY INIT COMMENTS
## ------ ---- --------
#########################################################################

## set wd
setwd("~/Documents/Teaching/unix-shell/code")

## load the text files from se_ex1
## skip lets us skip the lines with the system.time output
out <- readRDS("ex3_output_n_500_beta_2_b_10000_seed_547.rds")

## check out the first column to get the names
out[, 1]

## betahats
betahats <- out[1, ]

## coverage
cover.model <- out[2, ]
cover.sand <- out[3, ]

## plot the estimates of beta
png("../2017/se_ex3_beta_ests.png")
hist(betahats, xlab = expression(hat(beta)[1]), 
     main = expression(paste("Histogram of ", hat(beta)[1], " values", sep = "")),
     breaks = 30)
abline(v = 2, col = "red", lwd = 2)
abline(v = mean(betahats), col = "blue", lwd = 2)
legend("topright", legend = c("Truth", "Avg."), col = c("red", "blue"), lty = c(1, 1))
dev.off()

## get the estimated coverage
mean(cover.model)
mean(cover.sand)
table.out <- cbind(mean(cover.model), mean(cover.sand))
colnames(table.out) <- c("Model-based", "Sandwich")
rownames(table.out) <- "Coverage"

## print out in a nice TeX-able table (NB: also works in markdown!!)
library(knitr)
kable(table.out, col.names = colnames(table.out), row.names = TRUE, caption = "Coverage of nominal 95% CIs", format = "latex")
