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
setwd("~/biost_561")

## load the text files from se_ex1
## skip lets us skip the lines with the system.time output
out.10 <- read.table("ex1_output_b5000_s147_n10_beta2.txt", header = TRUE, skip = 2)
out.100 <- read.table("ex1_output_b5000_s247_n100_beta2.txt", header = TRUE, skip = 2)
out.1000 <- read.table("ex1_output_b5000_s347_n1000_beta2.txt", header = TRUE, skip = 2)

## make final output matrix
out <- matrix(0, ncol = 5, nrow = 6)
colnames(out) <- c("betahat", "Model", "Sandwich", "Bootstrap", "n")
out[1, ] <- c(unlist(out.10), 10) ## need to unlist since it is a data frame, putting into a matrix
out[2, ] <- c(unlist(out.100), 100)
out[3, ] <- c(unlist(out.1000), 1000)

## load the Rdata files from se_ex2
out.inter <- matrix(0, ncol = 5, nrow = 15)
colnames(out.inter) <- c("betahat", "Model", "Sandwich", "Bootstrap", "n")


ns <- c(50, 300, 500)

for (i in 1:15) {
  ## get the correct value of n, increases by 3
  n <- ns[ifelse(i %% 3 == 0, 3, i %% 3)]
  
  ## load the data
  load(paste("ex2_output_b_1000_s_", n + 2 + i, "_n_", n, "_beta_2_t_", i, ".Rdata", sep = ""))
  
  ## calculate means - note valid to do this separately and combine later
  current <- rowMeans(output)
  
  ## add to output matrix
  out.inter[i, ] <- c(unlist(current), n) 
}

## order by n
out.inter <- out.inter[order(out.inter[, 5]), ]

## take more means, put into the final output matrix
out[4, ] <- colMeans(out.inter[1:5, ])
out[5, ] <- colMeans(out.inter[6:10, ])
out[6, ] <- colMeans(out.inter[11:15, ])

## order the output by n
out <- out[order(out[, 5]), ]
out

## make into a nice latex table
library(xtable)
print(xtable(out, digits = 3, label = "output_mat", caption = ""))

cover <- out[, 2:4]
ns <- c(10, 50, 100, 300, 500, 1000)
## also make a plot, why not
png("cover_by_n.png")
## no axes so that we can make our own labels
## color by type
plot(rep(ns[1], 3), cover[1, ], main = "Approximate coverage vs n", 
     ylab = "Approximate coverage", xlab = "n", cex = 2, pch = 16, axes = FALSE,
     xlim = c(5, 1005), ylim = c(.7, 1), col = c("black", "blue", "orange"))

## add on the rest of the points
for (i in 2:6){
  points(rep(ns[i], 3), cover[i, ], cex = 2, pch = 16, col = c("black", "blue", "orange"))
}

## add a dashed bar at .95, the nominal level
abline(h=.95, col = "red", lty = 2, cex = 2)

## add axes
axis(side = 2, at = seq(.7, 1, 0.05)) # y-axis
axis(side = 1, at = ns) ## x-axis
box()

## add a legend
legend(700, 0.9, legend = c("Model-based", "Sandwich", "Bootstrap"), 
       col = c("black", "blue", "orange"), pch = 16)
dev.off()