#################################################################################################################
##
## FILE: permutation_test_vs_bootstrap.R
##
## CREATED: 25 October 2016 by Brian Williamson
##
## PURPOSE: Example of a permutation test sampling distribution and a bootstrap 
##          sampling distribution 
##
## UPDATES:
## DDMMYY INIT COMMENTS
## ------ ---- --------
#################################################################################################################
setwd("C:/Users/brianw26/Dropbox/Courses/2016-2017 Third Year/Presentations/BIOST 561 - Lecture")
## Use the example on SNPs
## make up some data (thanks Ken!)
carrier <- rep(c(0,1), c(100,200))

## outcome under the null and alternative
null.y <- rnorm(300)

alt.y <- rnorm(300, mean = carrier/2)

## function to do one permutation test
## FUNCTION: one.test
## ARGS: x - the covariate
##       y - the outcome
## RETURNS: the results of one permutation test (i.e. difference in means, for us)
oneTest <- function(x, y) {
  ## get a new bootstrap sample of x
  xstar <- sample(x)
  
  ## return the difference in means
  ret <- mean(y[xstar == 1]) - mean(y[xstar == 0])
  
  return(ret)
}

## Function to perform a bootstrap
## Args: data - the dataset to bootstrap on
##          b - the number of bootstrap samples
## Returns: the b bootstrap datasets, as a list
myBootstrap <- function(data, b){
  boot <- list()
  if(is.vector(data)){
    for(i in 1:b){
      samp <- sample(seq_len(length(data)), length(data), replace=TRUE)
      bootb <- data[samp]
      boot[[i]] <- bootb
    }
  } else {
    for(i in 1:b){
      samp <- sample(seq_len(nrow(data)), nrow(data), replace=TRUE)
      bootb <- data[samp,]
      boot[[i]] <- bootb
    }
  }
  return(boot)
}

## set a seed first!
set.seed(4747)
system.time(output.truenull <- replicate(10000, oneTest(carrier, null.y)))
system.time(output.falsenull <- replicate(10000, oneTest(carrier, alt.y)))

null.diff <- mean(null.y[carrier == 1]) - mean(null.y[carrier == 0])
alt.diff <- mean(alt.y[carrier == 1]) - mean(alt.y[carrier == 0])

## plot histograms
png("perm_true_null.png")
hist(output.truenull, main = expression(paste("Permutation sampling distribution - ", H[0], " true",  sep = "")),
     xlab = "Difference in mean Y")
abline(v = null.diff, lwd = 2, col = "red")
dev.off()
mean(abs(output.truenull) > abs(null.diff))

png("perm_false_null.png")
hist(output.falsenull, main = expression(paste("Permutation sampling distribution - ", H[0], " false",  sep = "")),
     xlab = "Difference in mean Y")
abline(v = alt.diff, lwd = 2, col = "red")
dev.off()
mean(abs(output.falsenull) > abs(alt.diff))

## do the bootstrapping
boot.samp <- myBootstrap(data.frame(carrier, null.y), 10000)
boot.stats <- unlist(lapply(boot.samp, function(x) mean(x$null.y[x$carrier == 1]) - mean(x$null.y[x$carrier == 0])))

png("boot.png")
hist(boot.stats, main = "Bootstrap sampling distribution",
     xlab = "Difference in mean Y")
abline(v = mean(null.y[carrier == 1]) - mean(null.y[carrier == 0]), lwd = 2, col = "red")
dev.off()

