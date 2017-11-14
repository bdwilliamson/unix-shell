#################################################################################################################
##
## FILE: permutation_test_example.R
##
## CREATED: 22 October 2016 by Brian Williamson
##
## PURPOSE: Run a permutation test example for BIOST 561, Autumn 2016
##
## UPDATES:
## DDMMYY INIT COMMENTS
## ------ ---- --------
#################################################################################################################

## make up some data (thanks Ken!)
carrier <- rep(c(0,1), c(100,200))

## outcome under the null and alternative
null.y <- rnorm(300)

alt.y <- rnorm(300, mean = carrier/2)

## function to do one permutation test
## FUNCTION: do.one
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

## Function to do the permutation test, including generating data
## ARGS: n - sample size
##       B - number of MC reps
## RETURNS: the results of the permutation test (sampling distribution and p-value)
permTestSNP <- function(n, B) {
  carrier <- rep(c(0, 1), c(n/3, 2*n/3)) ## make the genotypes (0/1)
  null.y <- rnorm(n) ## make y under the null hypothesis
  alt.y <- rnorm(n, mean = carrier/2) ## make y under the alternative hypothesis
  output.truenull <- replicate(B, oneTest(carrier, null.y)) ## run the test B times
  output.falsenull <- replicate(B, oneTest(carrier, alt.y))
  ## get the observed difference in means
  null.diff <- mean(null.y[carrier == 1]) - mean(null.y[carrier == 0])
  alt.diff <- mean(alt.y[carrier == 1]) - mean(alt.y[carrier == 0])
  ## return the sampling distribution and the p-values
  return(list(samp.truenull = output.truenull, samp.falsenull = output.falsenull,
              p.truenull = mean(abs(output.truenull)) > abs(null.diff),
              p.falsenull = mean(abs(output.falsenull)) > abs(alt.diff))
  )
}

## perform this a bunch of times
## set a seed first!
set.seed(4747)
system.time(output.truenull <- replicate(100000, one.test(carrier, null.y)))
system.time(output.falsenull <- replicate(100000, one.test(carrier, alt.y)))
## Takes 4.3 seconds for the first, 4.4 seconds for the second

## get the difference in means in the original data
null.diff <- mean(null.y[carrier == 1]) - mean(null.y[carrier == 0])
alt.diff <- mean(alt.y[carrier == 1]) - mean(alt.y[carrier == 0])

## plot histograms
hist(output.truenull)
abline(v = null.diff, lwd = 2, col = "red")
mean(abs(output.truenull) > abs(null.diff))

hist(output.falsenull)
abline(v = alt.diff, lwd = 2, col = "red")
mean(abs(output.falsenull) > abs(alt.diff))
