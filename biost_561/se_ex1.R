#!/usr/local/bin/Rscript


#########################################################################
##
## FILE: se_ex1.R
##
## CREATED: 27 October 2016 by Brian Williamson
##
## PURPOSE: Robust SE example for BIOST 561
##
## UPDATES:
## DDMMYY INIT COMMENTS
## ------ ---- --------
## 171116 BDW  Added bootstrap functionality
#########################################################################
args <- commandArgs(TRUE)
## Parse the arguments
if(length(args) == 0) {
  print("No arguments supplied.")
} else {
  for (i in 1:length(args)) {
    eval(parse(text = args[i]))
  }
}
if(!exists("truebeta")) {
  truebeta <- 2
  cat("\n Note: Assmuming truebeta = 2 \n")
}
if(!exists("n")) {
  n <- 500
  cat("\n Note: Setting n = 500 \n")
}
if(!exists("seed")) {
  seed <- 547
  cat("\n Note: Setting seed = 547 \n")
}
if(!exists("B")) {
  B <- 1000
  cat("\n Note: setting B = 1000 \n")
}

## Function to perform a bootstrap
## Args: data - the dataset to bootstrap on
##          b - the number of bootstrap samples
## Returns: the b bootstrap datasets, as a list
myBootstrap <- function(data, b){
  boot <- list()
  if (is.vector(data)) {
    for (i in 1:b) {
      samp <- sample(seq_len(length(data)), length(data), replace = TRUE)
      bootb <- data[samp]
      boot[[i]] <- bootb
    }
  } else {
    for (i in 1:b) {
      samp <- sample(seq_len(nrow(data)), nrow(data), replace = TRUE)
      bootb <- data[samp,]
      boot[[i]] <- bootb
    }
  }
  return(boot)
}

## Function to run the simulation once
doOne <- function(n, beta) {
  ## create the data
  x <- rnorm(n, 0, 1)
  u <- rnorm(n, 0, 1)
  eps <- abs(x)*u
  beta0 <- 1
  y <- beta0 + beta*x + eps
  
  ## create the bootstrap datasets
  boot <- myBootstrap(data.frame(x, y), b = 1000)
  ## get the estimates
  betahats <- unlist(lapply(boot, function(x) coefficients(lm(y ~ ., data = x))[2]))
  
  ## fit the linear regression model
  mod <- lm(y ~ x)
  
  ## extract model coefficients and SEs
  est <- coefficients(mod)[2]
  se <- vector("numeric", 3)
  se[1] <- sqrt(diag(vcov(mod)))[2]
  se[2] <- sqrt(diag(vcovHC(mod, "HC0")))[2]
  se[3] <- sd(betahats)
  
  ## Create CIs
  ci <- est + se %o% qnorm(c(0.025, 0.975))
  cover <- beta > ci[, 1] & beta < ci[, 2]
  names(cover) <- c("Model", "Sandwich", "Bootstrap")
  
  ## return
  return(c(est, cover))
}

library(sandwich)
set.seed(seed)
system.time(output <- replicate(B, doOne(n = n, beta = truebeta)))
apply(output, 1, mean)
