#########################################################################
##
## FILE: my_bootstrap.R
##
## CREATED: 14 November 2017 by Brian Williamson
##
## PURPOSE: Robust SE example for BIOST 561
##
## UPDATES:
## DDMMYY INIT COMMENTS
## ------ ---- --------
#########################################################################

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