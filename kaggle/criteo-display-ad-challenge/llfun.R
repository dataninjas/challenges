# Logarithmic loss function (mostly from Kaggle, added na.rm=TRUE)
llfun <- function(actual, prediction) {
  epsilon <- .000000000000001
  yhat <- pmin(pmax(prediction, rep(epsilon)), 1-rep(epsilon))
  logloss <- -mean(actual*log(yhat)
                   + (1-actual)*log(1 - yhat), na.rm=TRUE)
  return(logloss)
}