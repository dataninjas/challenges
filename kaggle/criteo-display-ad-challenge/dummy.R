# Generate matrix of dummy variables from data table
# Based on: www.flaviobarros.net/2014/02/25/genetic-data-large-matrices-glmnet/
GetDummyVariables <- function(dt)
{
  df <- as.data.frame(dt)
  X <- sparse.model.matrix(~df[,1]-1)
  
  for (i in 2:ncol(df)) 
  {
    if (nlevels(df[,i])>1) 
    {
      column <- sparse.model.matrix(~df[,i]-1)
      X <- cBind(X, column)
    }
    else 
    {
      column <- as.numeric(as.factor(df[,i]))
      X <- cBind(X, column)
    }
  }
  return(X)
}