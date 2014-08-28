require(data.table)   # fast aggregation of large data
#require(colbycol)     # efficient parsing of csv and random data sampling
require(sm)           # compare density function for classification
require(lattice)      # scatter plots
require(hexbin)       # high density plots with binning
require(FSelector)    # feature selection
require(randomForest) # random forest model and feature importance
require(LiblineaR)    # regularized logistic regression

# input parameters
runRF = FALSE
runLR = TRUE

# Logarithmic loss function (mostly from Kaggle, added na.rm=TRUE)
llfun <- function(actual, prediction) {
  epsilon <- .000000000000001
  yhat <- pmin(pmax(prediction, rep(epsilon)), 1-rep(epsilon))
  logloss <- -mean(actual*log(yhat)
                   + (1-actual)*log(1 - yhat), na.rm=TRUE)
  return(logloss)
}

# Load data and process features
if (!exists('data.dt'))
{
  ###### TODO ######
#   Need a different way to randomly sample from training data
#   Cannot use cbc, because it drops rows with missing value!
#
#   set.seed(1234)
#   samplingRate = 0.001
#   train.cbc <- cbc.read.table('train.csv',
#                           sample.pct = samplingRate,
#                           sep = ',',
#                           fill = TRUE,
#                           header = TRUE)
#    
#   train.dt <- as.data.table(as.data.frame(train.cbc))

  # Read training data from CSV
  if (!file.exists('train.csv'))
  {
    stop('Cannot find input file train.csv.')
  }
  train.dt <- fread('train.csv',
                 sep = ',',
                 select = 1:15, # drop categorical features to save memory
                 nrows = 100000,
                 header = TRUE)

  # Read test data from CSV
  if (!file.exists('test.csv'))
  {
    stop('Cannot find input file test.csv.')
  }
  test.dt <- fread('test.csv',
                   sep = ',',
                   select = 1:14, # drop categorical features to save memory
                   header = TRUE)
  
  # Add Label column to test data, set to NA
  test.dt[, Label:=NA]

  # Combine into data.dt and remove other data tables
  data.dt <- rbind(train.dt, test.dt, use.names=TRUE)
  rm(train.dt)
  rm(test.dt)

  int_features <- grep('I[0-9]+', colnames(data.dt), value=TRUE)

  # Convert integer feature columns to class integer
  data.dt[, (int_features):=lapply(.SD,as.integer), .SDcols=int_features]

  # Convert label column to factor
  data.dt[, Label:=as.factor(Label)]

  # Add indictor variables for missing features
  missingCols <- sapply(int_features, function(x) {paste('Missing',x,sep='_')})
  data.dt[, (missingCols):=lapply(.SD,is.na), .SDcols=int_features]
  data.dt[, (missingCols):=lapply(.SD,as.integer), .SDcols=missingCols]

  # Replace NAs with median value of column
  impute.median <- function(x) replace(x, is.na(x), round(median(x, na.rm = TRUE)))
  data.dt[, (int_features):=lapply(.SD, impute.median), .SDcols=int_features]

  # Feature normalization
  data.dt[, (int_features):=lapply(.SD, scale), .SDcols=int_features]
}

# Feature selection
allFeatures <- c(int_features, missingCols)
data.dt[, Label:=as.numeric(Label)-1]
data.corr <- linear.correlation(formula = as.simple.formula(allFeatures, 'Label'),
                                data = data.dt[!is.na(Label)])
topFeatures <- cutoff.k(data.corr, 20)
data.dt[, Label:=as.factor(Label)]

# Compare PDF of feature I2 for Label 0 vs 1d
# sm.density.compare(train.dt$I2, train.dt$Label)

# Scatter plot for features I1:I5 for Label 0 vs 1
# splom(train.dt[, c('I1','I10'),with=FALSE], groups = train.dt$Label)

# Scatter plot with binning for feature I3 vs I4
# hexbinplot(I1 ~ I10 | Label, train.dt)

# Random forest
if (runRF)
{
  train.dt <- data.dt[!is.na(Label),]
  set.seed(1234)
  data.rf <- randomForest(as.simple.formula(allFeatures, 'Label'),
                           data = train.dt,
                           ntree = 100,
                           mtry = 5,
                           classwt = c(0.75,0.25),
                           do.trace = 10,
                           importance = TRUE)

  # Score of trained random forest on training data
  data.rf.score <- llfun(as.numeric(train.dt$Label)-1, data.rf$votes)
}

# Logistic regression
if (runLR)
{
  # train logistic regression model with LiblineaR
  train.dt <- data.dt[!is.na(Label),]
  data.lr <- LiblineaR(data = train.dt[, topFeatures, with=FALSE],
                      labels = train.dt$Label,
                      type = 7, # L1 regularized
                      bias = TRUE) # y-intercept

  temp <- predict(data.lr, data.dt[, topFeatures, with=FALSE], proba=TRUE)
  data.dt[, Predicted:=temp$probabilities[,2]]
  
  # Score of trained logistic regression on training data
  data.lr.score <- llfun(as.numeric(data.dt$Label)-1, data.dt$Predicted)
  
  # Plot prediction vs label of training data
  sm.density.compare(data.dt$Predicted, data.dt$Label)
}

# Write predictions to submission.csv
write.csv(format(data.dt[is.na(Label),c('Id','Predicted'), with=FALSE], scientific=FALSE), 
          'submission.csv',
          row.names = FALSE)

