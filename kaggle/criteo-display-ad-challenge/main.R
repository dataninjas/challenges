# source our custom functions
source('dataPreparation.R')
source('dummy.R')

# load required libraries
require(data.table)   # fast aggregation of large data
require(sm)           # compare density function for classification
require(glmnet)       # regularized logistic regression
require(Matrix)       # sparse matrix
require(doParallel)

# input parameters
smallDataset <- TRUE
runTrain <- TRUE
runSubmit <- FALSE
numSamplesPerDay <- 6548660 # computed by numRows in train.csv / 7
maxUniqueValuesInCatFeatures <- 10 # only use categorical features with <= 10 unique values (memory constraint)

if (smallDataset) {
  trainFile <- 'train_sample_100000.csv'
  testFile <- 'train_sample_20000.csv'
} else {
  trainFile <- 'train_sample_6548660.csv'
  testFile <- NULL
}

# 4 cores for parallel execution
registerDoParallel(4)

# Logarithmic loss function (mostly from Kaggle, added na.rm=TRUE)
llfun <- function(actual, prediction) {
  epsilon <- .000000000000001
  yhat <- pmin(pmax(prediction, rep(epsilon)), 1-rep(epsilon))
  logloss <- -mean(actual*log(yhat)
                   + (1-actual)*log(1 - yhat), na.rm=TRUE)
  return(logloss)
}

# Generate feature matrix from data table
GetFeatureMatrix <- function(dt)
{
  int_features <- grep('^I[0-9]+', colnames(dt), value=TRUE)
  cat_features <- grep('^C[0-9]+', colnames(dt), value=TRUE)
  missingCol_features <- grep('^Missing', colnames(dt), value = TRUE)
  
  # Use dummy variables for categorical features, for states that exist in at least 1% data
  dummy <- GetDummyVariables(dt[, cat_features, with = FALSE])
  dummy_cols <- (colMeans(dummy) > 0.1)
  
  # return feature matrix
  cbind(poly(dt$normId, degree = 5, raw = TRUE),
        as.matrix(dt[, int_features, with = FALSE]),
        as.matrix(dt[, missingCol_features, with = FALSE]),
        as.matrix(dummy[, dummy_cols]))
}

# Logistic regression with Glmnet
if (runTrain)
{
  # Load training data
  if (!exists('train.dt'))
  {
    if (!file.exists(trainFile) )
    {
      stop(paste('Cannot find input file', trainFile))
    }
    
    train.dt <- fread(trainFile, sep = ',', header = TRUE)
    CleanseRawDatatable(train.dt)
  }
  
  # k-fold cross validation using glmnet
  train.fit <- cv.glmnet(x = GetFeatureMatrix(train.dt),
                         y = as.matrix(train.dt$Label),
                         nfolds = 10,
                         family = "binomial",
                         parallel = TRUE)
  
  # Load test data
  if (!exists('test.dt'))
  {
    if (!file.exists(testFile) )
    {
      stop(paste('Cannot find input file', testFile))
    }
    
    test.dt <- fread(testFile, sep = ',', header = TRUE)
    CleanseRawDatatable(test.dt)
  }  
  
  # Compute LogLoss score on test set
  test.dt[, Predicted:=predict(train.fit,
                                 GetFeatureMatrix(test.dt),
                                 type = "response",
                                 s = "lambda.min")]
  
  test.score <- llfun(as.numeric(test.dt$Label)-1, test.dt$Predicted)
  
  # Plot prediction vs label of test data
  sm.density.compare(test.dt$Predicted, test.dt$Label)
}

# Write predictions to submission.csv
if (runSubmit) 
{
  # Load test data
  if (!exists('submit.dt'))
  {
    if (!file.exists('test.csv'))
    {
      stop('Cannot find input file test.csv.')
    }
    submit.dt <- fread('test.csv',
                     sep = ',',
                     header = TRUE)
    
    # Add Label column (set to NA) to test data to match the dimension of training data
    submit.dt[, Label:=NA]
    
    CleanseRawDatatable(test.dt)
  }
  
  submit.dt[, Predicted:=predict(train.fit,
                               GetFeatureMatrix(submit.dt),
                               type = "response",
                               s = "lambda.min")]
  
  submit.dt[, Predicted:=round(Predicted, 12)]
  
  write.csv(format(submit.dt[,c('Id','Predicted'), with=FALSE],
                   scientific=FALSE), 
            'submission.csv',
            quote=FALSE,
            row.names = FALSE)
}

# Compare PDF of feature I10 for Label 0 vs 1
# sm.density.compare(train.dt$I10, train.dt$Label)

# Scatter plot for features I1:I5 for Label 0 vs 1
# splom(train.dt[, c('I7','I9'),with=FALSE], groups = train.dt$Label)

# Scatter plot with binning for feature I3 vs I4
# hexbinplot(I1 ~ I10 | Label, train.dt, aspect = 1)
