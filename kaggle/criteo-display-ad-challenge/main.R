# source our custom functions
source('dataPreparation.R')
source('dummy.R')
source('llfun.R')

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

# constants
numSamplesPerDay <- 6548660 # computed by numRows in train.csv / 7
if (smallDataset) {
  trainFile <- 'train_sample_100000.csv'
  testFile <- 'train_sample_20000.csv'
  submitFile <- NULL
} else {
  trainFile <- 'train_sample_6548660.csv'
  testFile <- NULL
  submitFile <- 'test.csv'
}

# 4 cores for parallel execution
registerDoParallel(4)

# Generate feature matrix from data table
GetFeatureMatrix <- function(dt)
{
  int_features <- grep('^I[0-9]+', colnames(dt), value=TRUE)
  cat_features <- grep('^C[0-9]+', colnames(dt), value=TRUE)
  missingCol_features <- grep('^Missing', colnames(dt), value = TRUE)
  
  # Use dummy variables for categorical features, for states that exist in at least 1% data
  dummy <- GetDummyVariables(dt[, cat_features, with = FALSE])
  dummy_cols <- (colMeans(dummy) > 0.01)
  
  # return feature matrix
  cbind(poly(dt$normId, degree = 5, raw = TRUE),
        as.matrix(dt[, int_features, with = FALSE]),
        as.matrix(dt[, missingCol_features, with = FALSE]),
        as.matrix(dummy[, dummy_cols]))
}

# Load data
if (!exists('data.dt'))
{
  if (runTrain)
  {
    if (!file.exists(trainFile))
    {
      stop(paste('Cannot find input file', trainFile))
    }
    data.dt <- fread(trainFile, sep = ',', header = TRUE)
    data.dt[, type:="train"]
    numTrainSamples <- dim(data.dt)[1]
    
    if (!file.exists(testFile))
    {
      stop(paste('Cannot find input file', testFile))
    }
    test.dt <- fread(testFile, sep = ',', header = TRUE)
    test.dt[, type:="test"]
    numTestSamples <- dim(test.dt)[1]
    
    data.dt <- rbind2(data.dt, test.dt)
    rm(test.dt)
  }    
    
  if (runSubmit)
  {
    if (!file.exists(submitFile))
    {
      stop(paste('Cannot find input file', submitFile))
    }
    submit.dt <- fread(submitFile, sep = ',', header = TRUE)
    submit.dt[, type:="submit"]
      
    # Add Label column (set to NA) to test data to match the dimension of training data
    submit.dt[, Label:=NA]
    
    data.dt <- rbind2(data.dt, submit.dt)
    rm(submit.dt)
  }
  
  CleanseRawDatatable(data.dt)

  Features <- GetFeatureMatrix(data.dt)
}

# Logistic regression with Glmnet
if (runTrain)
{
  # k-fold cross validation using glmnet
  train.fit <- cv.glmnet(x = Features[1:numTrainSamples,],
                         y = as.matrix(data.dt[1:numTrainSamples, "Label", with = FALSE]),
                         nfolds = 10,
                         family = "binomial",
                         parallel = TRUE)
  
  # Make predictions based on fitted model
  data.dt[, Predict:= predict(train.fit,
                              Features,
                              type = "response",
                              s = "lambda.min")]
  
  # Compute LogLoss score on test set
  test.score <- llfun(as.numeric(data.dt$Label)[(numTrainSamples+1):(numTrainSamples+numTestSamples)]-1, 
                      data.dt$Predict[(numTrainSamples+1):(numTrainSamples+numTestSamples)])
}
 
# Write predictions to submission.csv
if (runSubmit) 
{
  data.dt[, Predicted:=round(Predicted, 12)]
  
#   write.csv(format(data.dt[,c('Id','Predicted'), with=FALSE],
#                    scientific=FALSE), 
#             'submission.csv',
#             quote=FALSE,
#             row.names = FALSE)
}

# Compare PDF of feature I10 for Label 0 vs 1
# sm.density.compare(train.dt$I10, train.dt$Label)

# Scatter plot for features I1:I5 for Label 0 vs 1
# splom(train.dt[, c('I7','I9'),with=FALSE], groups = train.dt$Label)

# Scatter plot with binning for feature I3 vs I4
# hexbinplot(I1 ~ I10 | Label, train.dt, aspect = 1)
