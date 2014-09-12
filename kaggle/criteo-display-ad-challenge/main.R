require(data.table)   # fast aggregation of large data
require(sm)           # compare density function for classification
require(glmnet)       # regularized logistic regression
require(Matrix)       # sparse matrix
require(doParallel)

# input parameters
runGlmnet <- TRUE
writeCSV <- FALSE
numSamplesPerDay <- 6548660 # computed by numRows in train.csv / 7
numTrainingSamples <- numSamplesPerDay
maxUniqueValuesInCatFeatures <- 10 # only use categorical features with <= 10 unique values (memory constraint)

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

CleanseRawDatatable <- function(dt)
{
  int_features <- grep('^I[0-9]+', colnames(dt), value=TRUE)
  cat_features <- grep('^C[0-9]+', colnames(dt), value=TRUE)
  
  # Convert integer feature columns to class integer
  dt[, (int_features):=lapply(.SD,as.integer), .SDcols = int_features]
  
  # Convert categorical feature columns to class factor
  dt[, (cat_features):=lapply(.SD,as.factor), .SDcols=cat_features]
  
  # Convert label column to factor
  dt[, Label:=as.factor(Label)]
  
  # Add indictor variables for missing integer features
  missingCols <- sapply(int_features, function(x) { paste('Missing', x, sep='_') })
  dt[, (missingCols):=lapply(.SD, is.na), .SDcols = int_features]
  dt[, (missingCols):=lapply(.SD, as.integer), .SDcols = missingCols]
  
  # Replace NAs in integer features with median value of column
  impute.median <- function(x) replace(x, is.na(x), round(median(x, na.rm = TRUE)))
  dt[, (int_features):=lapply(.SD, impute.median), .SDcols = int_features]
  
  # Normalize Id to approximate time of day
  minId <- min(dt$Id)
  dt[, normId:=((Id-minId) %% numSamplesPerDay) / numSamplesPerDay]
}

# Generate feature matrix from data table
GetFeatureMatrix <- function(dt)
{
  int_features <- grep('^I[0-9]+', colnames(dt), value=TRUE)
  cat_features <- grep('^C[0-9]+', colnames(dt), value=TRUE)
  missingCol_features <- grep('^Missing', colnames(dt), value = TRUE)
  
  # Use categorical features with at most 10 unique values
  numUniqueFeatureValues <- sapply(lapply(dt[, cat_features, with=FALSE], unique), length)
  catFeaturesToTrain <- cat_features[numUniqueFeatureValues <= maxUniqueValuesInCatFeatures]
  
  # return feature matrix
  cbind(poly(dt$normId, degree = 4, raw = TRUE),
        as.matrix(dt[, int_features, with = FALSE]),
        as.matrix(dt[, missingCol_features, with = FALSE]))
  #model.matrix(~ ., dt[, catFeaturesToTrain, with = FALSE]))
}

# Logistic regression with Glmnet
if (runGlmnet)
{
  # Load training  data
  if (!exists('train.dt'))
  {
    if (!file.exists('train.csv') )
    {
      stop('Cannot find input file train.csv.')
    }
    
    train.dt <- fread('train.csv', sep = ',', nrows = numTrainingSamples, header = TRUE)
    CleanseRawDatatable(train.dt)
  }
  
  # k-fold cross validation using glmnet
  train.fit <- cv.glmnet(x = GetFeatureMatrix(train.dt),
                         y = as.matrix(train.dt$Label),
                         nfolds = 10,
                         family = "binomial",
                         parallel = TRUE)
  
  # Compute LogLoss score on training set
  train.dt[, Predicted:=predict(train.fit,
                                GetFeatureMatrix(train.dt),
                                type = "response",
                                s = "lambda.min")]
  train.score <- llfun(as.numeric(train.dt$Label)-1, train.dt$Predicted)
  
  # Plot prediction vs label of cross validation data
  sm.density.compare(train.dt$Predicted, train.dt$Label)
}

# Write predictions to submission.csv
if (writeCSV) 
{
  # Load test data
  if (!exists('test.dt'))
  {
    if (!file.exists('test.csv'))
    {
      stop('Cannot find input file test.csv.')
    }
    test.dt <- fread('test.csv',
                     sep = ',',
                     header = TRUE)
    
    # Add Label column (set to NA) to test data to match the dimension of training data
    test.dt[, Label:=NA]
    
    CleanseRawDatatable(test.dt)
  }
  
  test.dt[, Predicted:=predict(train.fit,
                               GetFeatureMatrix(test.dt),
                               type = "response",
                               s = "lambda.min")]
  
  test.dt[, Predicted:=round(Predicted, 12)]
  
  write.csv(format(test.dt[,c('Id','Predicted'), with=FALSE],
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
