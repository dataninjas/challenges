require(data.table)   # fast aggregation of large data
require(sm)           # compare density function for classification
require(glmnet)       # regularized logistic regression
require(Matrix)       # sparse matrix

# input parameters
runLR <- TRUE
writeCSV <- TRUE
numTrainingSamples <- 5000000
numCVSamples <- 2500000
numSamplesPerDay <- 6548660 # computed by numRows in train.csv / 7
maxUniqueValuesInCatFeatures <- 10 # only use categorical features with <= 10 unique values (memory constraint)

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
}

# Generate feature matrix from data table
GetFeatureMatrix <- function(dt)
{
  int_features <- grep('^I[0-9]+', colnames(dt), value=TRUE)
  cat_features <- grep('^C[0-9]+', colnames(dt), value=TRUE)
  missingCol_features <- grep('^Missing', colnames(dt), value = TRUE)
  
  # Use categorical features with at most 10 unique values
  numUniqueFeatureValues <- sapply(lapply(train.dt[, cat_features, with=FALSE], unique), length)
  catFeaturesToTrain <- cat_features[numUniqueFeatureValues <= maxUniqueValuesInCatFeatures]
  
  # Use normalized Id to approximate time of day
  normId <- ((dt$Id - 1e7) %% numSamplesPerDay) / numSamplesPerDay
  
  # return feature matrix
  cbind(as.matrix(dt[, int_features, with = FALSE]),
        as.matrix(dt[, missingCol_features, with = FALSE]),
        model.matrix(~ ., dt[, catFeaturesToTrain, with = FALSE]),
        poly(normId, degree = 4, raw = TRUE))
}

# Load training and cross validation data
if (!exists('train.dt') || !exists('cv.dt'))
{
  if (!file.exists('train.csv') )
  {
    stop('Cannot find input file train.csv.')
  }
  
  set.seed(1)
  numRows <- numTrainingSamples + numCVSamples
  randomSeq <- sample(1:numRows, numRows)
  
  data.dt <- fread('train.csv',
                   sep = ',',
                   nrows = numRows,
                   header = TRUE)
  
  CleanseRawDatatable(data.dt)
  
  train.dt <- data.dt[randomSeq[1:numTrainingSamples],]
  cv.dt <- data.dt[randomSeq[(numTrainingSamples+1):numRows],]
  
  rm(data.dt)
}

# Logistic regression
if (runLR)
{
  # train regularized logistic model with training set
  train.lr <- glmnet(GetFeatureMatrix(train.dt),
                     as.matrix(train.dt$Label),
                     family = "binomial")
  
  # run predictions on cross validation set and find the best score
  cv.scores <- sapply(as.data.frame(predict(train.lr, 
                                            GetFeatureMatrix(cv.dt),
                                            type = "response")),
                      function(x) { llfun(as.numeric(cv.dt$Label)-1, x) })
  cv.bestscore <- min(cv.scores)
  cv.bestlambda <- train.lr$lambda[which.min(cv.scores)]
  cv.dt[, Predicted:=predict(train.lr, 
                             GetFeatureMatrix(cv.dt),
                             type = "response",
                             s = cv.bestlambda)]
  
  # Plot prediction vs label of cross validation data
  sm.density.compare(cv.dt$Predicted, cv.dt$Label)
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
    
    # Add Label column (set to NA) to test data to match the dimension of trainig data
    test.dt[, Label:=NA]
    
    CleanseRawDatatable(test.dt)
  }
  
  test.dt[, Predicted:=predict(train.lr,
                               GetFeatureMatrix(test.dt),
                               type = "response",
                               s = cv.bestlambda)]
  
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
