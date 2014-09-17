# source our custom functions
source('dataPreparation.R')

# load required libraries
require(corrplot) #corrplot: the library to compute correlation matrix.

#trainFile <- 'train.csv'
trainFile <- 'train_sample_10000.csv'
train.dt <- fread(trainFile, sep = ',', header = TRUE)
CleanseRawDatatable(train.dt)

# remove first two columns and last column
train.dt.features <- train.dt[, 4:ncol(data.frame(train.dt))-1, with=FALSE]

# scale the features
train.dt.numeric.scaled <- scale(train.dt.features[,sapply(train.dt.features, is.numeric),with=FALSE], center=TRUE, scale=TRUE);

# compute the correlation matrix
# take only the original integer columns, not the matrix columns
corMatrix <- cor(train.dt.numeric.scaled[,c(1:12)])

#visualize the matrix, clustering features by correlation index.
corrplot(corMatrix, order = "hclust")
