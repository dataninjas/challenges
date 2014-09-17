# More info: http://www.r-bloggers.com/introduction-to-feature-selection-for-bioinformaticians-using-r-correlation-matrix-filters-pca-backward-selection/

# source our custom functions
source('dataPreparation.R')

# load required libraries
require(corrplot) #corrplot: the library to compute correlation matrix.
require(caret)

#trainFile <- 'train.csv'
trainFile <- 'train_sample_10000.csv'
train.dt <- fread(trainFile, sep = ',', header = TRUE)
CleanseRawDatatable(train.dt)

# remove first two columns and last column
train.dt.features <- train.dt[, 4:ncol(data.frame(train.dt))-1, with=FALSE]

# scale the features
train.dt.numeric.scaled <- scale(train.dt.features[,sapply(train.dt.features, is.numeric),with=FALSE], center=TRUE, scale=TRUE);
train.dt.numeric.scaled <- train.dt.numeric.scaled[,c(1:12)]

# compute the correlation matrix
# take only the original integer columns, not the matrix columns
corMatrix <- cor(train.dt.numeric.scaled)

#visualize the matrix, clustering features by correlation index.
corrplot(corMatrix, order = "hclust")

require(FactoMineR) 
# PCA with function PCA

#scale all the features,  ncp: number of dimensions kept in the results (by default 5)
pca <- PCA(train.dt.numeric.scaled, scale.unit=TRUE, ncp=5, graph=T)

dimdesc(pca)
#This line of code will sort the variables the most linked to each PC. It is very useful when you have many variables.
