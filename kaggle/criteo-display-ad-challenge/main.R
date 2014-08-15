require(data.table) # fast aggregation of large data
require(colbycol)   # efficient parsing of csv and random data sampling
require(sm)         # compare density function for classification
require(lattice)    # scatter plots
require(hexbin)     # high density plots with binning

# input parameters
samplingRate = 0.00001

# load from CSV if necessary
if (!exists('train.dt'))
{
  if (!file.exists('train.csv'))
  {
    stop('Cannot find input file train.csv.')
  }

  train.cbc <- cbc.read.table('train.csv',
                         sample.pct = samplingRate,
                         sep = ',',
                         header = TRUE)
  
  train.dt <- as.data.table(as.data.frame(train.cbc))
  
  # Convert all values to integers (more memory efficient and easier to plot)
  train.dt <- train.dt[, lapply(.SD, as.integer)]
  
  # TODO: normalize the integer features and removal outliers
  
  # Convert label to factor
  train.dt[,Label:=as.factor(Label)]
}

# Compare PDF of feature I2 for Label 0 vs 1
sm.density.compare(train.dt$I2, train.dt$Label)

# Scatter plot for features I1:I5 for Label 0 vs 1
train.df <- as.data.frame(train.dt)
#splom(train.df[3:7], groups = train.df$Label)

# Scatter plot with binning for feature I3 vs I4
hexbinplot(I3 ~ I4 | Label, train.df)