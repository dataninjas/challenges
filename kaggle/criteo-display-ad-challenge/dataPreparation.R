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
  
  # Convert type column to factor
  dt[, type:=as.factor(type)]
  
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
