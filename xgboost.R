train = read.csv("train.csv", stringsAsFactors = FALSE)
test = read.csv("test.csv", stringsAsFactors = FALSE)

train$train = 1
test$train = 0
test$SalePrice = -1
train$SalePrice = log(1 + train$SalePrice)

df = rbind(train, test)

df$age = df$YrSold - df$YearBuilt
df$mod_age = df$YrSold - df$YearRemodAdd
df$modified = as.factor(df$age - df$mod_age > 0)
df$garage_age = df$YrSold - df$GarageYrBlt
df$YearBuilt = as.factor(df$YearBuilt)
df$YearRemodAdd = as.factor(df$YearRemodAdd)
df$GarageYrBlt = as.factor(df$GarageYrBlt)
df$FullBath = as.factor(df$FullBath)
df$HalfBath = as.factor(df$HalfBath)
df$BsmtFullBath = as.factor(df$BsmtFullBath)
df$BsmtHalfBath = as.factor(df$BsmtHalfBath)

df$MoYo = as.factor(paste(df$MoSold, df$YrSold, sep = "-"))
df$YrSold = as.factor(df$YrSold)
df$MoSold = as.factor(df$MoSold)
df$MSSubClass = as.factor(df$MSSubClass)
df$OverallQual = as.factor(df$OverallQual)
df$OverallCond = as.factor(df$OverallCond)

for(variable in variables)
{
  if(any(is.na(df[[variable]])))
  {
    if(is.character(df[[variable]]))
    {
      df[[variable]][is.na(df[[variable]])] <- "Missing"
    }
    else
    {
      df[[variable]][is.na(df[[variable]])] <- mean(df[[variable]],na.rm=TRUE)
    }
  }

}

variables <- names(df)
# variables <- variables[!(variables %in%  c("SalePrice", "train", "Id", "MoYo", "age"))] #idk why moyo and age are bringing R2 down!

# sf_vars = c("WoodDeckSF", "OpenPorchSF", "BsmtFinSF1", "BsmtFinSF2", "BsmtUnfSF", "TotalBsmtSF", "X1stFlrSF", "X2ndFlrSF", "LowQualFinSF", "GrLivArea", "GarageArea", "EnclosedPorch", "3SsnPorch", "ScreenPorch", "PoolArea", "LotArea", "MasVnrArea")

# Deal with factors, and log the numeric variables!
for(variable in variables)
{
  if(is.character(df[[variable]]))
  {
    levels <- sort(unique(df[[variable]]))
    df[[variable]] <- factor(df[[variable]],levels=levels)
  }
  # else if (variable %in% sf_vars){ #now, only log the SF variables -- this is giving lower score on Public LB
  #   df[[variable]] = log(1 + df[[variable]])
  # }
  else if (is.numeric(df[[variable]])){
    df[[variable]] = log(1 + df[[variable]])
  }
}

# This is increasing RMSE :(
# for (variable in sf_vars){
#   new_var = paste(variable, "zero", sep = "_")
#   df[[new_var]] = as.factor(df[[variable]] == 0)
# }

variables <- names(df)
variables <- variables[!(variables %in%  c("SalePrice", "train", "Id", "MoYo", "age"))] #idk why moyo and age are bringing R2 down!

train_res = subset(df, df$train == 1)
test_res = subset(df, df$train == 0)

train_res$train = NULL
test_res$train = NULL
test_res$SalePrice = NULL

trainData<- as.matrix(train_res, rownames.force = NA)
testData<- as.matrix(test_res, rownames.force = NA)

train2 <- as(trainData, "sparseMatrix")
test2 <- as(testData, "sparseMatrix")

library(xgboost)
trainD <- xgb.DMatrix(data = train2[,variables], label = train2[,"SalePrice"])
