train = read.csv("train.csv", stringsAsFactors = FALSE)
test = read.csv("test.csv", stringsAsFactors = FALSE)

train$train = 1
test$train = 0
test$SalePrice = -1

df = rbind(train, test)

variables <- names(df)
variables <- variables[!(variables %in%  c("SalePrice","train"))]

for(variable in variables)
{
  if(any(is.na(df[[variable]])))
  {
    print(paste(variable,"-",class(df[[variable]])))
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

# Deal with factors
for(variable in variables)
{
  if(is.character(df[[variable]]))
  {
    levels <- sort(unique(df[[variable]]))
    df[[variable]] <- factor(df[[variable]],levels=levels)
  }
}

train_res = subset(df, df$train == 1)
test_res = subset(df, df$train == 0)

train_res$train = NULL
test_res$train = NULL

# train_res$MSSubClass = as.factor(train_res$MSSubClass)
# test_res$MSSubClass = as.factor(test_res$MSSubClass)
train_res$MSSubClass = NULL
test_res$MSSubClass = NULL

#library(Boruta)
#bt = Boruta(train_res,train_res$SalePrice)
#m = lm(SalePrice ~ . - Id, data = train_res)

library(randomForest)
#rf <- randomForest(SalePrice ~ MSZoning + LotArea + Street + LotConfig + LandSlope + Neighborhood + Condition1 + Condition2 + OverallQual + OverallCond + YearBuilt + RoofMatl + RoofStyle + MasVnrArea + ExterQual + BsmtQual + BsmtExposure + BsmtFinSF1 + BsmtFinSF2 + X1stFlrSF +X2ndFlrSF + KitchenQual + GarageQual +GarageCond + GarageArea + PoolArea + PoolQC + Fireplaces + Functional + WoodDeckSF + Fence + SaleCondition + BedroomAbvGr + KitchenAbvGr + FullBath, data = train_res)
rf = randomForest(SalePrice ~ ., data = train_res)
p = as.data.frame(predict(rf, newdata = test_res))

colnames(p) = c("SalePrice")

write.csv(p,file="submission_rf.csv", row.names=TRUE)
