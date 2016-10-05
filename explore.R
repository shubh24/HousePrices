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

# for(variable in variables)
# {
#   if(any(is.na(df[[variable]])))
#   {
#     if(is.character(df[[variable]]))
#     {
#       # df[[variable]][is.na(df[[variable]])] <- "Missing"
#     }
#     else
#     {
#       df[[variable]][is.na(df[[variable]])] <- mean(df[[variable]],na.rm=TRUE)
#     }
#   }
# 
# }

variables <- names(df)
variables <- variables[!(variables %in%  c("SalePrice", "train", "Id", "MoYo", "age"))] #idk why moyo and age are bringing R2 down!

sf_vars = c("WoodDeckSF", "OpenPorchSF", "BsmtFinSF1", "BsmtFinSF2", "BsmtUnfSF", "TotalBsmtSF", "X1stFlrSF", "X2ndFlrSF", "LowQualFinSF", "GrLivArea", "GarageArea", "EnclosedPorch", "3SsnPorch", "ScreenPorch", "PoolArea", "LotArea", "MasVnrArea")

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


# Not giving any improvements
# train$YrSold = as.factor(train$YrSold)
# price_ave = aggregate(SalePrice ~ YrSold, data = train, FUN = mean)
# colnames(price_ave) = c("YrSold", "price_ave")
# df = merge(df, price_ave, by = "YrSold")
# 
# train$MoSold = as.factor(train$MoSold)
# price_ave_month = aggregate(SalePrice ~ MoSold, data = train, FUN = mean)
# colnames(price_ave_month) = c("MoSold", "price_ave_month")
# df = merge(df, price_ave_month, by = "MoSold")

variables <- names(df)
variables <- variables[!(variables %in%  c("SalePrice", "train", "Id", "MoYo", "age"))] #idk why moyo and age are bringing R2 down!

train_res = subset(df, df$train == 1)
test_res = subset(df, df$train == 0)

train_res$train = NULL
test_res$train = NULL

library(h2o)
h2o.init()
write.table(train_res, gzfile('./train.csv.gz'),quote=F,sep=',',row.names=F)
write.table(test_res, gzfile('./test.csv.gz'),quote=F,sep=',',row.names=F)

train_h2o = h2o.uploadFile("./train.csv.gz", destination_frame = "house_prices_train")
test_h2o = h2o.uploadFile("./test.csv.gz", destination_frame = "house_prices_test")
#Try looking at moving averages, and better ways to impute missing vals --> removed, also remove outliers --> don't seem like any outliers present!
rf_prev = h2o.randomForest(x = variables, y = "SalePrice", training_frame = train_h2o, ntrees = 200, max_depth = 50)

a = h2o.varimp(rf_prev)
sel_vars = a$variable[1:30]
rf = h2o.randomForest(x = sel_vars, y = "SalePrice", training_frame = train_h2o, ntrees = 200, max_depth = 50)

res = exp(predict(rf, test_h2o)) + 1
sp = as.vector(res)
ids = as.vector(test_res$Id)
pred <- data.frame(Id = ids, SalePrice = sp)
write.csv(pred, "submission_rf_h2o.csv", row.names = FALSE)

# glm = h2o.glm(x = variables, y = "SalePrice", training_frame = train_h2o,family = "gaussian")
# res = exp(predict(glm, test_h2o)) + 1
# sp = as.vector(res)
# ids = as.vector(test_res$Id)
# pred <- data.frame(Id = ids, SalePrice = sp)
# write.csv(pred, "submission_glm_h2o.csv", row.names = FALSE)

# require(caTools)
# set.seed(101) 
# sample = sample.split(train_res, SplitRatio = .75)
# train_train = subset(train_res, sample == TRUE)
# val_train = subset(train_res, sample == FALSE)
# write.table(train_train, gzfile('./train.csv.gz'),quote=F,sep=',',row.names=F)
# write.table(val_train, gzfile('./val.csv.gz'),quote=F,sep=',',row.names=F)
# write.table(test_res, gzfile('./test.csv.gz'),quote=F,sep=',',row.names=F)
# train_h2o = h2o.uploadFile("./train.csv.gz", destination_frame = "house_prices_train")
# val_h2o = h2o.uploadFile("./val.csv.gz", destination_frame = "house_prices_val")
# test_h2o = h2o.uploadFile("./test.csv.gz", destination_frame = "house_prices_test")
# gbm <- h2o.gbm(x = variables, y = "SalePrice", training_frame = train_h2o, validation_frame = val_h2o, ntrees = 1000, learn_rate = 0.01, sample_rate = 0.8, col_sample_rate = 0.8, stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "r2")
# res = exp(predict(rf, test_h2o)) + 1
# sp = as.vector(res)
# ids = as.vector(test_res$Id)
# pred <- data.frame(Id = ids, SalePrice = sp)
# write.csv(pred, "submission_gbm_h2o.csv", row.names = FALSE)

#library(Boruta)
#bt = Boruta(train_res,train_res$SalePrice)
#m = lm(SalePrice ~ . - Id, data = train_res)
