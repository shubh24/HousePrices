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

train_res$MSSubClass = as.factor(train_res$MSSubClass)
test_res$MSSubClass = as.factor(test_res$MSSubClass)

library(randomForest)
rf <- randomForest(SalePrice~.,train_res)
p = as.data.frame(predict(rf, newdata = test_res))

colnames(p) = c("SalePrice")

write.csv(p,file="submission_rf.csv", row.names=TRUE)