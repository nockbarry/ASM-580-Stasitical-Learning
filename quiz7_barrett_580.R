# AMS580 Quiz 7
# Nicholas Barrett


library(MASS)
library(tidyverse)
library(caret)
library(glmnet)
library(caTools)

#Q1

data = read.csv('wine.csv',header=TRUE)
set.seed(123)

split <- sample.split(1:dim(data)[1], SplitRatio = 0.75)
train.data <- data[split,]
test.data <- data[!split,]
str(test.data)
str(train.data)
#Q2

x <- model.matrix(quality~., train.data)[,-1]
y <- train.data$quality

cv <- cv.glmnet(x, y, alpha = 0)
cv$lambda.min
#Ridge
model <- glmnet(x, y, alpha = 0, lambda = cv$lambda.min) 
coef(model)

x.test <- model.matrix(quality ~., test.data)[,-1]
predictions <- model %>% predict(x.test) %>% as.vector()
data.frame(
  RMSE = RMSE(predictions, test.data$quality),
  Rsquare = R2(predictions, test.data$quality)
)


#Q3
cv <- cv.glmnet(x, y, alpha = 1)
cv$lambda.min
#LASSO
model <- glmnet(x, y, alpha = 1, lambda = cv$lambda.min) 
coef(model)

x.test <- model.matrix(quality ~., test.data)[,-1]
predictions <- model %>% predict(x.test) %>% as.vector()

data.frame(
  RMSE = RMSE(predictions, test.data$quality),
  Rsquare = R2(predictions, test.data$quality)
)

#Q4
#Elastic Net
model <- train(
  quality ~., data = train.data, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
model$bestTune

coef(model$finalModel, model$bestTune$lambda)

x.test <- model.matrix(quality ~., test.data)[,-1]
predictions <- model %>% predict(x.test)
data.frame(
  RMSE = RMSE(predictions, test.data$quality),
  Rsquare = R2(predictions, test.data$quality)
)


