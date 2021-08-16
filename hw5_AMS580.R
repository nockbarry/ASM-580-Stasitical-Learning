# AMS580 HW 5
# Nicholas Barrett


library(MASS)
library(tidyverse)
library(caret)
library(glmnet)
library(caTools)

#Q1

data("Boston", package = "MASS")
set.seed(123)

split <- sample.split(1:dim(Boston)[1], SplitRatio = 0.75)
train.data <- Boston[split,]
test.data <- Boston[!split,]

#Q2

x <- model.matrix(medv~., train.data)[,-1]
y <- train.data$medv

cv <- cv.glmnet(x, y, alpha = 0)
cv$lambda.min
#Ridge
model <- glmnet(x, y, alpha = 0, lambda = cv$lambda.min) 
coef(model)

x.test <- model.matrix(medv ~., test.data)[,-1]
predictions <- model %>% predict(x.test) %>% as.vector()
data.frame(
  RMSE = RMSE(predictions, test.data$medv),
  Rsquare = R2(predictions, test.data$medv)
)


#Q3
cv <- cv.glmnet(x, y, alpha = 1)
cv$lambda.min
#LASSO
model <- glmnet(x, y, alpha = 1, lambda = cv$lambda.min) 
coef(model)

x.test <- model.matrix(medv ~., test.data)[,-1]
predictions <- model %>% predict(x.test) %>% as.vector()

data.frame(
  RMSE = RMSE(predictions, test.data$medv),
  Rsquare = R2(predictions, test.data$medv)
)

#Q4
#Elastic Net
model <- train(
  medv ~., data = train.data, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
model$bestTune

coef(model$finalModel, model$bestTune$lambda)

x.test <- model.matrix(medv ~., test.data)[,-1]
predictions <- model %>% predict(x.test)
data.frame(
  RMSE = RMSE(predictions, test.data$medv),
  Rsquare = R2(predictions, test.data$medv)
)


