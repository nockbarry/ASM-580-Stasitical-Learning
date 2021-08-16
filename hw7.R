library(tidyverse)
library(caret)
library(neuralnet)
data <- read.csv('Titanic.csv')
data <- subset(data, select = -c(PassengerId,Name,Ticket,Cabin))
# Remember we have added "PassengerId" into the removal list
data <- subset(data, is.na(Age) == FALSE)
data$Embarked[data$Embarked == ""] =NA
data <- subset(data, is.na(Embarked) == FALSE)
data <- model.matrix(~., data = data)[,-1]
data <- data.frame(data)
dim(data)[1]

set.seed(123)
training.samples <- data$Survived %>%
  createDataPartition(p = 0.75, list = FALSE)
train.data <- data[training.samples, ]
test.data <- data[-training.samples, ]
str(train.data) # 534 obs
str(test.data)

#Q2
set.seed(123)
model = neuralnet(Survived~., data = train.data, hidden = 0, err.fct = "sse", linear.output = F)
plot(model, rep = "best")
probabilities = predict(model, test.data)
predicted.classes = ifelse(probabilities > 0.5, 1, 0)
confusionMatrix(factor(predicted.classes), factor(test.data$Survived), positive = "1")

#Q3
set.seed(123)
model = neuralnet(Survived~., data = train.data, hidden = 0, err.fct = "ce", linear.output = F)
plot(model, rep = "best")
probabilities = predict(model, test.data)
predicted.classes = ifelse(probabilities > 0.5, 1, 0)
confusionMatrix(factor(predicted.classes), factor(test.data$Survived), positive = "1")

#Q4
set.seed(123)
model = glm(Survived~., family = binomial, data = train.data)
model
probabilities = model %>% predict(test.data, type = "response")
predicted.classes = ifelse(probabilities > 0.5, 1, 0)
confusionMatrix(factor(predicted.classes), factor(test.data$Survived), positive = "1")

#Q5
set.seed(123)
model = neuralnet(Survived~., data = train.data, hidden = 3, err.fct = "sse", linear.output = F)
plot(model, rep = "best")
probabilities = predict(model, test.data)
predicted.classes = ifelse(probabilities > 0.5, 1, 0)
confusionMatrix(factor(predicted.classes), factor(test.data$Survived), positive = "1")

#Q6
set.seed(123)
model = neuralnet(Survived~., data = train.data, hidden = 3, err.fct = "ce", linear.output = F)
plot(model, rep = "best")
probabilities = predict(model, test.data)
predicted.classes = ifelse(probabilities > 0.5, 1, 0)
confusionMatrix(factor(predicted.classes), factor(test.data$Survived), positive = "1")
