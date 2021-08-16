library(tidyverse)
library(caret)
library(neuralnet)
data <- read.csv('banknote.csv')
na.omit(data)

data <- data.frame(data)
dim(data)[1]
set.seed(123)

training.samples <- data$class %>%
  createDataPartition(p = 0.75, list = FALSE)
train.data <- data[training.samples, ]
test.data <- data[-training.samples, ]
str(train.data) # 1029 obs
str(test.data)

#Q2
set.seed(123)
model = neuralnet(class~., data = train.data, hidden = 0, err.fct = "sse", linear.output = F)
plot(model, rep = "best")
probabilities = predict(model, test.data)
predicted.classes = ifelse(probabilities > 0.5, 1, 0)
confusionMatrix(factor(predicted.classes), factor(test.data$class), positive = "1")

#Q3
set.seed(123)
model = neuralnet(class~., data = train.data, hidden = 0, err.fct = "ce", linear.output = F)
plot(model, rep = "best")
probabilities = predict(model, test.data)
predicted.classes = ifelse(probabilities > 0.5, 1, 0)
confusionMatrix(factor(predicted.classes), factor(test.data$class), positive = "1")

#Q4
set.seed(123)
model = glm(class~., family = binomial, data = train.data)
model
probabilities = model %>% predict(test.data, type = "response")
predicted.classes = ifelse(probabilities > 0.5, 1, 0)
confusionMatrix(factor(predicted.classes), factor(test.data$class), positive = "1")
#most closely mastches the Q3 model with ce loss function. both accuracy and coefficients
# are close, but the coefficients were close to Q2 as well


#Q5
set.seed(123)
model = neuralnet(class~., data = train.data, hidden = 3, err.fct = "sse", linear.output = F)
plot(model, rep = "best")
probabilities = predict(model, test.data)
predicted.classes = ifelse(probabilities > 0.5, 1, 0)
confusionMatrix(factor(predicted.classes), factor(test.data$class), positive = "1")
#100% accuracy, better than 2

#Q6
set.seed(123)
model = neuralnet(class~., data = train.data, hidden = 3, err.fct = "ce", linear.output = F)
plot(model, rep = "best")
probabilities = predict(model, test.data)
predicted.classes = ifelse(probabilities > 0.5, 1, 0)
confusionMatrix(factor(predicted.classes), factor(test.data$class), positive = "1")
#100% accuracy, better than 3