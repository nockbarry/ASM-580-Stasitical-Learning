#Quiz 6 AMS 580 
#Nicholas Barrett

library(tidyverse)
library(caret)
library(randomForest)

data <- read.csv('banknote.csv')

data$class <- as.factor(data$class)
str(data)

dim(data)[1] 
set.seed(123)
split <- data$class %>%
  createDataPartition(p = 0.75, list = FALSE)
train <- data[split,]
test <- data[-split,]
str(train) 
str(test) 

#Q2
set.seed(123)
model <- train(  class ~., data = train, method = "rf",
                 trControl = trainControl("cv", number = 10),
                 importance = TRUE  )
# Best tuning parameter
model$bestTune
cm = model$finalModel$confusion

my.ssa = function(cm){
  sens = cm[4]/(cm[2]+cm[4])
  specif = cm[1]/(cm[1]+cm[3])
  accu = (cm[1]+cm[4])/(cm[2]+cm[3]+cm[1]+cm[4])
  x = c(sens = sens, specif = specif, accu = accu)
  x
}

model$finalModel
my.ssa(cm)

#Q3
pred <- model %>% predict(test)
cm = table(test$class,pred)
cm
my.ssa(cm)

#Q4
# Plot MeanDecreaseAccuracy
varImpPlot(model$finalModel, type = 1)
# Plot MeanDecreaseGini
varImpPlot(model$finalModel, type = 2)

#Q5
varImp(model)

#Q6
sqrt(25)

