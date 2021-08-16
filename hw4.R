library(tidyverse)
library(caret)
library(randomForest)

data <- read.csv('Titanic.csv')

data <- subset(data, select = -c(PassengerId,Name,Ticket,Cabin))
# Remember we have added "PassengerId" into the removal list
data <- subset(data, is.na(Age) == FALSE)
data$Survived <- as.factor(data$Survived)
str(data)

dim(data)[1] #714 passengers
set.seed(123)
split <- data$Survived %>%
  createDataPartition(p = 0.75, list = FALSE)
train <- data[split,]
test <- data[-split,]
str(train) #536
str(test) #178

#Q2
set.seed(123)
model <- train(  Survived ~., data = train, method = "rf",
                 trControl = trainControl("cv", number = 10),
                 importance = TRUE  )
# Best tuning parameter
model$bestTune
model$finalModel
cm = model$finalModel$confusion

my.ssa = function(cm){
  sens = cm[4]/(cm[2]+cm[4])
  specif = cm[1]/(cm[1]+cm[3])
  accu = (cm[1]+cm[4])/(cm[2]+cm[3]+cm[1]+cm[4])
  x = c(sens = sens, specif = specif, accu = accu)
  x
}
my.ssa(cm)
varImpPlot(model$finalModel,type = 1)
varImpPlot(model$finalModel,type = 2)
varImp(model)
#Q3
pred <- model %>% predict(test)
cm = table(test$Survived,pred)
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
sqrt(36)
#diffetent way below
library(caTools)
set.seed(123)
split <- sample.split(data$Survived, SplitRatio = 0.75)
train <- data[split,]
test <- data[!split,]
str(train) #536
str(test) #178
fit <- randomForest(Survived ~ ., data = train, importance = TRUE)
fit
cm = fit$confusion
my.ssa(cm)

