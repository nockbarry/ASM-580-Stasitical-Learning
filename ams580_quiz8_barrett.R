#AMS 580 Quiz 8 Nicholas Barrett

my.ssa = function(cm){
  sens = cm[4]/(cm[2]+cm[4])
  specif = cm[1]/(cm[1]+cm[3])
  accu = (cm[1]+cm[4])/(cm[2]+cm[3]+cm[1]+cm[4])
  x = c(sens = sens, specif = specif, accu = accu)
  x
}

library(tidyverse)
library(caret)
data = read.csv('banknote.csv')
data <- na.omit(data)
data$class = factor(data$class)
sample_n(data, 3)

#Q1
dim(data)[1]
# 1372 observations remain
set.seed(123)
training.samples <- data$class %>%  createDataPartition(p = 0.75, list = FALSE)
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]

#Q2

model <- train(  class ~., data = train.data, 
                 method = "svmLinear",  
                 trControl = trainControl("cv", number = 10),  
                 preProcess = c("center","scale")  
                 )

predicted.classes <- model %>% predict(test.data)
model
model$finalModel
cm = table(test.data$class,predicted.classes)
cm
my.ssa(cm)

#Q3

model <- train(  class ~., data = train.data, 
                 method = "svmLinear",  
                 trControl = trainControl("cv", number = 10),  
                 tuneGrid = expand.grid(C = seq(0.1, 2, length = 19)),  
                 preProcess = c("center","scale") 
                 )
model$bestTune

predicted.classes <- model %>% predict(test.data)

cm = table(test.data$class,predicted.classes)
cm
my.ssa(cm)



#Q4

model <- train(  class ~., data = train.data, 
                 method = "svmRadial",  
                 trControl = trainControl("cv", number = 10),  
                 preProcess = c("center","scale"),  
                 tuneLength = 10  
                 )

model$bestTune

predicted.classes <- model %>% predict(test.data)
cm = table(test.data$class,predicted.classes)
cm
my.ssa(cm)

#Q5

model <- train(  class ~., 
                 data = train.data, 
                 method = "svmPoly",  
                 trControl = trainControl("cv", number = 10),  
                 preProcess = c("center","scale"),  
                 tuneLength = 4  
                 )

model$bestTune

predicted.classes <- model %>% predict(test.data)

cm = table(test.data$class,predicted.classes)
cm
my.ssa(cm)


#Q6

#Based on these results both the radial basis kernal and polynomial kernal give the highest accuracy of 100%
