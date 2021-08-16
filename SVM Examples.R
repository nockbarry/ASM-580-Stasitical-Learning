library(tidyverse)
library(caret)
# Load the data
data("PimaIndiansDiabetes2", package = "mlbench")
pima.data <- na.omit(PimaIndiansDiabetes2)
# Predicting the probability of being diabetes positive based on multiple clinical variables
# Inspect the data
sample_n(pima.data, 3)
# Split the data into training and test set
set.seed(123)
training.samples <- pima.data$diabetes %>%  createDataPartition(p = 0.8, list = FALSE)
train.data  <- pima.data[training.samples, ]
test.data <- pima.data[-training.samples, ]

#1
# Fit the model on the training set
model <- train(  diabetes ~., data = train.data, 
                 method = "svmLinear",  
                 trControl = trainControl("cv", number = 10),  
                 preProcess = c("center","scale")  
                 )

# Make predictions on the test data
predicted.classes <- model %>% predict(test.data)
head(predicted.classes)

# Compute model accuracy rate
mean(predicted.classes == test.data$diabetes)


#with tuning parameter C

# Fit the model on the training set

model <- train(  diabetes ~., data = train.data, 
                 method = "svmLinear",  
                 trControl = trainControl("cv", number = 10),  
                 tuneGrid = expand.grid(C = seq(0.1, 2, length = 19)),  
                 preProcess = c("center","scale") 
                 )
# Plot model accuracy vs different values of Cost
plot(model)
model$bestTune

# Make predictions on the test data
predicted.classes <- model %>% predict(test.data)
# Compute model accuracy rate
mean(predicted.classes == test.data$diabetes)

#non linear kernal

# Fit the model on the training set
model <- train(  diabetes ~., data = train.data, 
                 method = "svmRadial",  
                 trControl = trainControl("cv", number = 10),  
                 preProcess = c("center","scale"),  
                 tuneLength = 10  
                 )
# Print the best tuning parameter sigma and C that
# maximizes model accuracy
model$bestTune
#Make predictions on the test data
predicted.classes <- model %>% predict(test.data)
# Compute model accuracy rate
mean(predicted.classes == test.data$diabetes)

#Polynomial kernal

# Fit the model on the training set
model <- train(  diabetes ~., 
                 data = train.data, 
                 method = "svmPoly",  
                 trControl = trainControl("cv", number = 10),  
                 preProcess = c("center","scale"),  
                 tuneLength = 4  
                 )
# Print the best tuning parameter sigma and C that
# maximizes model accuracy
model$bestTune

# Make predictions on the test data
predicted.classes <- model %>% predict(test.data)
# Compute model accuracy rate
mean(predicted.classes == test.data$diabetes)



#