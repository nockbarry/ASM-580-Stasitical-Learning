#AMS 580 Midterm 1 Nicholas Barrett
#Q1
library(carData)
library.dynam.unload()
data = read.csv('C:\\Users\\Nick\\OneDrive\\Documents\\Spring 2021\\AMS 580 Stat Learn\\diamond2.csv', header = TRUE)

library(fastDummies)
library(tidyverse)
library(caret)
library(leaps) 
library(MASS)

models <- regsubsets(price~., data = data, nvmax = 6) 
summary(models)
#16 variables including dummies

get_model_formula <- function(id, object, outcome){ 
  # get models data  
  models <- summary(object)$which[id,-1]
  # Get outcome variable
  form <- as.formula(object$call[[2]])
  outcome <- all.vars(form)[1]
  # Get model predictors  
  predictors <- names(which(models == TRUE))   
  predictors <- paste(predictors, collapse = "+") 
  # Build model formula   
  as.formula(paste0(outcome, "~", predictors))
}

get_cv_error <- function(model.formula, data){
  set.seed(1)
  train.control <- trainControl(method = "cv", number = 5)  
  cv <- train(model.formula, data = data, method = "lm",trControl = train.control)  
  cv$results$RMSE
}
#b

dataf <- dummy_cols(data, select_columns = c('color','clarity'),remove_selected_columns = TRUE)
names(dataf)<-gsub("\\_","",names(dataf))

model.ids <- 1:6
cv.errors <-  map(model.ids, get_model_formula, models, "price") %>%  
  map(get_cv_error, data = dataf) %>%  
  unlist()
cv.errors

which.min(cv.errors)
coef(models, 6)
#best model
#(Intercept)       carat      colorH      colorI      colorJ  claritySI1  claritySI2 
#-2041.734    8361.978    -577.504   -1094.594   -1957.439    -829.979   -1679.735

#c
res.lm <- lm(price ~., data = dataf)
step <- stepAIC(res.lm, direction = "both", trace = FALSE) 
step

#Q2
data = read.csv('C:\\Users\\Nick\\OneDrive\\Documents\\Spring 2021\\AMS 580 Stat Learn\\wine.csv', header = TRUE)

str(data)
data = na.omit(data)
pc = princomp(data[,2:14], cor = T)
# Scree-plot
library(factoextra)
fviz_eig(pc)
# K means
k.means.fit <- kmeans(data[,2:14], 3) 
library(cluster)
clusplot(data, k.means.fit$cluster, main='2D representation of the Cluster solution',color=TRUE, shade=TRUE,labels=2, lines=0)

table(data[,1],k.means.fit$cluster)

d <- dist(data[,2:14], method = "euclidean")
H.fit <- hclust(d, method="ward.D")

plot(H.fit)
rect.hclust(H.fit, k=3, border="red")

groups <- cutree(H.fit, k=3)
clusplot(data, groups, main='2D representation of the Cluster solution',color=TRUE, shade=TRUE,labels=2, lines=0)
clusters = factor(groups, levels = 1:3, labels = c("CultiVar1", "CultiVar2",  "CultiVar3"))
table(data[,1], clusters)

#accuracy of Kmeans is 46+50+29/150 = 125/150
#accuracy of ward is 53+16+51/150 = 120
#so pretty close

#3
data = iris
str(data)

set.seed(1)
intrain <- createDataPartition(y = data$Species, p= 0.7, list = FALSE)
training <- data[intrain,]
testing <- data[-intrain,]

#b
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(1)

knn_fit <- train(training$Species ~., data = training, method = "knn", trControl=trctrl, preProcess= c("center", "scale"), tuneLength = 10)
knn_fit
#i keep getting this error
# Error in `[.data.frame`(data, , all.vars(Terms), drop = FALSE) : 
# undefined columns selected
#i cannot find what the problem is

plot(knn_fit$results$k, knn_fit$results$Accuracy, xlab = "#Neighbors", ylab = "Accuracy")

#c
test_pred <- predict(knn_fit, newdata = testing)
test_pred
confusionMatrix(test_pred, testing$V1)





