#Part 1
my.ssa = function(cm){
  sens = cm[4]/(cm[2]+cm[4])
  specif = cm[1]/(cm[1]+cm[3])
  accu = (cm[1]+cm[4])/(cm[2]+cm[3]+cm[1]+cm[4])
  x = c(sens = sens, specif = specif, accu = accu)
  x
}

#1.

library(tidyverse)
library(caret)
library(neuralnet)
library(randomForest)
library(rpart)
library(rattle)
library(MASS)
library(tidyverse)
library(glmnet)
library(leaps)
library(ggplot2)

data <- read.csv('Titanic.csv')
data <- subset(data, select = -c(PassengerId,Name,Ticket,Cabin))
# Remember we have added "PassengerId" into the removal list
data <- subset(data, is.na(Age) == FALSE)
data$Embarked[data$Embarked == ""] =NA
data <- subset(data, is.na(Embarked) == FALSE)
data <- model.matrix(~., data = data)[,-1]
data <- data.frame(data)
cat('There are', nrow(data), 'observations left.')

set.seed(123)
training.samples <- data$Survived %>% createDataPartition(p = 0.75, list = FALSE)
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]




#2
#a)
set.seed(123)
model = neuralnet(Survived~., data = train.data, hidden = 0, err.fct = "sse", linear.output = F)
plot(model, rep = "best")

probabilities = predict(model, test.data)
predicted.classes = ifelse(probabilities > 0.5, 1, 0)

confusionMatrix(factor(predicted.classes), factor(test.data$Survived), positive = "1")

#b)
set.seed(123)
model = neuralnet(Survived~., data = train.data, hidden = 0, err.fct = "ce", linear.output = F)
plot(model, rep = "best")

probabilities = predict(model, test.data)
predicted.classes = ifelse(probabilities > 0.5, 1, 0)

confusionMatrix(factor(predicted.classes), factor(test.data$Survived), positive = "1")

#c)
set.seed(123)
model = glm(Survived~., family = binomial, data = train.data)

probabilities = model %>% predict(test.data, type = "response")
predicted.classes = ifelse(probabilities > 0.5, 1, 0)

confusionMatrix(factor(predicted.classes), factor(test.data$Survived), positive = "1")

#d)
set.seed(123)
model = neuralnet(Survived~., data = train.data, hidden = 3, err.fct = "sse", linear.output = F)
plot(model, rep = "best")

probabilities = predict(model, test.data)
predicted.classes = ifelse(probabilities > 0.5, 1, 0)

confusionMatrix(factor(predicted.classes), factor(test.data$Survived), positive = "1")

#e)
set.seed(123)
model = neuralnet(Survived~., data = train.data, hidden = 3, err.fct = "ce", linear.output = F)
plot(model, rep = "best")

probabilities = predict(model, test.data)
predicted.classes = ifelse(probabilities > 0.5, 1, 0)

confusionMatrix(factor(predicted.classes), factor(test.data$Survived), positive = "1")

#3
# a)
train.data$Survived <- as.factor(train.data$Survived)
test.data$Survived <- as.factor(test.data$Survived)
set.seed(123)
model <- train(
  Survived ~., data = train.data, method = "rf",
  trControl = trainControl("cv", number = 10),
  importance = TRUE
  )
# Best tuning parameter
model$bestTune
model$finalModel

my.ssa(model$finalModel$confusion)




pred <- model %>% predict(test.data)
table(test.data$Survived,pred)
my.ssa(table(test.data$Survived,pred))


#c)
# Plot MeanDecreaseAccuracy
varImpPlot(model$finalModel, type = 1)
# Plot MeanDecreaseGini
varImpPlot(model$finalModel, type = 2)
#d)
varImp(model)
#e)
sqrt(36)


#4

#a)
model <- rpart(Survived ~., data = train.data, control = rpart.control(cp=0))
par(xpd = NA)
fancyRpartPlot(model)
pred <- predict(model,newdata = test.data, type ='class')
table(test.data$Survived,pred)
my.ssa(table(test.data$Survived,pred))

# b)
set.seed(123)
model2 <- train(
  Survived ~., data = train.data, method = "rpart",
  trControl = trainControl("cv", number = 10),
  tuneLength = 100)
model2
plot(model2)

model2$bestTune
fancyRpartPlot(model2$finalModel)

# c)
pred <- predict(model2, newdata = test.data)
table(test.data$Survived,pred)
my.ssa(table(test.data$Survived,pred))
#Sensitivity

40/(40+38)

#Specificity

98/(98+2)

#Accuracy
(40+98)/(40+98+38+2)

#5.

#a)
# Fit the model on the training set
set.seed(123)
model <- train(
  Survived ~., data = train.data, method = "svmLinear",
  trControl = trainControl("cv", number = 10)
  )

# Make predictions on the test data
predicted.classes <- model %>% predict(test.data)
head(predicted.classes)

# Confusion matrix
table(test.data$Survived, predicted.classes)
my.ssa(table(test.data$Survived,predicted.classes))
# Sensitivity (predicted == 1|Survived == 1)
55/(55+23)

# Specificity (predicted == 0|Survived == 0)
88/(88+12)

# Compute model accuracy rate
mean(predicted.classes == test.data$Survived)
# 0.8033708

# b)
# Fit the model on the training set
set.seed(123)
model <- train(
  Survived ~., data = train.data, method = "svmLinear",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(C = seq(.1, 2, length = 19)),
  )

# Plot model accuracy vs different values of Cost
plot(model)

# Print the best tuning parameter C that
# maximizes model accuracy
model$bestTune

# Make predictions on the test data
predicted.classes <- model %>% predict(test.data)

# Confusion matrix
table(test.data$Survived, predicted.classes)
my.ssa(table(test.data$Survived,predicted.classes))


#c)
# Fit the model on the training set
set.seed(123)
model <- train(
  Survived ~., data = train.data, method = "svmRadial",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale"),
  tuneLength = 10
  )
# Print the best tuning parameter sigma and C that
# maximizes model accuracy
model$bestTune

# Make predictions on the test data
predicted.classes <- model %>% predict(test.data)

# Confusion matrix
table(test.data$Survived, predicted.classes)
my.ssa(table(test.data$Survived,predicted.classes))


# d)
# Fit the model on the training set
set.seed(123)
model <- train(
  Survived ~., data = train.data, method = "svmPoly",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale"),
  tuneLength = 4
  )
# Print the best tuning parameter sigma and C that
# maximizes model accuracy
model$bestTune

# Make predictions on the test data
predicted.classes <- model %>% predict(test.data)

# Confusion matrix
table(test.data$Survived, predicted.classes)
my.ssa(table(test.data$Survived,predicted.classes))


#e)
#Overall accuracy rate
#Linear: 0.8033708
#Radial basis kernel: 0.8089888
#Polynomial kernel: 0.8146067
#Here polynomial kernel is the best.


#Part 2.

#1.

#data = read.csv("red_wine.csv", sep = ";")
data = read.csv("wine.csv")
str(data) #1599 obs
set.seed(123)
training.samples <- data$quality %>% createDataPartition(p = 0.75, list = FALSE)
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]
str(train.data) #1200 obs
str(test.data) #399 obs

#2
# a)
x <- model.matrix(quality~., train.data)[,-1]
y <- train.data$quality
cv <- cv.glmnet(x, y, alpha = 0)
cv$lambda.min

model <- glmnet(x, y, alpha = 0, lambda = cv$lambda.min) # alpha=0: ridge
coef(model)

x.test <- model.matrix(quality ~., test.data)[,-1]
predictions <- model %>% predict(x.test) %>% as.vector()
data.frame(
  RMSE = RMSE(predictions, test.data$quality),
  Rsquare = R2(predictions, test.data$quality)
)
ggplot(data = test.data, aes(x = quality, y = predictions)) + geom_point()

# b)
cv <- cv.glmnet(x, y, alpha = 1)
cv$lambda.min

model <- glmnet(x, y, alpha = 1, lambda = cv$lambda.min) # alpha=1: lasso
coef(model)

x.test <- model.matrix(quality ~., test.data)[,-1]
predictions <- model %>% predict(x.test) %>% as.vector()

data.frame(
  RMSE = RMSE(predictions, test.data$quality),
  Rsquare = R2(predictions, test.data$quality)
)
ggplot(data = test.data, aes(x = quality, y = predictions)) + geom_point()


# c)
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
ggplot(data = test.data, aes(x = quality, y = predictions)) + geom_point()


# d)
lambda <- 10^seq(-3, 3, length = 100)
set.seed(123)
ridge <- train(
  quality ~., data = train.data, method = "glmnet", trControl = trainControl("cv", number = 10), tuneGrid = expand.grid(alpha = 0, lambda = lambda)
  )
set.seed(123)
lasso <- train(
  quality ~., data = train.data, method = "glmnet", trControl = trainControl("cv", number = 10), tuneGrid = expand.grid(alpha = 1, lambda = lambda) )
set.seed(123)
elastic <- train(
  quality ~., data = train.data, method = "glmnet", trControl = trainControl("cv", number = 10), tuneLength = 10
)
models <- list(ridge = ridge, lasso = lasso, elastic = elastic)
resamples(models) %>% summary( metric = "RMSE")

# Elastic net is the best with the lowest RMSE if we comparing their medians
# Lasso is the best with the lowest RMSE if we comparing their means

#3
#a)
models <- regsubsets(quality~., data = train.data, nvmax = 5)
summary(models)

#b)
get_model_formula <- function(id, object, outcome){
  # get models data
  models <- summary(object)$which[id,-1]
  # Get outcome variable
  #form <- as.formula(object$call[[2]])
  #outcome <- all.vars(form)[1]
  # Get model predictors
  predictors <- names(which(models == TRUE))
  predictors <- paste(predictors, collapse = "+")
  # Build model formula
  as.formula(paste0(outcome, "~", predictors))
}

get_cv_error <- function(model.formula, data){
  set.seed(1)
  train.control <- trainControl(method = "cv", number = 5)
  cv <- train(model.formula, data = data, method = "lm",
              trControl = train.control)
  cv$results$RMSE
}

model.ids <- 1:5
cv.errors <-  map(model.ids, get_model_formula, models, "quality") %>%
  map(get_cv_error, data = train.data) %>%
  unlist()
cv.errors
which.min(cv.errors)
# The overall best model is the model with 5 variables:
# quality ~ volatile.acidity+residual.sugar+density+pH+alcohol

# c)
res.lm <- lm(quality ~., data = train.data)
step <- stepAIC(res.lm, direction = "both", trace = FALSE)
step

# d)
best_sub = lm(quality~volatile.acidity+residual.sugar+density+pH+alcohol, data = train.data) # from b)
models = list(ridge = ridge, lasso = lasso, elastic = elastic, best_sub = best_sub, step = step)
(compare = lapply(models %>% predict(test.data), RMSE, test.data$quality))
# The best model is step wise regression from stepAIC() with lowest RMSE.

#Part 3.

#1.

str(iris)
data = na.omit(iris)
data = scale(data[,-5])
pc = princomp(data, cor = T)
# Scree-plot
library(factoextra)
fviz_eig(pc)

# K means
k.means.fit <- kmeans(data, 3)
library(cluster)
clusplot(data, k.means.fit$cluster, main='2D representation of the Cluster solution',color=TRUE, shade=TRUE,labels=2, lines=0)
table(k.means.fit$cluster, iris$Species)
# confusion matrix:
# 50 0 0
# 0 39 14
# 0 11 36
# accuracy = (50+39+36)/150 = 125/150

# H.Ward
d <- dist(data, method = "euclidean")
H.fit <- hclust(d, method="ward.D")
plot(H.fit)
rect.hclust(H.fit, k=3, border="red")
groups <- cutree(H.fit, k=3)
clusplot(data, groups, main='2D representation of the Cluster solution',color=TRUE, shade=TRUE,labels=2, lines=0)
clusters = factor(groups, levels = 1:3, labels = c("setosa", "versicolor",  "virginica"))
table(iris[,5], clusters)
# accuracy = (49+50+27)/150 = 126/150

# H.Single
H.fit <- hclust(d, method="single")
plot(H.fit)
rect.hclust(H.fit, k=3, border="red")
groups <- cutree(H.fit, k=3)
clusplot(data, groups, main='2D representation of the Cluster solution',color=TRUE, shade=TRUE,labels=2, lines=0)
clusters = factor(groups, levels = 1:3, labels = c("setosa", "versicolor",  "virginica"))
table(iris[,5], clusters)
# accuracy = 99/150

# H.Complete
H.fit <- hclust(d, method="complete")
plot(H.fit)
rect.hclust(H.fit, k=3, border="red")
groups <- cutree(H.fit, k=3)
clusplot(data, groups, main='2D representation of the Cluster solution',color=TRUE, shade=TRUE,labels=2, lines=0)
clusters = factor(groups, levels = 1:3, labels = c("setosa", "versicolor",  "virginica"))
table(iris[,5], clusters)
# accuracy = 118/150

# H.Average
H.fit <- hclust(d, method="average")
plot(H.fit)
rect.hclust(H.fit, k=3, border="red")
groups <- cutree(H.fit, k=3)
clusplot(data, groups, main='2D representation of the Cluster solution',color=TRUE, shade=TRUE,labels=2, lines=0)
clusters = factor(groups, levels = 1:3, labels = c("setosa", "versicolor",  "virginica"))
table(iris[,5], clusters)
# accuracy = 103/150

# H.Centroid
H.fit <- hclust(d, method="centroid")
plot(H.fit)
rect.hclust(H.fit, k=3, border="red")
groups <- cutree(H.fit, k=3)
clusplot(data, groups, main='2D representation of the Cluster solution',color=TRUE, shade=TRUE,labels=2, lines=0)
clusters = factor(groups, levels = 1:3, labels = c("setosa", "versicolor",  "virginica"))
table(iris[,5], clusters)
# accuracy = 99/150

# (5) Ward's method is a little bit better than K-means

# (6) Comparison:
# Ward > K-means > Complete > Average > Single = Centroid
# I would recommend the cluster analysis as a good way to deal with this IRIS dataset.
# K-means and complete methods are usually great, but here Ward method is the best one from the confusion matrix.

#2.

library(devtools)
library(ggbiplot)
#(a)
iris.pca = prcomp(data, center = TRUE,scale. = TRUE)
summary(iris.pca)
#(b)
ggbiplot(iris.pca)
#(c)
ggbiplot(iris.pca, ellipse=TRUE, groups=iris$Species)
#(d)
iris.pca$rotation[,1] #PC1

#(e)

#I recommend the PCA method as an efficient way here.
#It can reduce the dimension of the data, and the first two PCs have a cumulative proportion of variance over 95%, which means this two PCs contains most of the information of the original variables.
#Additionally, The bi-plot created in part c would allows the biologist to see the groupings of species clearly.

