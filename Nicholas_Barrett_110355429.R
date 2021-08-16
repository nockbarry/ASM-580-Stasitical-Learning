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

data <- read.csv('GreatUnknown.csv')
summary(data)

set.seed(123)
training.samples <- data$y %>% createDataPartition(p = 0.75, list = FALSE)
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]
str(test.data)
str(train.data)


#2
#a)
set.seed(123)
model = neuralnet(y~., data = train.data, hidden = 0, err.fct = "sse", linear.output = F)
plot(model, rep = "best")

probabilities = predict(model, test.data)
predicted.classes = ifelse(probabilities > 0.5, 1, 0)

confusionMatrix(factor(predicted.classes), factor(test.data$y), positive = "1")

#b)
set.seed(123)
model = neuralnet(y~., data = train.data, hidden = 0, err.fct = "ce", linear.output = F)
plot(model, rep = "best")

probabilities = predict(model, test.data)
predicted.classes = ifelse(probabilities > 0.5, 1, 0)

confusionMatrix(factor(predicted.classes), factor(test.data$y), positive = "1")

#c)
set.seed(123)
model = glm(y~., family = binomial, data = train.data)
model
probabilities = model %>% predict(test.data, type = "response")
predicted.classes = ifelse(probabilities > 0.5, 1, 0)

confusionMatrix(factor(predicted.classes), factor(test.data$y), positive = "1")
#very similar coefficients to the cross entropy based model in b)

#d)
set.seed(123)
model = neuralnet(y~., data = train.data, hidden = 3, err.fct = "sse", linear.output = F)
plot(model, rep = "best")

probabilities = predict(model, test.data)
predicted.classes = ifelse(probabilities > 0.5, 1, 0)
pred.nn = predicted.classes

confusionMatrix(factor(predicted.classes), factor(test.data$y), positive = "1")
#higher accuracy than all previous methods including 2.a)

#e)
set.seed(123)
model = neuralnet(y~., data = train.data, hidden = 3, err.fct = "ce", linear.output = F)
plot(model, rep = "best")

probabilities = predict(model, test.data)
predicted.classes = ifelse(probabilities > 0.5, 1, 0)

confusionMatrix(factor(predicted.classes), factor(test.data$y), positive = "1")
#better than the 2b model in accuracy

#f)
#d and e seem to be tied for "best" model, ill be using d in part 6


#3
# a)
train.data$y <- as.factor(train.data$y)
test.data$y <- as.factor(test.data$y)
set.seed(123)
model <- train(
  y ~., data = train.data, method = "rf",
  trControl = trainControl("cv", number = 10),
  importance = TRUE
  )
# Best tuning parameter
model$bestTune
model$finalModel

my.ssa(model$finalModel$confusion)



#b
pred <- model %>% predict(test.data)
pred.rf = pred
table(test.data$y,pred)
my.ssa(table(test.data$y,pred))


#c)
# Plot MeanDecreaseAccuracy
varImpPlot(model$finalModel, type = 1)
# Plot MeanDecreaseGini
varImpPlot(model$finalModel, type = 2)
#d)
varImp(model)


#4

#a)
model <- rpart(y ~., data = train.data, control = rpart.control(cp=0))
par(xpd = NA)
fancyRpartPlot(model)
pred <- predict(model,newdata = test.data, type ='class')
table(test.data$y,pred)
my.ssa(table(test.data$y,pred))

# b)
set.seed(123)
model2 <- train(
  y ~., data = train.data, method = "rpart",
  trControl = trainControl("cv", number = 10),
  tuneLength = 100)
model2
plot(model2)

model2$bestTune
fancyRpartPlot(model2$finalModel)

# c)
pred <- predict(model2, newdata = test.data)
table(test.data$y,pred)
my.ssa(table(test.data$y,pred))


#5.

#a)
# Fit the model on the training set
set.seed(123)
model <- train(
  y ~., data = train.data, method = "svmLinear",
  trControl = trainControl("cv", number = 10)
  )

# Make predictions on the test data
predicted.classes <- model %>% predict(test.data)
head(predicted.classes)

# Confusion matrix
table(test.data$y, predicted.classes)
my.ssa(table(test.data$y,predicted.classes))


# b)
# Fit the model on the training set
set.seed(123)
model <- train(
  y ~., data = train.data, method = "svmLinear",
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
table(test.data$y, predicted.classes)
my.ssa(table(test.data$y,predicted.classes))


#c)
# Fit the model on the training set
set.seed(123)
model <- train(
  y ~., data = train.data, method = "svmRadial",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale"),
  tuneLength = 10
  )
# Print the best tuning parameter sigma and C that
# maximizes model accuracy
model$bestTune

# Make predictions on the test data
predicted.classes <- model %>% predict(test.data)
pred.svm = predicted.classes

# Confusion matrix
table(test.data$y, predicted.classes)
my.ssa(table(test.data$y,predicted.classes))


# d)
# here radial is the best based on accuracy

#6)
levels(pred.svm) = c(0,1)
levels(pred.rf) = c(0,1)
pred.compare = cbind(as.numeric(pred.nn),as.numeric(pred.rf)-1,as.numeric(pred.svm)-1)
head(pred.compare)
pred.av = rep(0,length(pred.nn))
for(i in 1:length(pred.nn)){pred.av[i] = mean(pred.compare[i,])}
pred.av = round(pred.av)

table(test.data$y, pred.av)
my.ssa(table(test.data$y,pred.av))
#Does really well

#Part 2.
my.ssa = function(cm){
  sens = cm[4]/(cm[2]+cm[4])
  specif = cm[1]/(cm[1]+cm[3])
  accu = (cm[1]+cm[4])/(cm[2]+cm[3]+cm[1]+cm[4])
  x = c(sens = sens, specif = specif, accu = accu)
  x
}
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

#1.


data = read.csv("QuestionMark.csv")
str(data)
set.seed(123)
training.samples <- data$y %>% createDataPartition(p = 0.75, list = FALSE)
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]
str(train.data)
str(test.data)

#2
# a)
x <- model.matrix(y~., train.data)[,-1]
y <- train.data$y

cv <- cv.glmnet(x, y, alpha = 0)
cv$lambda.min

model <- glmnet(x, y, alpha = 0, lambda = cv$lambda.min) # alpha=0: ridge
coef(model)

x.test <- model.matrix(y ~., test.data)[,-1]
predictions <- model %>% predict(x.test) %>% as.vector()
data.frame(
  RMSE = RMSE(predictions, test.data$y),
  Rsquare = R2(predictions, test.data$y)
)
ggplot(data = test.data, aes(x = y, y = predictions)) + geom_point()

# b)
cv <- cv.glmnet(x, y, alpha = 1)
cv$lambda.min

model <- glmnet(x, y, alpha = 1, lambda = cv$lambda.min) # alpha=1: lasso
coef(model)

x.test <- model.matrix(y ~., test.data)[,-1]
predictions <- model %>% predict(x.test) %>% as.vector()

data.frame(
  RMSE = RMSE(predictions, test.data$y),
  Rsquare = R2(predictions, test.data$y)
)
ggplot(data = test.data, aes(x = y, y = predictions)) + geom_point()


# c)
#w4 wasnt working here and i dont know why i kept getting
#Error: variable 'w4' was fitted with type "factor" but type "numeric" was supplied
#In addition: Warning message:
#  In model.frame.default(Terms, newdata, na.action = na.action, xlev = object$xlevels) :
#  variable 'w4' is not a factor

#so i am just removing them since they werent in the final model anyway
data = read.csv("QuestionMark.csv")
data = subset(data,select = -c(w4))
set.seed(123)
training.samples <- data$y %>% createDataPartition(p = 0.75, list = FALSE)
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]


model <- train(
  y ~., data = train.data, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
model$bestTune

coef(model$finalModel, model$bestTune$lambda)

x.test <- model.matrix(y ~., test.data)[,-1]
#colnames(x.test)[4]="w4"
#x.test[,4]=factor(x.test[,4])
predictions <- model %>% predict(x.test) %>% as.vector()
data.frame(
  RMSE = RMSE(predictions, test.data$y),
  Rsquare = R2(predictions, test.data$y)
)
ggplot(data = test.data, aes(x = y, y = predictions)) + geom_point()

#return to normal data
data = read.csv("QuestionMark.csv")
set.seed(123)
training.samples <- data$y %>% createDataPartition(p = 0.75, list = FALSE)
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]

# d)
lambda <- 10^seq(-3, 3, length = 100)
set.seed(123)
ridge <- train(
  y ~., data = train.data, method = "glmnet", trControl = trainControl("cv", number = 10), tuneGrid = expand.grid(alpha = 0, lambda = lambda)
)
set.seed(123)
lasso <- train(
  y ~., data = train.data, method = "glmnet", trControl = trainControl("cv", number = 10), tuneGrid = expand.grid(alpha = 1, lambda = lambda) )
set.seed(123)
elastic <- train(
  y ~., data = train.data, method = "glmnet", trControl = trainControl("cv", number = 10), tuneLength = 10
)
models <- list(ridge = ridge, lasso = lasso, elastic = elastic)
resamples(models) %>% summary( metric = "RMSE")
resamples(models) %>% summary( metric = "Rsquare")

#Rsquared is fairly similar, RMSE shows lasso and elastic net tied for lowest making them equivalent in this case

#3
#a)
models <- regsubsets(y~., data = train.data, nvmax = 6)
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

model.ids <- 1:6
cv.errors <-  map(model.ids, get_model_formula, models, "y") %>%
  map(get_cv_error, data = train.data) %>%
  unlist()
cv.errors
which.min(cv.errors)
# The overall best model is the model with 6 variables
# y ~ w1 + w5 + w6 + w9 + w10 + w13

# c)
res.lm <- lm(y ~., data = train.data)
step <- stepAIC(res.lm, direction = "both", trace = FALSE)
step

# d)
best_sub = lm(y~ w1 + w5 + w6 + w9 + w10 + w13, data = train.data) # from b)
models = list(ridge = ridge, lasso = lasso, elastic = elastic, best_sub = best_sub, step = step)
(compare = lapply(models %>% predict(test.data), RMSE, test.data$y))
# the best model based on RMSE is ridge coming in at the lowest of the models tried at  909

#Part 3.

#1.

data <- read.csv('GreatUnknown.csv')
summary(data)

pc = princomp(data, cor = T)
# Scree-plot
library(factoextra)
fviz_eig(pc)

# K means
k.means.fit <- kmeans(data, 2)
library(cluster)
clusplot(data, k.means.fit$cluster, main='2D representation of the Cluster solution',color=TRUE, shade=TRUE,labels=2, lines=0)
table(data$y,k.means.fit$cluster)
my.ssa(table(k.means.fit$cluster, data$y))

# H.Ward
d <- dist(data, method = "euclidean")
H.fit <- hclust(d, method="ward.D")
plot(H.fit)
rect.hclust(H.fit, k=2, border="red")
groups <- cutree(H.fit, k=2)
clusplot(data, groups, main='2D representation of the Cluster solution',color=TRUE, shade=TRUE,labels=2, lines=0)
clusters = factor(groups, levels = 1:2)
table(clusters,data$y)
my.ssa(table(clusters,data$y))


# H.Single
H.fit <- hclust(d, method="single")
plot(H.fit)
rect.hclust(H.fit, k=2, border="red")
groups <- cutree(H.fit, k=2)
clusplot(data, groups, main='2D representation of the Cluster solution',color=TRUE, shade=TRUE,labels=2, lines=0)
clusters = factor(groups, levels = 1:2)
table(data$y,clusters)
my.ssa(table(data$y,clusters))

# H.Complete
H.fit <- hclust(d, method="complete")
plot(H.fit)
rect.hclust(H.fit, k=2, border="red")
groups <- cutree(H.fit, k=2)
clusplot(data, groups, main='2D representation of the Cluster solution',color=TRUE, shade=TRUE,labels=2, lines=0)
clusters = factor(groups, levels = 1:2)
table(data$y, clusters)
my.ssa(table(data$y,clusters))

# H.Average
H.fit <- hclust(d, method="average")
plot(H.fit)
rect.hclust(H.fit, k=2, border="red")
groups <- cutree(H.fit, k=2)
clusplot(data, groups, main='2D representation of the Cluster solution',color=TRUE, shade=TRUE,labels=2, lines=0)
clusters = factor(groups, levels = 1:2)
table(data$y, clusters)
my.ssa(table(data$y,clusters))

# H.Centroid
H.fit <- hclust(d, method="centroid")
plot(H.fit)
rect.hclust(H.fit, k=2, border="red")
groups <- cutree(H.fit, k=2)
clusplot(data, groups, main='2D representation of the Cluster solution',color=TRUE, shade=TRUE,labels=2, lines=0)
clusters = factor(groups, levels = 1:2)
table(data$y, clusters)
my.ssa(table(data$y,clusters))

#by overall accuracy these all suck, with all of thme coming around .6 accuracy, sin some cases like centroid
# the specificty was high at the cost of 0 sensitivity, so i dont feel comfortable ranking them when they all suck
#for this classification challenge

#2.

library(devtools)
library(ggbiplot)
#(a)
unknown.pca = prcomp(data, center = TRUE,scale. = TRUE)
summary(unknown.pca)
#(b)
ggbiplot(unknown.pca)
#(c)
ggbiplot(unknown.pca, ellipse=TRUE, groups=data$y)
#(d)
unknown.pca$rotation[,1] #PC1

# PC1 seems pretty dependent on all other variables

