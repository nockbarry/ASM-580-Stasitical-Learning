library(carData)
str(Salaries)

library(tidyverse)
library(caret)
library(leaps) 
library(MASS)

Salaries_dummy <- data.frame(Salaries[ , ! colnames(Salaries) %in% "rank"], model.matrix( ~ -1 + rank, Salaries))
Salaries_dummy <- data.frame(Salaries_dummy[ , ! colnames(Salaries_dummy) %in% "discipline"], model.matrix( ~ discipline - 1, Salaries_dummy))
Salaries_dummy <- data.frame(Salaries_dummy[ , ! colnames(Salaries_dummy) %in% "sex"], model.matrix( ~ sex - 1, Salaries_dummy))
Salaries_dummy = Salaries_dummy[,-c(4,7,9)]
str(Salaries_dummy)

models <- regsubsets(salary~., data = Salaries_dummy, nvmax = 6) 
summary(models)

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
  cv <- train(model.formula, data = data, method = "lm",trControl = train.control)  
  cv$results$RMSE
}

model.ids <- 1:6
cv.errors <-  map(model.ids, get_model_formula, models, "salary") %>%  
  map(get_cv_error, data = Salaries_dummy) %>%  
  unlist()
cv.errors

which.min(cv.errors)
coef(models, 6)

#b
res.lm <- lm(salary ~., data = Salaries_dummy)
step <- stepAIC(res.lm, direction = "both", trace = FALSE) 
step

Salaries$rankAsstProf[Salaries$rank == "AsstProf"] = 1
Salaries$rankAsstProf[Salaries$rank != "AsstProf"] = 0
Salaries$rankAssocProf[Salaries$rank == "AssocProf"] = 1
Salaries$rankAssocProf[Salaries$rank != "AssocProf"] = 0
Salaries$sexMale[Salaries$sex == "Male"] = 1
Salaries$sexMale[Salaries$sex != "Male"] = 0
Salaries$disciplineB[Salaries$discipline == "B"] = 1
Salaries$disciplineB[Salaries$discipline != "B"] = 0
Salaries$rank = NULL
Salaries$sex = NULL
Salaries$discipline = NULL
str(Salaries)

#2
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
d <- dist(data, method = "euclidean")
H.fit <- hclust(d, method="ward.D")
plot(H.fit)
rect.hclust(H.fit, k=3, border="red")

groups <- cutree(H.fit, k=3)
clusplot(data, groups, main='2D representation of the Cluster solution',color=TRUE, shade=TRUE,labels=2, lines=0)
clusters = factor(groups, levels = 1:3, labels = c("setosa", "versicolor",  "virginica"))
table(iris[,5], clusters)

#3a
library(caret)
dataurl <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"
download.file(url = dataurl, destfile = "wine.data")
wine_df <- read.csv("wine.data", header = FALSE)
wine_df$V1 = factor(wine_df$V1)
str(wine_df)

anyNA(wine_df)
set.seed(1)
intrain <- createDataPartition(y = wine_df$V1, p= 0.7, list = FALSE)
training <- wine_df[intrain,]
testing <- wine_df[-intrain,]
#b
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(1)
knn_fit <- train(V1 ~., data = training, method = "knn", trControl=trctrl, preProcess= c("center", "scale"), tuneLength = 10)
knn_fit

plot(knn_fit$results$k, knn_fit$results$Accuracy, xlab = "#Neighbors", ylab = "Accuracy")

#c
test_pred <- predict(knn_fit, newdata = testing)
test_pred
confusionMatrix(test_pred, testing$V1)
