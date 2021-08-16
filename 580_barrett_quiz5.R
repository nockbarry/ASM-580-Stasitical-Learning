#Q1
library(MASS)

bwt <- with(birthwt, {
  race <- factor(race, labels = c("white", "black", "other"))
  ptd <- factor(ptl > 0)
  ftv <- factor(ftv)
  levels(ftv)[-(1:2)] <- "2+"
  data.frame(low = factor(low), age, lwt, race, smoke = (smoke > 0), ptd, ht = (ht > 0), ui = (ui > 0), ftv)
})

Data <- bwt
library(caTools)
set.seed(123)
split = sample.split(Data, SplitRatio = 0.8)
Train <- subset(Data, split == TRUE)
Test <- subset(Data, split == FALSE)
str(Train)

#Q2
library(rpart)
library(rattle)
model <- rpart(Train$low ~., data = Train, control = rpart.control(cp=0))

par(xpd = NA) # this value specifies where in the plotting device an object can actually be plotted
fancyRpartPlot(model)
pred <- predict(model,newdata = Test)
pred
table(Test$low,round(pred[,2]))

5/(9+5)#sens
25/(25+3)#spec
(25+5)/(9+3+25+5)#acc

#Q3
library(caret)
set.seed(123)
model2 <- train(low ~., data = Train, method = "rpart",  trControl = trainControl("cv", number = 10),  tuneLength = 100)
model2
plot(model2)
model2$bestTune
fancyRpartPlot(model2$finalModel)

#Q4
pred <- predict(model2, newdata = Test)
table(Test$low,pred)
#sensitivity
2/(12+2)
27/(27+1)#spec
(27+2)/(27+1+12+2)#acc
