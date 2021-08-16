#AMS580 Quiz 4
#By Nicholas Barrett
library(tidyverse)
library(caret)
library(MASS)

#Q1
data("birthwt", package = "MASS")
#this factoring method was taken from the reference in the homework
bwt <- with(birthwt, {
  race <- factor(race, labels = c("white", "black", "other"))
  ptd <- factor(ptl > 0)
  ftv <- factor(ftv)
  levels(ftv)[-(1:2)] <- "2+"
  data.frame(low = factor(low), age, lwt, race, smoke = (smoke > 0), ptd, ht = (ht > 0), ui = (ui > 0), ftv)
})
options(contrasts = c("contr.treatment", "contr.poly"))

#Split Data
set.seed(123)
training.samples <- bwt$low %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- bwt[training.samples, ]
test.data <- bwt[-training.samples, ]


#Testing function
test <- function(model,test.data){
  probabilities <- model %>% predict(test.data, type = "response")
  predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
  count.acc = 0
  count.tp = 0
  count.fp = 0
  count.fn = 0
  len = length(test.data[ ,1])
  for(i in 1:len){
    if(predicted.classes[i]==test.data$low[i]){ #true positive and true negative
      if(predicted.classes[i]==1){count.tp = count.tp +1}#TP
      count.acc = count.acc + 1
    }
    if(test.data$low[i]==0 && predicted.classes[i]==1){
      count.fp = count.fp +1 #false positive
    }
    if(test.data$low[i]==1 && predicted.classes[i]==0){
      count.fn = count.fn +1 #false negative
    }
  }
  sen = count.tp/(count.tp+count.fn)
  count.tn =(count.acc-count.tp) #TN
  spe = (count.tn)/(count.tn + count.fp)
  acc = count.acc/len
  out = list(Acc = acc, Specif = spe, Sensit = sen)
  conf.mat = matrix(data = c(count.tn,count.fp,count.fn,count.tp),byrow = TRUE,nrow=2)
  print(conf.mat)
  return(out)
}

#Full Model
model <- glm(train.data$low ~., data = train.data, family = binomial)
summary(model)
test(model,test.data)

#Q2.1
step <- stepAIC(model)
step$anova
test(step,test.data)
BIC <- stepAIC(model,k=log(nrow(bwt)))
BIC$anova
test(BIC,test.data)

#Q2.2
library(leaps)
library(bestglm)
library(dummies)
library(tidyverse)

bwt.move<-bwt[,-1]
bwt.move$low<-bwt$low

race = data.frame(dummy(bwt$race)[,c(1,2)])
ftv = data.frame(dummy(bwt$ftv)[,c(2,3)])
bwt.dummy = bwt[,-c(1,4,9)]
low = bwt$low
bwt.dummy = cbind(bwt.dummy,race,ftv, low)

BIC.sub = bestglm(bwt.dummy,IC="BIC",family=binomial)

test(BIC.sub$BestModel,test.data)
models <- regsubsets(train.data~., data=train.data, nvmax = 5)
#The subset variable BIC seems to be the best, with the stepwise BIC just behind it
