#Quiz 3 AMS 580 Nicholas Barrett

#Q1
library(tidyverse)
library(car)
data = mtcars
attach(mtcars)
cyl = factor(cyl)
vs = factor(vs)
am = factor(am)
gear = factor(gear)
carb = factor(carb)

#a
fit = lm(mpg~am)
fit
summary(fit)
plot(am,mpg)
am
#level 0 is baseline,
#the model is mpg ~ 17.147 + 7.245*am1
#am is a significant predictor of mpg with a p value of less than .001
 
#b
amswap = relevel(am,ref='1')
amswap
fit2 = lm(mpg~amswap)
fit2
summary(fit2)
plot(amswap,mpg) 


#c
gear
fitc = lm(mpg ~ hp + wt + am + gear)
Anova(fitc)
#
summary(fitc)

#hp and wt are significant at .01

#d
#R2 = .84 which indicates a good fit

#e
#the normality of hp and wt are important assumptions
qqPlot(hp)
shapiro.test(hp)
qqPlot(wt)
shapiro.test(wt)


#Q2
install.packages("tidyverse")
install.packages("caret")
install.packages("leaps")
install.packages("MASS")
library(tidyverse)
library(caret)
library(leaps)
library(MASS)
#a
models <- regsubsets(mpg~., data = mtcars, nvmax = 5)
summary(models)

#b
set.seed(123) 
train.control <- trainControl(method = "cv", number = 5)
model1 <- train(mpg~wt, data = mtcars, method = "lm", trControl = train.control)
model2 <- train(mpg~wt+cyl, data = mtcars, method = "lm", trControl = train.control)
model3 <- train(mpg~wt+qsec+am, data = mtcars, method = "lm", trControl = train.control)
model4 <- train(mpg~hp+wt+qsec+am, data = mtcars, method = "lm", trControl = train.control)
model5 <- train(mpg~disp+hp+wt+qsec+am, data = mtcars, method = "lm", trControl = train.control)
modelfull <- train(mpg~., data = mtcars, method = "lm", trControl = train.control)
print(modelfull)
# Summarize the results
print(model1)
print(model2)
print(model3)
print(model4)
print(model5)
#Based on this result it seems model 5 mpg~disp+hp+wt+qsec+am has the highest Rsquared and lowest error

#c
library(MASS)
full.model = lm(mpg~.,data=mtcars)
step.model = stepAIC(full.model,direction = "both",trace = FALSE)
summary(step.model)
#best model for this method is mpg ~ wt + qsec + am


