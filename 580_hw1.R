x <- c(1,3,2,1,3)
y <- c(14,24,18,17,27)
model <- lm(y ~ x)
plot(x,y)

model
abline(lm(y ~ x))
summary(model)
cor.test(x,y)

s <- c(9,12,14,11,13,10,6,9,6,10,12,14,11,13,11,9,8,11,7,8)
mat <- matrix(data = s, nrow = 5)
data <- data.frame(data = mat)
boxplot(mat,use.cols = TRUE)

aov()

a = c(9,12,14,11,13)
b = c(10,6,9,9,10)
c = c(12,14,11,13,11)
d = c(9,8,11,7,8)

dati = c(a,b,c,d)
groups = factor(rep(letters[1:4], each = 5))
fit = lm(formula = dati ~ groups)
anova(fit)


t.test(c,d,var.equal = TRUE)
