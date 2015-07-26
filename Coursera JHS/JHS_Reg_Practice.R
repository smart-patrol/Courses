
### Linear Regression

#install.packages("ISLR")
library(ISLR)
library(MASS)

data(Boston) ; attach(Boston)

view(Boston)
names(Boston)
?Boston

fit = lm(medv ~ lstat)

summary(fit)
coef(fit)
confint(fit)

predict(fit, data.frame(lstat=(c(5,10,15))), interval="confidence")

predict(fit, data.frame(lstat=(c(5,10,15))), interval="prediction")

plot(lstat, medv,  pch=3) ; abline(fit , lwd=3 , col="red")

# compute residual plots to look at nonlinearity and proper fit
plot(predict(fit) , residuals(fit))
plot(predict(fit) , rstudent(fit))

#compute leverage to look at values that have undue influence
plot(hatvalues(fit))
which.max(hatvalues(fit)) # largest leveraage stat

### Multiple Linear Regression

fit = lm(medv ~ lstat + age)
summary(fit)

# include all
fit = lm(medv ~.  , data=Boston)
summary(fit)

summary(fit)$r.sq
summary(fit)$sigma # error

library(car)
vif(fit)

# remove age
fit1 = update(fit , ~.-age)
summary(fit1)

#interaction terms
summary(lm(medv ~ lstat * age , data=Boston))

?I()
# nolinear transformation of predictor
fit2 = lm(medv ~ lstat+I(lstat^2))
summary(fit2)

anova(fit, fit2) # second models with squared is superior

plot(fit2)

#fifth order polynomial fit
fit3 = lm(medv ~ poly(lstat, 5))
summary(fit3)

#log stat fit
summary(lm(medv ~ log (rm) , data=Boston))

### qualitative perdictors

data(Carseats) ; attach(Carseats)
names(Carseats)

fit = lm(Sales ~. + Income : Advertising + Price:Age , data=Carseats)
summary(fit)

# look at coding
contrasts(ShelveLoc)

LoadLib = function() {
  library(ISLR)
  library(MASS)
  print("Libraries Loaded")
}

LoadLib()

data(Advertising)

#######################################
#students final example
data(mtcars)
head(mtcars,2)

print(cor(mtcars[,-1]))
eta <- abs(cor(mtcars[,-1]))
eta

mod = lm(mpg ~ am, data=mtcars)
rstandard(mod) ; plot(rstandard(mod))
resid(mod) / (1-hatvalues(mod))
     
plot(mtcars$mpg ~ mtcars$am, col=mtcars$hp, pch=19, main="Fuel Consumption over Transmission",
     xlab="Transmission", ylab="Miles / (US) gallon")
abline(mod, lwd=3, col="red")

par(mar = rep(1, 2))


