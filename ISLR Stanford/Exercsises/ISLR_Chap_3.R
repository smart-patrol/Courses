load_lib <- function(){
library(MASS)
library(ISLR)
library(car)
print("Libraries Loaded")
}

load_lib()

#-----------------------------------------------------------------
# Simple lm
names(Boston)
?Boston
with(Boston, plot(medv ~ lstat))
fit1 = lm(medv ~ lstat, data=Boston)
summary(fit1)

with(Boston, plot(medv ~ lstat))
abline(fit1, col="red")

confint(fit1)
predict(fit1, data.frame(lstat=c(5,10,15)), interval = "confidence")

#-----------------------------------------------------------------
#multiple lm
fit2 = lm(medv ~ lstat + age, data=Boston)
summary(fit2)
fit3 = lm(medv~., Boston)
summary(fit3)

residuals(fit3) ; rstudent(fit3)
plot(predict(fit3), residuals(fit3))
plot(predict(fit3), rstudent(fit3))
# leverage stast
?hatvalues()
plot(hatvalues(fit3))
which.max(hatvalues(fit3)) #max

# multicollineartiy
vif(fit3) > 5

par(mfrow=c(2,2))
plot(fit3)
fit4 <- update(fit3, ~.-age-indus)
summary(fit4)$coef

#-----------------------------------------------------------------
# nonlinear terms and interactions
fit5 <- lm(medv ~ lstat*age, Boston)
summary(fit5)
fit6 <- lm(medv ~ lstat + I(lstat^2), Boston)
summary(fit6)

par(mfrow=c(1,1))
with(Boston, plot(medv ~ lstat))
points(Boston$lstat, fitted(fit6), col="red", pch=20)

fit7 = lm(medv ~ poly(lstat,4), Boston)
points(Boston$lstat, fitted(fit7), col="blue", pch=20)

anova(fit1, fit2, fit3, fit4, fit5, fit6, fit7)

#-----------------------------------------------------------------
# nonlinear terms and interactions
data(Carseats)
View(Carseats)
summary(Carseats)
names(Carseats)
str(Carseats)

names(Carseats) <- tolower(names(Carseats))
fit <- lm(sales ~. + income:advertising + age:price, Carseats)
summary(fit)$coef
contrasts(Carseats$shelveloc)
contrasts(Carseats$urban)

# witing an R function
reg_plot <- function(x,y){
  fit = lm(y ~ x)
  plot(x,y)
  abline(fit, col="red")
}

with(Carseats, reg_plot(price, sales))


reg_plot <- function(x,y, ...){  #adding extra args
  fit = lm(y ~ x)
  plot(x,y, ...) #here
  abline(fit, col="red")
}

with(Carseats, reg_plot(price, sales, xlab="price", ylab="sales"))
with(Carseats, reg_plot(advertising, sales, xlab="price", ylab="advertising"))

#-------------------------------------------------------------------
# Applied Exercises Start

data(Auto)
summary(Auto)
str(Auto)

mod <- lm(mpg ~ horsepower, data = Auto)
summary(mod)
# horse power effects mpg negatively with correlation at 0.6 Rsq 

predict(mod  ,data.frame(horsepower=c(98)), interval = "confidence")

with(Auto, plot(horsepower, mpg))
abline(fit, col="red")

par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))

# 9
plot(Auto)
cor(Auto[ ,names(Auto) != "name" ])
mod <- lm(mpg ~., data = Auto[ ,names(Auto) != "name" ])
summary(mod)
vif(mod)
plot(mod)

# interaction
mod <- lm(mpg ~ origin * acceleration, data = Auto)
summary(mod)

mod <- lm(mpg ~ origin*acceleration*year, data = Auto)
summary(mod)

# transform 
qqnorm(Auto$mpg)
qqnorm(Auto$weight)

mod1 <- lm(mpg ~  log(weight), data = Auto)
summary(mod1)
mod2 <- lm(mpg ~  poly(weight,2), data = Auto)
summary(mod2)
mod3 <- lm(mpg ~  sqrt(weight), data = Auto)
summary(mod3)

anova(mod1, mod2, mod3)

with(Auto, plot(weight, mpg))
points(Auto$weight, fitted(mod1), col="red", pch=20)
points(Auto$weight, fitted(mod2), col="blue", pch=20)
points(Auto$weight, fitted(mod3), col="green", pch=20)

rm(mod1, mod2, mod3)

# 10 
str(Carseats)

mod <- lm(sales ~ price + urban + us, data=Carseats)
summary(mod)
vif(mod)

mod <- lm(sales ~ price + us , Carseats)
summary(mod)
confint(mod)
par(mfrow=c(2,2))
plot(mod)
par(mfrow=c(1,1))

# 11 


