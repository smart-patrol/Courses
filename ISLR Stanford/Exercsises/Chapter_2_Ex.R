install.packages("ISLR")
library("ISLR")

data(Auto)
attach(Auto)
cylinders = as.factor(cylinders)
plot(cylinders, mpg, col="red", varwidth=T)
plot(cylinders, mpg, col="red", varwidth=T, horizontal = T, 
     xlab = "clyinders", ylab = "MPG")

hist(mpg, col = 2, breaks = 15)

pairs(Auto)
pairs( ~mpg + displacement + horsepower + weight
       + acceleration, Auto)

# interaction method
plot(horsepower, mpg)
identify(horsepower, mpg, name)

summary(Auto)

detach(Auto)

#----------------------------------------------
# 2.4 Exercises

# 1 = b

# 2 
# a) is regression and would be more intreested in p
# b) is classifiaction and would be prediction
# c) is regression and would be prediction

# 3
# bias will decrease as flexibility increases
# variance will increase as fleixbility increases
# Gains in flexibility will decrease training error
# test error will be concave up

#5
# a very flexiable approch will yeild a predictive model
# in which inference/description will be hard to do.
# Conversely, low feliability will generate a model that is 
# simpler and easy to interpret
# The trade off again is in the stregths - either having
# a model that will be highly predictive or one that is easy
# to explain

#6
# A parametric apporch assumes assumes a form of F and thus
# simplifies estimation.
# A non-para approch does not assume a functional form 
# and thus requires a large number of obeservations
# The advantage of a parametric form is that it requires a low
# number of n. If p is high - this becomes a disadvantage and 
# overfitting will occour

#7
x1 = c(0,2,0,0,-1,1)
x2 = c(3,0,1,1,0,1)
x3 = c(0,0,3,2,1,1)
y = c("R","R","R","G","G","R")

train = as.matrix(cbind(x1,x2,x3))

dist(train)
?dist

#8
library(ISLR)
data(College)
df = College
head(df,2)

df$school <- rownames(df)
rownames(df) <- NULL

summary(df)
names(df)
?College
str(df)

pairs(df[,1:10])

with(df, boxplot(Private, Outstate))

summary(df$Top10perc)
df$Elite <- factor(ifelse(df$Top10perc < 10, 1, 0))
head(table(df$Top10perc, df$Elite),12)

prop.table(table(df$Elite))*100
with(df, boxplot(Elite, Outstate))

library(psych)
excl <- c("Private","school","Elite", "Top10perc", "Top25perc", "Outstate")
pairs.panels(df[ , !names(df)  %in% excl ])

hist(df$PhD, col="red", breaks=20, xlab = "PhDs")

excl <- c("school")
library(corrplot)
rho <- cor(df[ , !names(df) %in% excl])
corrplot(rho)
 # strong positive relationship between apps, accpetance, enrollment and F ungergrands

# 9

#10 
library("MASS")
data(Boston)
df = Boston
?Boston
str(Boston)

dim(df)
# each row is a town
splom(df)
library(lattice)
splom(df)

cor(df)[,1]
# strongest is access to radial highways followd by property tax 

qqnorm(df$crim)
qqnorm(df$tax)
qqnorm(df$ptratio)
# yes

table(df$chas)
# 35

median(df$ptratio)

min(df$medv)
df[5, ]
colMeans(df)
# it's differnet  - it looks rural

table(df$rm>7)
table(df$rm>8)
df[ df$rm>8,]
# older buildings, not by the river, black, and high home values



