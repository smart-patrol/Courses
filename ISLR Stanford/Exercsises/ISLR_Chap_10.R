
#----------------------------------------------------
# PCA
dimnames(USArrests)

apply(USArrests, 2, mean)
apply(USArrests, 2, var)

# assualt will dominate PCA b/c of high variance

a_pca <- prcomp(USArrests, scale=TRUE)
a_pca
names(a_pca)
biplot(a_pca, scale=0)

#----------------------------------------------------
# k means example
set.seed(101)
x = matrix(rnorm(100*2), 100, 2)
xmean <- matrix(rnorm(8, sd=4),4,2)
which = sample(1:4, 100, replace=TRUE)
x <- x+xmean[which,]
plot(x, col = which, pch=19)

km_out <- kmeans(x,4,nstart=15)
km_out

plot(x, col=km_out$cluster, cex=2, pch=1, lwd=2)
points(x, col=which, pch=19)
# reassign the colors
points(x, col = c(4,3,2,1)[which], pch=19)

#----------------------------------------------------
#hiearchical Clustering
hc_comp <- hclust(dist(x), method = "complete")
plot(hc_comp)

hs_sng <- hclust(dist(x), method = "single")
plot(hs_sng)

hs_avg <- hclust(dist(x), method = "average")
plot(hs_avg)

#cut tree to 4 and compare vs kmeans vs actual
hc_cut <- cutree(hc_comp,4)
table(hc_cut, which)
table(hc_cut, km_out$cluster)

plot(hc_comp, labels = which) # actual vals

#----------------------------------------------------
#courseware questions

load("C:\\Users\\Home\\Downloads\\10.R.RData")

df <- rbind(x, x.test)

my_pca <- prcomp(df, scale=TRUE)

summary(my_pca) #0.3499




# fit on the normal data
xy <- data.frame(y, x)

fit <- lm(y ~. , data = xy)

pred <- predict(fit,  newdata=x.test)

mean((y.test - pred)^2)



