library('ISLR') # набор данных Hitters
library('leaps') # функция regsubset() — отбор оптимального 
# подмножества переменных
library('glmnet') # функция glmnet() — лассо
library('pls') # регрессия на главные компоненты — pcr()
# и частный МНК — plsr()

my.seed <- 1
attach(Auto)
str(Auto)
Auto <- Auto[,-9]

train.percent <- 0.85 #доля обучающей выборки
set.seed(my.seed)
inTrain <- sample(seq_along(Auto$mpg),#формируем обучающую выборку
                  nrow(Auto)*train.percent)
fact <- Auto[inTrain, ]
test <- Auto[-inTrain, ]
train <- sample(c(T, F), nrow(Auto), rep = T)

fit1<-lm(mpg~. -cylinders -acceleration -displacement -horsepower, data = fact) #регрессия на обучающей выборке
summary(fit1)
y.model.lm <- predict(fit1, test)
MSE.fit1 <- sum((y.model.lm - test$mpg)^2) / length(fit1)
MSE.fit1

apply(Auto, 2, mean)
apply(Auto, 2, var)

pr.out=prcomp(fact, scale=TRUE) #8 главных компонент
pr.out$center
pr.out$scale
pr.out$rotation

dim(pr.out$x)
biplot(pr.out, scale=0)
pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out, scale=0)
pr.out$sdev
pr.var=pr.out$sdev^2
pr.var
pve=pr.var/sum(pr.var)
pve
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1),type='b')
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')
# 2 главные компоненты, так как суммарна дисперсия превышает 75 процентов.

pcr.fit <- pcr(mpg ~ ., data = fact, scale = T, validation = 'CV')
summary(pcr.fit)
# график ошибок
validationplot(pcr.fit, val.type = 'MSEP')
set.seed(my.seed)
pcr.fit <- pcr(mpg ~ ., data = test, subset = train, scale = T,
               validation = 'CV')
validationplot(pcr.fit, val.type = 'MSEP')
pcr.pred <- predict(pcr.fit, test, ncomp = 2)
round(mean((pcr.pred - test$mpg)^2), 0)

set.seed(1)
x=Auto
km.out=kmeans(x,2,nstart=20)

km.out=kmeans(x,3,nstart=20)
y<-km.out$cluster
km.out=kmeans(x,2,nstart=20)
km.out$tot.withinss
km.out=kmeans(x,3,nstart=20)
km.out$tot.withinss
#Выборку можно разбить на 2 или 3 кластера, каждый кластер характеризуется значением mpg, 
#так как при использовании метода k-средних исследователь сам определяет количество итоговых кластеров 

km<-data.frame(Auto,y)
kl1<-km[y==1,]
kl2<-km[y==2,]
kl3<-km[y==3,]

set.seed(my.seed)
inTrain <- sample(seq_along(kl1$mpg),#формируем обучающую выборку
                  nrow(kl1)*train.percent)
fact <- kl1[inTrain, ]
test <- kl1[-inTrain, ]
fitkm1<-lm(fact$mpg ~ . -y -origin -acceleration -horsepower, fact)
summary(fitkm1)
y.model.lm <- predict(fitkm1, test)
MSE.fitkm1 <- sum((y.model.lm - test$mpg)^2) / length(fitkm1)
MSE.fitkm1
set.seed(my.seed)
inTrain <- sample(seq_along(kl2$mpg),#формируем обучающую выборку
                  nrow(kl2)*train.percent)
fact <- kl2[inTrain, ]
test <- kl2[-inTrain, ]
fitkm2<-lm(fact$mpg ~ . -y-acceleration-cylinders-displacement, fact)
summary(fitkm2)
y.model.lm <- predict(fitkm2, test)
MSE.fitkm2 <- sum((y.model.lm - test$mpg)^2) / length(fitkm2)
MSE.fitkm2

set.seed(my.seed)
inTrain <- sample(seq_along(kl3$mpg),#формируем обучающую выборку
                  nrow(kl3)*train.percent)
fact <- kl3[inTrain, ]
test <- kl3[-inTrain, ]
fitkm3<-lm(fact$mpg ~ . -y-displacement-origin -acceleration-cylinders , fact)
summary(fitkm3)
y.model.lm <- predict(fitkm3, test)
MSE.fitkm3 <- sum((y.model.lm - test$mpg)^2) / length(fitkm3)
MSE.fitkm3

x=Auto
hc.complete=hclust(dist(x), method="complete")
hc.average=hclust(dist(x), method="average")
hc.single=hclust(dist(x), method="single")
par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc.average, main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc.single, main="Single Linkage", xlab="", sub="", cex=.9)
cutree(hc.complete, 3)
y<-cutree(hc.average, 3)
cutree(hc.single, 2)
cutree(hc.single, 4)
xsc=scale(x)
plot(hclust(dist(xsc), method="average"), main="Hierarchical Clustering with Scaled Features")

ie<-data.frame(Auto,y)
kl1<-ie[y==1,]
kl2<-ie[y==2,]
kl3<-ie[y==3,]
#1 кластер
set.seed(my.seed)
inTrain <- sample(seq_along(kl1$mpg),#формируем обучающую выборку
                  nrow(kl1)*train.percent)
fact <- kl1[inTrain, ]
test <- kl1[-inTrain, ]
fitie1<-lm(fact$mpg ~ . -y -acceleration-cylinders-displacement, fact)
summary(fitie1)
y.model.lm <- predict(fitie1, test)
MSE.fitie1 <- sum((y.model.lm - test$mpg)^2) / length(fitie1)
MSE.fitie1
set.seed(my.seed)
inTrain <- sample(seq_along(kl2$mpg),#формируем обучающую выборку
                  nrow(kl2)*train.percent)
fact <- kl2[inTrain, ]
test <- kl2[-inTrain, ]
fitkm2<-lm(fact$mpg ~ . -y-acceleration-cylinders-displacement, fact)
summary(fitkm2)
y.model.lm <- predict(fitkm2, test)
MSE.fitkm2 <- sum((y.model.lm - test$mpg)^2) / length(fitkm2)
MSE.fitkm2

set.seed(my.seed)
inTrain <- sample(seq_along(kl3$mpg),#формируем обучающую выборку
                  nrow(kl3)*train.percent)
fact <- kl3[inTrain, ]
test <- kl3[-inTrain, ]
fitkm3<-lm(fact$mpg ~ . -y-displacement-origin -acceleration-cylinders , fact)
summary(fitkm3)
y.model.lm <- predict(fitkm3, test)
MSE.fitkm3 <- sum((y.model.lm - test$mpg)^2) / length(fitkm3)
MSE.fitkm3

x=Auto
hc.complete=hclust(dist(x), method="complete")
hc.average=hclust(dist(x), method="average")
hc.single=hclust(dist(x), method="single")
par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc.average, main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc.single, main="Single Linkage", xlab="", sub="", cex=.9)
cutree(hc.complete, 3)
y<-cutree(hc.average, 3)
cutree(hc.single, 2)
cutree(hc.single, 4)
xsc=scale(x)
plot(hclust(dist(xsc), method="average"), main="Hierarchical Clustering with Scaled Features")

ie<-data.frame(Auto,y)
kl1<-ie[y==1,]
kl2<-ie[y==2,]
kl3<-ie[y==3,]
#1 кластер
set.seed(my.seed)
inTrain <- sample(seq_along(kl1$mpg),#формируем обучающую выборку
                  nrow(kl1)*train.percent)
fact <- kl1[inTrain, ]
test <- kl1[-inTrain, ]
fitie1<-lm(fact$mpg ~ . -y -acceleration-cylinders-displacement, fact)
summary(fitie1)
y.model.lm <- predict(fitie1, test)
MSE.fitie1 <- sum((y.model.lm - test$mpg)^2) / length(fitie1)
MSE.fitie1

#2 кластер

inTrain <- sample(seq_along(kl2$mpg),#формируем обучающую выборку
                  nrow(kl2)*train.percent)
fact <- kl2[inTrain, ]
test <- kl2[-inTrain, ]
fitie2<-lm(fact$mpg ~ . -y -cylinders -horsepower -origin, fact)
summary(fitie2)
y.model.lm <- predict(fitie2, test)
MSE.fitie2 <- sum((y.model.lm - test$mpg)^2) / length(fitie2)
MSE.fitie2
#3 кластер
inTrain <- sample(seq_along(kl3$mpg), #формируем обучающую выборку
                  nrow(kl3)*train.percent)
fact <- kl3[inTrain, ]
test <- kl3[-inTrain, ]
fitie3<-lm(fact$mpg ~ . -y -acceleration -horsepower -origin -weight, fact)
summary(fitie3)
y.model.lm <- predict(fitie3, test)
MSE.fitie3 <- sum((y.model.lm - test$mpg)^2) / length(fitie3)
MSE.fitie3

