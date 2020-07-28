library(caTools)
library(dplyr)
library(ggplot2)
library(class)
library(caret)
library(corrplot)

setwd("H:\\RStudio\\assignment")
datazoo<-read.csv(file.choose())
View(datazoo)
data1<-datazoo
normal <-scale(datazoo[,-c(1,18)])
View(normal)
zoo <-cbind(normal,datazoo[18])
View(zoo)
anyNA(zoo)
head(zoo)
corrplot(cor(zoo))
pairs(zoo)
samplezoo<-sample.split(zoo$type,SplitRatio=0.70)
ztrain <-commonbind[samplezoo,]
ztest <-commonbind[-samplezoo,]
View(ztrain)
View(ztest)
zmodel1 <-knn(ztrain[1:10],ztest[1:10],ztrain$Type,k=3)
summary(zmodel1)
error<-mean(zmodel1!=ztest$Type)
error
confusionMatrix(table(zmodel1,ztest$Type))
pred1<-NULL
er1<-NULL

for(i in 1:10){
  pred1 <-knn(ztrain[1:10],ztest[1:10],ztrain$Type,k=i)
  er1<-mean(pred1!=test$Type)
}
knn_err <-as.data.frame(cbind(k=1:9,error=er1))
ggplot(knn_err,aes(k,er1))+ 
  geom_point()+ 
  geom_line() + 
  scale_x_continuous(breaks=1:10)+ 
  theme_bw() +
  xlab("Value of K") +
  ylab('Error')

model2 <-knn(ztrain[1:10],ztest[1:10],ztrain$Type,k=5)
summary(model2)
error1<-mean(model2!=ztest$Type)
error1
confusionMatrix(table(model2,ztest$Type))
pre2<-NULL
er2<-NULL
for(k in 1:10){
  pre2<-knn(ztrain[1:10],ztest[1:10],ztrain$Type,k=k)
  er2<-mean(pre2!=ztest$Type)
}

model3 <-knn(ztrain[1:10],ztest[1:10],ztrain$Type,k=11)
summary(model3)
error3<-mean(model3!=ztest$Type)
error3
confusionMatrix(table(model3,ztest$Type))
pre3<-NULL
er3<-NULL
for(k in 1:10){
 pre3 <-knn(ztrain[1:10],ztest[1:10],ztrain$Type,k=k)
 er3 <-mean(pre3!=ztest$Type)
}

plot(model3)
plot(model2)
plot(zmodel1)
