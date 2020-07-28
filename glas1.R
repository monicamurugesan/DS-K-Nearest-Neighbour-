install.packages('caTools')
library(caTools)
library(dplyr)
library(ggplot2)
library(class)
library(caret)
library(corrplot)
dataglass <-read.csv(file.choose())
View(dataglass)
attach(dataglass)
normalize <-scale(dataglass[,-10])
View(normalize)
commonbind <-cbind(normalize,dataglass[10])
View(commonbind)
anyNA(commonbind)
head(commonbind)
corrplot(cor(commonbind))
pairs(commonbind)

sample<-sample.split(commonbind$Type,SplitRatio=0.70)
train <-commonbind[sample,]
test <-commonbind[-sample,]
View(train)
View(test)
model1 <-knn(train[1:9],test[1:9],train$Type,k=1)
summary(model1)
error<-mean(model1!=test$Type)
error
confusionMatrix(table(model1,test$Type))
pred1<-NULL
er1<-NULL

for(i in 1:10){
  pred1 <-knn(train[1:9],test[1:9],train$Type,k=i)
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

model2 <-knn(train[1:9],test[1:9],train$Type,k=3)
summary(model2)
error1<-mean(model2!=test$Type)
error1
confusionMatrix(table(model2,test$Type))
pre2<-NULL
er2<-NULL
for(k in 1:10){
  pre2<-knn(train[1:9],test[1:9],train$Type,k=k)
  er2<-mean(pre2!=test$Type)
}

# knn_err1 <-as.data.frame(cbind(k=1:9,error=er2))
# ggplot(knn_err1,aes(k,er2))+ 
#   geom_point()+ 
#   geom_line() + 
#   scale_x_continuous(breaks=1:10)+ 
#   theme_bw() +
#   xlab("Value of K") +
#   ylab('Error')

model3 <-knn(train[1:9],test[1:9],train$Type,k=5)
summary(model3)
error3<-mean(model3!=test$Type)
error3
confusionMatrix(table(model3,test$Type))
pre3<-NULL
er3<-NULL
for(k in 1:10){
  pre2<-knn(train[1:9],test[1:9],train$Type,k=k)
  er2<-mean(pre2!=test$Type)
}

plot(model3)
plot(model2)
plot(model1)

