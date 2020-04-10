knn<-read.csv("F://vag/Zoo.csv")
View(knn)
#first name is not requied bez it is animal name.
table(knn$type)
str(knn$type)
#record type as a factor
knn$type<-factor(knn$type,levels = c("1","2","3","4","5","6","7"),labels = c("mammel","bird","fish","reptile","amphibian","insect","cheetha"))
View(knn$type)
#table or proportion with more information labels
round(prop.table(table(knn$type))*100,digits = 1)
##check with summary 
summary(knn[c(1:17)])
#lets we will make normal data in legs.
normalize<-function(x){
  return((x-min(x)) / (max(x) - min(x)))
}
#lets test normalization function-result should 
#be identical
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))
#let's normalize the data of legs
knn_n<-as.data.frame(lapply(knn[2:17], normalize))
## lets check now the data is normalize
summary(knn_n)
## create training and test data
knn_train<-knn[1:70,]
knn_test<-knn[71:101,]
## create training and test data
Knn_train_labels<-knn[1:70,18]
View(Knn_train_labels)
knn_test_labels<-knn[71:101,18]
##training the data through the class 
install.packages("class")
library(class)
knn_train_pred<-knn(train = knn_train,test = knn_test,
                    cl = Knn_train_labels,k=7)
##load the gmodels library
install.packages("gmodels")
library(gmodels)
CrossTable(x = knn_test_labels,y=knn_train_pred,prop.chisq = FALSE)
## using loop
knn1<-read.csv("F://vag/Zoo.csv")
str(knn1$type)
table(knn1$type)
knn1$type<-factor(knn1$type,levels = c("1","2","3","4","5","6","7"),
                  labels = c("mammel","bird","fish","reptile","amphibian","insect","cheetha"))
round(prop.table(table(knn1$type))*100,digits = 1)
summary(knn1[c(2:17)])
normalize<-function(x){
  return((x - min(x)) / (max(x) - min(x)))
}
##test the normalize the data
normalize(c(1,2,3,4,5))
normalize(c(10,20,30,40,50))
knn1_type<-as.data.frame(lapply(knn1[2:17],normalize))
summary(knn1_type)
#create data from train to test
knn1_train<-knn1_type[1:70,]
knn1_test<-knn1_type[71:101,]
#create data from train to test
knn1_train_label<-knn1[1:70,18]
knn1_test_label<-knn1[71:101,18]
library(class)
test_acc<-NULL
train_acc<-NULL
for (i in seq(1,30,2))
  {
  Knn1_train_pred<-knn(train=knn1_train,test=knn1_train,cl=knn1_train_label,k=i)
  train_acc<-c(train_acc,mean(Knn1_train_pred==knn1_train_label))
  Knn1_test_pred<-knn(train=knn1_test,test=knn1_test,cl=knn1_test_label)
  test_acc<-c(test_acc,mean(Knn1_test_pred==knn1_test_label))
  
}
set.seed(20)
par(mfrow=c(1,2))#c(1,2)indicates 1 row and 2 col
windows()
plot(seq(1,30,2),train_acc,type="l",main="Train_accuracy",col="green")
plot(seq(1,30,2),test_acc,type="l",main="Test_accuracy",col="red")

acc_df<-data.frame(list(train_acc=train_acc,test_acc=test_acc,neigh=seq(1,50,2)))
install.packages("ggplot2")
library(ggplot2)
ggplot(acc_df,aes(x=neigh))+
  geom_line(aes(y=train_acc,colour="train_acc"),lwd=1.5)+
  geom_line(aes(y=test_acc,colour="test_acc"),lwd=1.5)+
  scale_fill_manual(" ",breaks = c("train_acc","test_acc"),values = c("train_acc"="green","test_acc"="red"))
glass_pred<-knn(train = knn1_train,test = knn1_test,cl=knn1_train_label,k=7)
