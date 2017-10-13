#install.packages("foreign")
#install.packages("data.tree")
#library(data.tree)
#library(foreign)
setwd("C:/Users/chengning/Desktop/R/760cs/hw1")


train<-read.arff("heart_train.arff")[[1]]
test<-read.arff("heart_test.arff")[[1]]


##diabetes
train<-read.arff("diabetes_train.arff")
test<-read.arff("diabetes_test.arff")




tree<-list(name='')
tree<-ID3(tree,train,10)
Printree(tree)
prediction(tree,test)










