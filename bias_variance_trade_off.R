# Bias-variance trade-off is a key concept in evaluating the performance of a machine learning algorithm. 
# An overly simplified model suffers from underfitting (high bias) while a highly complex model suffers 
# from overfitting (high variance). 
# This example illustrates the bias-variance trade-off effect:

# Author: Benjamin O. Tayo

# Date: 12/17/2018

#import necessary libraries
library(dslabs)
library(caret)
library(tidyverse)
library(caret)
library(broom)

# create data of sin(x) with random noise added
set.seed(1)
X<-seq(0,2*pi,length=100)
Y<-sin(X)+rnorm(100,0,0.5)

# define degree of polynomial
n<-9

# data frame containing X and Y 
df<-data.frame(X=X,Y=Y)

# generate polynomial fits of the data set for different degrees
fitted<-vector()
error<-vector(length = n)
variance<-vector(length = n)

for (i in 1:n) {
  fitted<-c(fitted,augment(lm(Y ~ poly(X,i)))$.fitted)
  error[i]<-(1/100)*sum((Y-augment(lm(Y ~ poly(X,i)))$.fitted)^2)
  variance[i]<-sd(augment(lm(Y ~ poly(X,i)))$.fitted)^2
}

X<-rep(X,times=n)
degree<-sort(rep(1:n,times=100))

# create dataframe containing fitted Y values
data<-data.frame(X=X,Y=fitted,degree=degree)

# generate visualizations 
data%>%ggplot(aes(X,Y))+geom_line()+geom_point(aes(X,Y),data = df)+facet_wrap(~degree)

# plot bias error
plot(1:n,error)
lines(1:n,error)

# plot variance error
plot(1:n,variance)
lines(1:n,variance)

