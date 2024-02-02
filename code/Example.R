rm(list=ls())
graphics.off()
library('ggplot2')

#set working directory to the location of Functions.R
source("Functions.R")

#contagion synthetic example

set.seed(7)
T<-252

a<- -1
b<- 1
ae<- -2
be<- 2

m1<-runif(T,min=a,max=b)
e<-runif(T,min=ae,max=be)

t<-(1:T)
highind<-t>=(T-9*21+1) & t<T-6*21

thigh<-t[highind]
tlow<-tlow<-t[!highind]

m1[thigh]<-m1[thigh]*10
m2<-0.2*m1+e

myX<-myTIME<-vector('list',length=2)

myX[[1]]<-(m1-mean(m1))/sd(m1)
myX[[2]]<-(m2-mean(m2))/sd(m2)
myTIME[[1]]<-myTIME[[2]]<-1:T

tau.values<-c(5,10,20)
tau.values.matrix<-t(combn(tau.values,m=2))
K<-dim(tau.values.matrix)[2]
temp<-tau.values.matrix[,K:1] #reverse the combinations!!!
tau.values.matrix<-rbind(tau.values.matrix,temp)
tau.values.matrix<-rbind(tau.values.matrix,cbind(tau.values,tau.values))

df<-NULL

for(i in 1:dim(tau.values.matrix)[1])#as many tau vectors as the number of rows
{
  dftemp<-MTTPMA(X=myX,TIME=myTIME,xout=NULL,tau=tau.values.matrix[i,])
  dftemp$thickness<-toString(tau.values.matrix[i,])
  dftemp$thickness1<-tau.values.matrix[i,1]
  dftemp$thickness2<-tau.values.matrix[i,2]
  df<-rbind(df,dftemp)
}  
head(df)
tail(df)

gg<-ggplot(data=df,aes(x=time,y=rho))+geom_line()+facet_grid(thickness1~thickness2)
gg<-gg+theme_bw()+coord_cartesian(ylim=c(0,1))
print(gg)

