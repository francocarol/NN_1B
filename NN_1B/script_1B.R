# install.packages("neuralnet")
# https://cran.r-project.org/web/packages/neuralnet/neuralnet.pdf

# Using neuralnet package

library(neuralnet)

help(neuralnet)

# Data

sample1 <- read.csv("data/Sample1", sep=" ", header = F)
sample2 <- read.csv("data/Sample2", sep=" ", header = F)
sample3 <- read.csv("data/Sample3", sep=" ", header = F)

# First ex: SIMPLE DATA (Sample1) ----

#data <- sample1
# index <- sample(1:nrow(data),round(0.75*nrow(data)))
# train <- data[index,]
# test <- data[-index,]

# truth and input columns:
# we will only use the first 2 columns of data, for simplicity

truth <- sample1$V1 # taking the 1st column from the data frame (0s and 1s)
input1 <- sample1$V2
input2 <- sample1$V3 

df <- data.frame(truth,input1,input2)

head(df)
nnet1<-neuralnet(truth~input1+input2,df,c(8,5))
plot(nnet1)
test=compute(nnet1,t(c(1,2)))
test$net.result
# 1.001386687

# higher alpha

truth <- sample1$V1 # taking the 1st column from the data frame (0s and 1s)
input1 <- sample1$V2
input2 <- sample1$V3 

df <- data.frame(truth,input1,input2)

head(df)
nnet3<-neuralnet(truth~input1+input2,df,c(8,5),learningrate=0.15)
plot(nnet3)
test=compute(nnet3,t(c(1,2)))
test$net.result
# alpha = 0.15 , test$net.result = 1.001237436

# different alpha
out <-c()
out3 <-c()
for (alpha in seq(0.05,0.95, 0.05)) {
  nnet3<-neuralnet(truth~input1+input2,df,c(8,5),learningrate=alpha)
  test=compute(nnet3,t(c(1,2)))
  out <- rbind(out, cbind(alpha, test$net.result))
  test=compute(nnet3,t(c(1,2,3,2,1)))
  
  truthin <- as.numeric(truth)
  myout <- as.numeric(unlist(nnet3$net.result))#ifelse(nnet3$net.result[[1]]>0.5,1,0)
  #bin <- 
  
  #normm <- norm(truthin[[]] - myout[[]] , type="2") 
  # 
  # for (i in 1:1000) {
  #   #if ((as.numeric(unlist(truthin[i])) - as.numeric(unlist(myout[i]))) < 0.5) {
  #   if ((as.numeric(unlist(truthin[i])) - as.numeric(unlist(myout[i]))) < 0.5) {
  #     res <- 0
  #   }else{
  #       res <- 1
  #     }
  # }
  # 
  
  for (i in 1:1000) {
    res <- (truthin - myout)*(truthin - myout)
  }
  summ <- sum(res)
  
  out3 <- rbind(out3, cbind(alpha, summ))
  (cbind(alpha, test$net.result))
}

library(tidyverse)

ggplot(data=as.data.frame(out3)) +
  geom_path()

# Second ex: SIMPLE DATA (Sample1), with more inputs ----

truth <- sample1$V1
V2 <- sample1$V2
V3 <- sample1$V3
V4 <- sample1$V4
V5 <- sample1$V5
V6 <- sample1$V6

nnet2<-neuralnet(truth~V2+V3+V4+V5+V6,df,c(8,5),
                lifesign='full',
                algorithm='backprop',
                learningrate=0.05,
                linear.output=FALSE)
plot(nnet2)
test=compute(nnet2,t(c(1,2,3,2,1)))
test$net.result
# 0.9951220643 if I run it once
new <- ifelse(nnet2$net.result[[1]]>0.5,1,0)

out_res <-c()
for (i in 1:10) {
  nnet2<-neuralnet(truth~V2+V3+V4+V5+V6,df,c(8,5),
                   lifesign='full',
                   algorithm='backprop',
                   learningrate=0.15,
                   linear.output=FALSE)
  #plot(nnet2)
  test=compute(nnet2,t(c(1,2,3,2,1)))
  test$net.result
  out_res<-rbind(out_res, cbind(i, test$net.result))
  (cbind(i, test$net.result))
}
plot(out_res)

out_res <-c()
for (i in 1:100) {
  nnet2<-neuralnet(truth~V2+V3+V4+V5+V6,df,c(8,5),
                   lifesign='full',
                   algorithm='backprop',
                   learningrate=0.25,
                   linear.output=FALSE)
  #plot(nnet2)
  test=compute(nnet2,t(c(1,2,3,2,1)))
  test$net.result
  out_res<-rbind(out_res, cbind(i, test$net.result))
  (cbind(i, test$net.result))
}
plot(out_res)

# ROC plots ----
# netsays

Nsample <- dim(sample1)[1]
print(head(sample1))
for (i in 1:Nsample) {print(sample1[i,1]); print(sample1[i,-1])}
plot(c(0,1),c(0,1))
v <- netsays(t(sample1[,-1]))
p <- sample1[order(v),1]
nc <- sum(sample1[,1]==0)
nd <- Nsample-nc
nnc <- nc
nnd <- nd
for (i in 1:length(p)) {if(p[i]==1) {nd <- nd-1} else {nc <- nc-1}
  points(nc/nnc,nd/nnd,pch='.') }
vc <- rep(0,nnc)
vd <- rep(0,nnd)
nc <- 0
nd <- 0
for (i in 1:Nsample){
  itype <- sample1[i,1]
  isay <- netsays(as.numeric(sample1[i,-1]))
  if(itype==0) {nc <- nc+1;vc[nc] <- isay} else {nd<- nd+1;vd[nd] <- isay}
}
hc <- hist(vc,breaks=seq(0,1,.05))
hd <- hist(vd,breaks=seq(0,1,.05))

# Sample 2

truth <- sample2$V1
V2 <- sample2$V2
V3 <- sample2$V3
V4 <- sample2$V4
V5 <- sample2$V5
V6 <- sample2$V6

nnet2<-neuralnet(truth~V2+V3+V4+V5+V6,df,c(8,5),
                 lifesign='full',
                 algorithm='backprop',
                 learningrate=0.2,
                 linear.output=FALSE)
plot(nnet2)
test=compute(nnet2,t(c(1,2,3,2,1)))
test$net.result
# this set takes a lot of time

nnet2$net.result

# sample 2 loop


out_res2 <-c()
#for (i in 1:10) {
  nnet2<-neuralnet(truth~V2+V3+V4+V5+V6,df,c(8,5),
                   lifesign='full',
                   algorithm='backprop',
                   learningrate=0.15,
                   linear.output=FALSE)
  #plot(nnet2)
  test=compute(nnet2,t(c(1,2,3,2,1)))
  test$net.result
  out_res2<-rbind(out_res, cbind(i, test$net.result))
  (cbind(i, test$net.result))
#}
plot(out_res2)

# different alpha
out2 <-c()
for (alpha in seq(0.25,0.95, 0.05)) {
  nnet3<-neuralnet(truth~V2+V3+V4+V5+V6,df,c(8,5),
                   lifesign='full',
                   algorithm='backprop',
                   learningrate=alpha,
                   linear.output=FALSE)
  test=compute(nnet3,t(c(1,2,3,2,1)))
  
  truthin <- as.numeric(truth)
  myout <- as.numeric(unlist(nnet3$net.result))#ifelse(nnet3$net.result[[1]]>0.5,1,0)
  #bin <- 
  
  #normm <- norm(truthin[[]] - myout[[]] , type="2") 
  # 
  # for (i in 1:1000) {
  #   #if ((as.numeric(unlist(truthin[i])) - as.numeric(unlist(myout[i]))) < 0.5) {
  #   if ((as.numeric(unlist(truthin[i])) - as.numeric(unlist(myout[i]))) < 0.5) {
  #     res <- 0
  #   }else{
  #       res <- 1
  #     }
  # }
  # 
  
  for (i in 1:1000) {
      res <- (truthin - myout)*(truthin - myout)
  }
  summ <- sum(res)
  
  out2 <- rbind(out2, cbind(alpha, summ))
  (cbind(alpha, test$net.result))
}

#confidence.interval(nnet3, alpha=0.15)
plot(out2)
