if (!require("class")) install.packages("class")
if (!require("dtw")) install.packages("dtw")
library(dtw)
ntrial=50
ntest=50
tslength=50
deltat=1
d=1
testdataset1<-matrix(ncol=tslength, nrow=ntest)
testdataset2<-matrix(ncol=tslength, nrow=ntest)
traindataset1<-matrix(ncol=tslength, nrow=ntrial)
traindataset2<-matrix(ncol=tslength, nrow=ntrial)
for(i in c(1:ntest)){
  testdataset1[1,i]=0
  testdataset2[1,i]=0
  for (j in c(2:tslength)){
    testdataset1[j,i]=testdataset1[j-1,i]+sqrt(deltat)*rnorm(1)
    testdataset2[j,i]=testdataset2[j-1,i]+d*deltat+sqrt(deltat)*rnorm(1)
  }
}

for (i in c(1:ntrial)){
  traindataset1[1,i]=0
  traindataset2[1,i]=0
  for (j in c(2:tslength)){
    traindataset1[j,i]=traindataset1[j-1,i]+rnorm(1)
    traindataset2[j,i]=traindataset2[j-1,i]+0.5+rnorm(1)
  }
}
testdataset<-rbind(traindataset1,traindataset2,testdataset1,testdataset2)
train.labels <- c(rep(1, ntrial), rep(2, ntrial))
dist1 <- dist(testdataset, method = "dtw")
dist.matrix <- as.matrix(dist1)
k<-2
indnn<-matrix(ncol=k, nrow=(2*ntest))
for (i in c(1:(2*ntest))){
  disttest<-dist.matrix[1:(2*ntrial),i+(2*ntrial)]
  sortedist<-sort(disttest)
  indnn1<-which(disttest %in% sortedist[1:k])
  if (length(indnn1)!=k){
    print("elementi con la stessa distanza: effettuato troncamento")
  }
  indnn[i,]<-indnn1[1:k]
}
group<-matrix(nrow=2*ntest, ncol=k)
for (i in c(1:(2*ntest))){
  for (j in c(1:k)){
    if (indnn[i,j]<=ntrial){
      group[i,j]=1
    }else{
      group[i,j]=2
    }
  }
}
finalgroup<-vector(length=2*ntest)
for (i in c(1:(2*ntest))){
  groupings <- unique(group[i,])
  finalgroup[i]=groupings[which.max(tabulate(match(group[i,], groupings)))]
}

finalgroup
t<-ntrial*2
plot.df <- data.frame(traindataset2,testdataset2, finalgroup)
plot(1:t, plot.df$X1, col=c("red","blue")[finalgroup], xlab="", ylab="")
points(1:t, plot.df$X2, col=c("red","blue")[finalgroup])
points(1:t, plot.df$X3, col=c("red","blue")[finalgroup])
points(1:t, plot.df$X4, col=c("red","blue")[finalgroup])
points(1:t, plot.df$X5, col=c("red","blue")[finalgroup])
points(1:t, plot.df$X6, col=c("red","blue")[finalgroup])
points(1:t, plot.df$X7, col=c("red","blue")[finalgroup])
points(1:t, plot.df$X8, col=c("red","blue")[finalgroup])
points(1:t, plot.df$X9, col=c("red","blue")[finalgroup])
points(1:t, plot.df$X10, col=c("red","blue")[finalgroup])