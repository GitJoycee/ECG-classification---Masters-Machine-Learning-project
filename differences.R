m <- length(X.train[,1])
p<-length(X.train[1,])
X<-matrix(NA,m,p-62)
for(i in 1:m){
  X[i,]<- (diff(X.train[i,],lag=31,differences=2))^2
}


Xt.diff<-matrix(NA,m,p-62)
m1 <- length(X.test[,1])
for(i in 1:m1){
  Xt.diff[i,]<- (diff(X.test[i,],lag=31,differences=2))^2
}

for(i in 1:m){
  plot(X[i,],type="l")
}

L <- (diff(X.test[83,],lag=31,differences=2))^2
plot(L,type="l")
plot(X[106,],type="l")
plot(X.train[106,],type="l")


avg.hb.plot(79)
plot.ts(X[1,])
plot.ts(X.train[106,])
plot.ts(X[106,])     
plot(Xt.diff[1,],type="l")


X.norm[i,] <- matrix(NA,m,p)

for(i in 1:m){
  X.norm[i,] <- X.new[i,]-mean(X.new[i,])