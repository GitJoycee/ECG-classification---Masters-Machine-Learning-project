hb.plotter<-function(n){
  #Find the average heart beat length
  #x is a given ecg and y is the corresponding differenced plot.
  #To use hb.plotter for test values change X.train to X.test and X[n,] to Xt.diff[n,]
  x<-X.test[n,]
  y<-Xt.diff[n,]
  mu<-floor((mean(diff(peak.finder(y,0.035,300)$x))-10)/2)
  inds<-peak.finder(y,0.035,mu)$x+31
  
  new.inds<- numeric(length(inds))
  for( i in 1:length(inds)){
    temp <- (inds[i]-31):(inds[i]+31)
    v1 <- which(x==max(x[temp]))
    v2 <- which(v1 %in% temp)
    
    new.inds[i]<- v1[v2[1]]
  }
  
  #plot(X.train[n,],type="l")
  #points(new.inds, X.train[n,new.inds],col=2)
  
  #chop the signal up and represent each heartbeat as a vector
  m<- length(new.inds)-1 # number of full heartbeat cycles
  r <- 30000/m
  p<- 1000            # length we choose for the processed vectors
  X.new<-matrix(NA,m,p)
  for(m_ in 1:m){
    t.old<-new.inds[m_]:new.inds[m_+1]
    x.old<-x[new.inds[m_]:new.inds[m_+1]]
    t.new<-seq(new.inds[m_],new.inds[m_+1],length=p)
    x.new<-approx(t.old,x.old,t.new)$y
    X.new[m_,]<-x.new
  }
  
 X.norm<- matrix(NA,m,p)
  
  for(i in 1:m){
   X.norm[i,] <- X.new[i,]-mean(X.new[i,])
  }
  
 plot(X.new[1,],type='l',main = toString(n))
  for(i in 1:m){
    lines(X.new[i,])
  }
  ans<-list(beats=X.norm,n.beats=m,l.vec=p, int=r)
  
}

r.r.train <- integer(0)  # r to r interval for training data
for(i in 1:115){
  r.r.train[i]<- hb.plotter(i)$int
}

r.r.test <- integer(0)
for(i in 1:100){
  r.r.test[i]<- hb.plotter(i)$int
}


hb.plotter(11)$n.beats
plot(X.train[111,],type="l")
peak.finder(110)
