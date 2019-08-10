peak.finder<-function(x,tol=0.035,r=50){
  #plot.ts(x)
  q<-length(x)
  inds<-numeric(q)
  for ( i in 1:q){
    if((i-r)<1){
      if(x[i] > max(x[c(1:(i-1))]) && x[i] >= max(x[c((i+1):(i+r))]) && x[i]>tol){
        inds[i]<-1
      }else{
        inds[i]<-0
      }
    }else if((i+r)>length(x)){
      if(x[i] > max(x[c((i-r):(i-1))]) && x[i] == max(x[c(i:length(x))]) && x[i]>tol){
        inds[i]<-1
      }else{
        inds[i]<-0
    }
    }else{
      if(x[i] > max(x[c((i-r):(i-1))]) && x[i] >= max(x[c((i+1):(i+r))]) && x[i]>tol ){
        inds[i]<-1
      }else{
        inds[i]<-0
      }
    }
  }
  peaks<-which(inds==1)
  #points(peaks,x[peaks],col=2)
  beat<-list(x=peaks)
}

