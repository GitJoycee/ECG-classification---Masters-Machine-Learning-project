avg.hb.plot<-function(n){
  #Function to plot the average heartbeat
 hb<-hb.plotter(n)
 avg.hb<-numeric(hb$l.vec)
 for(i in 1:hb$l.vec){
   avg.hb[i]<-mean(hb$beats[1:hb$n.beats,i])
 }
 plot.ts(avg.hb,main = toString(n))
 ans <- list(heart = avg.hb)
}


train <- matrix(NA,115,1000)
for(i in 1:115){
  train[i,]<- avg.hb.plot(i)$heart  
}


newtrain <- train[c(4,20,24,28,31,37,52,57,64,66,70,83,86,89,93,108,111,115),]
write.csv(newtrain,file="newtrain.csv",row.names=F)

test <- matrix(NA,100,1000)
for(i in 1:100){
  test[i,]<- avg.hb.plot(i)$heart  
}
newtest <- test[c(5,7,16,19,24,46,52,52,57,61,65,67,73,74,77,83),]
write.csv(newtest,file="newtest.csv",row.names=F)

train1 <- cbind(train,y.train)


sample <- sample.int(n = nrow(train), size = floor(.75*nrow(train)), replace = F)
train.par <- train[sample, ]
y.train.par <- y.train[sample,]
test.par  <- train[-sample,]
y.test.par <- y.train[-sample,]
y.test.par

y.test.par != pred.par

nb.par<- naive_bayes(train.par,y.train.par)
pred.par <- predict(nb.par,test.par)
pred.par

nb <- naive_bayes(train,y.train)
nb
pred <- predict(nb,test)
table(pred)

write.csv(pred,file="ECG_group_A.csv",row.names=FALSE)




cl <- factor(train1[,201])
predictions<- knn(train,test,cl,k=40)
write.csv(predictions, file = "ECG_group_A.csv", row.names=FALSE)
