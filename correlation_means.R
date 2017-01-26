## my testing function

myfunc<-function(a,b, digits=2) { 
  #print data
  dataex<-data.frame(a,b)
  print(head(dataex))
  cor1<-cor(a,b)
  mean1<-mean(a)
  mean2<-mean(b)
  message(sprintf("the mean 
                  for the 1st var is %.2f and 
                  for the 2ns var is %.2f .
                  The correlation between them is %s",mean1, mean2,cor1))
  plot(dataex[,1], dataex[,2],main="scatter")
}

#test

bmi<-rnorm(n=50)
sbp<-rnorm(n=50)
data1<-data.frame(bmi,sbp)

#run
myfunc(bmi,sbp)
