############################### looping
x1<-rnorm(n=100,mean=50,sd=10)
x2<-rbinom(n=100,size=1,prob=.7)
x3<-as.factor(x2)
str(x3)
error1<-rnorm(n=100,mean=10,sd=4)
y2<-10+1.5*x1+error1
plot(x=x1,y=y2)
data<-data.frame(y2,x1,x3)
str(data)
names(data)
names(data)[1]
names(data)[3]
names(data)[1:3]

1:ncol(data)
for (i in 1:ncol(data)){
  print(names(data)[i])
  print(summary(data[,i]))
  print(lm(y2~data[,i],data=data))
  print(summary(lm(y2~data[,i],data=data)))
  print(summary(lm(y2~data[,i],data=data))[4])
}

#other method

values <- runif(50)
values[sample(1:length(values), 10)] <- NA
Y <- data.frame(matrix(values, ncol=5))
colnames(Y) <- paste0("Y", 1:5)

# single regression term
X <- runif(10)

# create regression between each column in Y and X
lms <- lapply(colnames(Y), function(y) {
  form <- paste0(y, " ~ X")
  lm(form, data=Y)
})
form
lms

#OPTION3
x4 <-  matrix(runif(10*10), 10,10)
y4 <-  runif(10)
x4
y4

t(apply(x4, 2, function(x.col) lm(y4~x.col)$coef))
apply(x4, 2, function(x.col) lm(y4~x.col)$coef)

#OPTION
x5 <- 1:10 
x5
mat <- matrix(rnorm(200), nrow=20, ncol=10) 
mat
resultlist <- apply(mat, 1, function(y) lm(y ~ x5)) 
resultlist
resultcoeffs <- apply(mat, 1, function(y) lm(y ~ x5)$coefficients) 

#option
id <- rep(1:2, c(6,8))
correct <- sample(0:1,14,TRUE)
phase <- c(rep("discr",3),rep("rev",3), rep("discr",4),rep("rev",4))
dat <- data.frame(id,correct,phase)
lapply(split(dat, dat$id), function(x) coef(summary(glm(correct~phase,family="binomial",data=x))))

#more info
set.seed(1)  # Makes the call to rnorm generate the same numbers every time
( mymat <- matrix(round(rnorm(16,10),2),4,4) )

myfunc <- function(x) {
  return(x/sum(x))
}

#slower
myfunc(mymat[,1])
all.equal(myfunc(mymat[,1]), mymat[,1]/sum(mymat[,1]))


#better
apply(mymat, 2, myfunc)


## the best?
set.seed(1000)
values <- runif(50)
values[sample(1:length(values), 10)] <- NA
Y <- data.frame(matrix(values, ncol=5))
colnames(Y) <- paste0("Y", 1:5)


# single regression term
X <- runif(10)
X

# create regression between each column in Y and X
lms <- lapply(colnames(Y), function(y) {
  form <- paste0(y, " ~ X")
  lm(form, data=Y)
})

lms

# create regression between each column in Y and X
lms1 <- lapply(colnames(Y), function(y) {
  form <- paste(y, " ~ X")
  lm(form, data=Y)
})

lms1



