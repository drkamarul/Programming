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
lm(Y5~X,data=Y)


# create regression between each column in Y and X
lms1 <- lapply(colnames(Y), function(y2) {
  form <- paste(y2, " ~ X")
  lm(form, data=Y)
})

lms1

########### THIS WORKSSSSSSSSSSSSSSS .. alhamdulillah
set.seed(1000)
values <- runif(50)
values[sample(1:length(values), 10)] <- NA
ind.var <- data.frame(matrix(values, ncol=5))
colnames(ind.var) <- paste0("X", 1:5)


# single regression term
dep <- runif(10)
dep

# create regression between each column in Y and X
lms3 <- lapply(colnames(ind.var), function(x) {
  form2 <- paste0("dep~",x)
  lm(form2, data=ind.var)
})

#see how this happens to explain above
lapply(colnames(ind.var), function(x) {
  +     form2 <- paste0("dep~",x) })


lms3
lm(dep~X5,data=ind.var)
lm(dep~X4,data=ind.var)

################################## IF USING apply ONLY

# create regression between each column in Y and X
lms4 <- apply(ind.var,2, function(x) {
    lm(dep~x,data=ind.var)
})

lms4
lm(dep~X5,data=ind.var)
lm(dep~X4,data=ind.var)

###TAK JADI
lms5 <- apply(ind.var,2, function(x) {
  summary(lm(dep~x,data=ind.var))
  })

lms5
lm(dep~X5,data=ind.var)
lm(dep~X4,data=ind.var)






