#data frame for covariates/independent variables (X)
set.seed(1000)
values <- runif(50)
values[sample(1:length(values), 10)] <- NA
ind.var <- data.frame(matrix(values, ncol=5))
colnames(ind.var) <- paste0("X", 1:5)


# a dependent var (Y)
dep <- runif(10)
dep

# create regression between each column in the dataframe against  dep variable
lms3 <- lapply(colnames(ind.var), function(x) {
  form.reg <- paste0("dep~",x)
  lm(form.reg, data=ind.var)
})
lms3

# the secret lies in these 2 lines -- see how this happens to explain above
lapply(colnames(ind.var), function(x) { 
  form.reg<-paste0("dep~",x)}) 

### USING apply ONLY, and see how the results differ 
# create regression between each column in Y and X
lms4 <- apply(ind.var,2, function(x) {
  lm(dep~x,data=ind.var)
})

lms4
lm(dep~X5,data=ind.var)
lm(dep~X4,data=ind.var)