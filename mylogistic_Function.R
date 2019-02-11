oc <- rbinom(n = 50, size = 1, prob = 0.5)
iv <- rnorm(n = 50, mean = 0, sd = 1)
iv2 <- rnorm(n = 50, mean = 10, sd = 1)
mydata <- data.frame(oc, iv, iv2)


myfunction4 <- function(a, mydata2){
  model4 <- glm(as.formula(a), family = binomial, data = mydata2)
  print(summary(model4))
  print(broom::tidy(model4))
  print("---------------------------------")
  
  b_or <- cbind(coef(model4), exp(coef(model4)))
  colnames(b_or) <- c('beta', 'OR')
  print(b_or)
  
  print("----------------------------------")
  b_or_ci <- cbind(confint(model4), exp(confint(model4)))
  colnames(b_or_ci) <- c('lower.ci_beta', 'upper.ci_beta', 
                         'lower.ci_OR', 'upper.ci_or')
  print(b_or_ci)
}

myfunction4(oc ~ iv + iv2, mydata2 = mydata)


myfunction <- function(a,b){
  mymodel <- glm(as.formula(paste(a, b, sep="~")), family = binomial, data = mydata)
  print(mymodel)
  print(summary(mymodel))
  print("This is the odds ratio")
  print("######################")
  print(exp(coef(mymodel)))
  print("This is the GOF checking")
  print("########################")
  LogisticDx::gof(mymodel)
   } 

myfunction("oc", "iv")

######################################################

myfunction2 <- function(a){
  mymodel <- glm(as.formula(a), family = binomial, data = mydata)
  print(mymodel)
  print(summary(mymodel))
  print("This is the odds ratio")
  print("######################")
  print(exp(coef(mymodel)))
  print("This is the GOF checking")
  print("########################")
  LogisticDx::gof(mymodel)
} 

myfunction2(oc ~ iv + iv2)

## this does not work for GOF

myfunction3 <- function(a, mydata2){
  mymodel3 <- glm(as.formula(a), family = binomial, data = mydata2)
  print(mymodel3)
  print(summary(mymodel3))
  print("This is the odds ratio")
  print("######################")
  print(exp(coef(mymodel3)))
  print("This is the GOF checking")
  print("########################")
  LogisticDx::gof(mymodel3)
}

myfunction3("oc ~ iv + iv2", mydata2 = mydata)




