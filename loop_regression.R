# Advance R practice

# apply, lapply, tapply

# prerequisite 10.3 

power <- function(exponent) {
  function(x) {
    x ^ exponent
  }
}

square <- power(2)
square(2)

# lapply
l <- replicate(20, runif(sample(1:10,1)), simplify = FALSE)
l

mtcars[] <- lapply(mtcars, function(x) )

c <- 1:10
c

d <- c('a', 'b', 'c')

for (i in c) {
  print(1 + i)
}


for (i in d) {
  print(i)
}

for (i in d) {
  opkim < - cbind( i,i)
}

print(paste("The year is", 2010))
print(paste("The year is", 2011))

year <- c(2010, 2011, 2012)
varkim <- c('aa', 'bb', 'cc')
year
varkim

for (yr in year) {
  print(paste('this year is', yr))
}

for (form1 in varkim) {
print(as.formula(paste('y ~', form1)))
}

}

# loop columns for regression

mydata <- mtcars
names(mydata)
apply(mydata[,2:11],2, function(x) 
  print(summary(lm( mpg ~ x, data = mydata))))

# Barry

sumlm <- function(outcome, predictors, data){
  mlist = lapply(predictors, function(predictor){
    f = as.formula(paste0(outcome,"~", predictor))
    m = broom::tidy(lm(f, data=data))
    m$predictor = predictor
    m
  })
  do.call(rbind,mlist)
}


sumlm("mpg",c("hp","cyl","disp"),mydata)
sumlm("mpg",names(mtcars),mydata)

# multiple histogram

hist(mydata$mpg)
hist(mydata$cyl)
hist(mydata$disp)

apply(mydata[,c('mpg', 'cyl', 'disp')], 2, function(x) {
  hist(x, col = 'blue') 
})

outcome <- 'mpg'
outcome
predictor <- 'hp'
f = as.formula(paste0(outcome,"~", predictor))
f               
af = paste0(outcome,"$", predictor)
af
aff <- as.(af)
aff

data2 <- 'mtcars'
data2
vars <- 'hp'
vars
f = paste0(data2,"$", vars)
f 
hist(f)


#### this works - codes by Barry

data2 <- 'mtcars'
data2
vars <- 'hp'
vars
get(data2)[vars]
get(data2)[[vars]]
hist(get(data2)[[vars]])

parse(text =  f)
eval(parse(text = f ))









