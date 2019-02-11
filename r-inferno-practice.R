# practice R inferno

vec <- numeric(0)
for(i in 1:10) 
vec <- c(vec, i)
vec


vec3 <- numeric(10)
for(i in 1:10) vec3[i] <- i
vec3


vec2 <- 1:10
vec2

vec ; vec2 ; vec3

my.df <- data.frame(a=character(0), b=numeric(0))
my.df
for(i in 1:10) {
  my.df <- rbind(my.df, data.frame(a=sample(letters, 1),
                                   b=runif(1)))
}


my.df <- data.frame(a=character(0), b=numeric(0))
my.df
for(i in 1:10) {
  this.N <- rpois(1, 10)
  my.df <- rbind(my.df, data.frame(a=sample(letters,
                                            this.N, replace=TRUE), b=runif(this.N)))
}

lsum <- 0
lsum
for(i in 1:length(10)) {
  lsum <- lsum + log(10[i])
}
lsum

lsum <- sum(log(10))
lsum
