# simulation - effect on sample size on poisson distribution

library(tidyverse)
mypop <-  rpois(n = 2000, lambda = 4)
hist(mypop)
sample_size <- 1:2000 

sample(mypop, 20)


mydist <- map(sample_size, ~ sample(mypop, .x))
head(mydist)

par(mfrow = c(2,5))
myhist <- map(mydist[seq(1,900, by =90)], ~ hist(.x))
par(mfrow = c(1,1))
