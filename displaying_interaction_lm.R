iv <- rnorm(n = 100, mean = 30, sd = 5)
sex <- rbinom(n = 100, size = 1, prob = 0.5)
error <- rnorm(n = 100, mean = 0, sd = 5) 
dv <- 5 - 0.85*iv + 1.5*sex + 0.6*iv*sex + error 
mydata <- data.frame(dv, iv, sex)
head(mydata)

#lm model
mymod <- lm(dv ~ iv + sex + iv:sex, data = mydata)
summary(mymod)

#better output
library(broom)
tidy(mymod)

mymodel <- augment(mymod)
head(mymodel)

#plot
library(tidyverse)
mymodel %>% ggplot(aes(x = iv, y = dv)) + 
  geom_point()

mymodel %>% ggplot(aes(x = iv, y = dv)) + 
  geom_point() +
  geom_smooth(method = 'lm')

mymodel %>% ggplot(aes(x = iv, y = dv, colour = factor(sex))) + 
  geom_point() +
  geom_smooth(method = 'lm')


