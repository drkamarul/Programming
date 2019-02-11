### THIS WORKS OK MULTIPLE HISTOGRAMS!!!

library(dplyr)
library(purrr)
library(ggplot2)
amputee2 %>% select(age, AMNOPRO, AMPRO) %>%
  names() %>%
  map(~ggplot(amputee2, aes_string(x = .)) + 
        geom_histogram(aes(y = ..density..),colour = 'black', fill = 'white') +
        geom_density(aes_string(x = .), alpha = 0.2, fill = "#FF6666"))

ggplot(mtcars, aes(disp)) + 
  geom_histogram(colour = 'black', fill = 'white') +
  geom_density(alpha = 0.2, fill = "#FF6666")
  
amputee2 %>% select(age, years_dx_dm, AMNOPRO, AMPRO) %>% 
  map(~hist(.x)) %>% map(~names)

### THIS WORK FOR MULTIPLE BAR CHARTS

dat <- read.table(text = "sample Types Number
sample1 A   3641
                  sample2 A   3119
                  sample1 B   15815
                  sample2 B   12334
                  sample1 C   2706
                  sample2 C   3147", header=TRUE)
dat2 <- read.table(text = "sample Types Number
sample1 A   3000
                  sample2 A   3500
                  sample1 B   15000
                  sample2 B   18000
                  sample1 C   15000
                  sample2 C   17000", header=TRUE)

dat1 <- data.frame(dat)
dat3 <- data.frame(dat2)
list.dat <- list(dat1, dat3)  
map(list.dat, ~ggplot(data=.x, aes(x=Types, y=Number, fill=sample)) + 
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label=Number), position=position_dodge(width=0.9), vjust=-0.25))

#############################

library(purrr)
library(summarytools)
amputee2 %>% select_if(is.factor) %>% map(~freq(.x, style = 'rmarkdown')) 

library(dplyr)
library(purrr)
library(ggplot2)
data %>% select(var1, var2) %>%
  map(~ggplot(data, aes(.x)) + 
        geom_histogram(aes(y = ..density..), colour = 'black', fill = 'white') +
        geom_density(alpha = 0.2, fill = "#FF6666"))


library(tidyr)
mtcars %>%
  gather(-mpg,key = "var", value = "value") %>%
  ggplot(aes(x = value, y = mpg)) +
  geom_point() +
  stat_smooth() +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

mtcars %>% 
  select(wt, disp, hp) %>% 
  names() %>%
  map(~ggplot(mtcars, aes_string(x = .)) + geom_histogram())

# BOX PLOTS

library(dplyr)
library(purrr)
library(summarytools)
library(ggplot2)
mydata <- tibble(male = factor(rbinom(n = 50, size = 1, prob = 0.5)),
                 status = factor(rbinom(n = 50, size = 1, prob = 0.3)),
                 stress = factor(rbinom(n =50, size = 1, prob = 0.2)))
mydata
ggplot(mydata, aes(male)) + geom_bar(stat = 'count')
ggplot(mydata, aes(status)) + geom_bar(stat = 'count')
ggplot(mydata, aes(stress)) + geom_bar(stat = 'count')

mydata %>% select(male, status, stress) %>%
  names() %>%
  map(~ggplot(mydata, aes_string(x = .)) + geom_bar())

# refer https://stackoverflow.com/questions/12018499/how-to-put-labels-over-geom-bar-for-each-bar-in-r-with-ggplot2?noredirect=1&lq=1 
mydata %>% select(male, status, stress) %>%
  names() %>%
  map(~ggplot(mydata, aes_string(x = .)) + geom_bar(stat = 'count'))


mydata %>% select(male) %>% count(male)
mydata %>% select(male) %>% group_by(male) %>% summarise(freq.gp = n())
mydata %>% select(male, status) %>% map(~summarise(freq.gp = n()))

# analysis tools


