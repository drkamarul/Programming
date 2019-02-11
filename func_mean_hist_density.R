library(dplyr)
library(ggplot2)

# function to calculate mean, plot histogram and density
mymean_hist <- function(mydata, iv, grp) {
iv <- enquo(iv)
grp <- enquo(grp)
# calc mean
mydesc <- mydata %>%
  summarize(mean = mean(!!iv, na.rm = TRUE),
            sd = sd(!!iv, na.rm = TRUE),
            min = min(!!iv, na.rm = TRUE),
            max = max(!!iv, na.rm = TRUE))
print(mydesc)
#plot histogram
myhist_den <- mydata %>% 
  ggplot(aes(x = !!iv, fill = !!grp)) +
  geom_histogram() + geom_density()
print(myhist_den)

#plot density
myfreq <- mydata %>% 
  ggplot(aes(x = !!iv, stat(density), colour = !!grp)) +
  geom_freqpoly()
print(myfreq)
}

mymean_hist(mtcars, mpg)
mymean_hist(diamonds, price)
mymean_hist(diamonds, price, cut)
