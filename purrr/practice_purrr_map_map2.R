library(tidyverse)
library(gapminder)
library(broom)

glimpse(gapminder)

mod <- map(gapminder, ~ lm(pop ~ .x, data = gapminder)) 

mod2 <- map(mtcars, ~ lm(mpg ~ .x, data = mtcars)) %>%
  map(~summary(.x))
mod2

library(tidyverse)
library(broom)
mod3 <- map(mtcars, ~ lm(mpg ~ .x, data = mtcars)) %>%
  map(~tidy(.x))
mod3

mod4 <- map(mtcars, ~ lm(mpg ~ .x, data = mtcars)) %>%
  map_df(~tidy(.x))
mod4

# below do not work
gender <- as.integer(as.factor((rep(c('male', 'fem'), 10))))-1
gender
m.coef <- rep(0.123, 20)
lin.comb <- map2(gender, m.coef, ~(.x + .y))
lin.comb

lin.comb2 <- map2_dbl(gender, m.coef, ~(.x + .y))
lin.comb2


# less preferred
mydata <- data.frame(gender, m.coef)
mydata %>% mutate(res = gender * m.coef)
