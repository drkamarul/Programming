# https://towardsdatascience.com/functional-programming-in-r-with-purrr-469e597d0229

library(tidyverse)
mtcars %>% 
  group_by(cyl) %>%
  summarise(mean_mpg = mean(mpg))

# the slow way
aov_mpg <- aov(mpg ~ factor(cyl), data = mtcars)
summary(aov_mpg)
aov_disp <- aov(disp ~ factor(cyl), data = mtcars)
summary(aov_disp)
aov_hp <- aov(hp ~ factor(cyl), data = mtcars)
summry(aov_hpp)


r_sq0 <- mtcars %>%
  filter(cyl == 4) %>%
  lm(mpg ~ wt, data = .)
r_sq0

r_sq01 <- mtcars %>%
  filter(cyl == 4) %>%
  lm(mpg ~ wt, data = .) %>%
  summary()
r_sq01
names(r_sq01)

r_sq012 <- mtcars %>%
  filter(cyl == 4) %>%
  lm(mpg ~ wt, data = .) %>%
  summary() %>%
  .$"r.squared"
r_sq012

r_sq <- mtcars %>% 
  filter(cyl == 4) %>%
  lm(mpg ~ wt, data = .) %>%
  summary() %>%
  .$"r.squared"
r_sq

# .x = A list or atomic vector (logical, integer, double/numeric, and character)
# .f = A function, formula, or atomic vector

mymod <- lm(mpg ~ wt, data = mtcars)
summary(mymod)
summary(mymod)$"r.squared"
names(mymod)

# remember mtcars$cyl
# remember summary$
mtcars %>%
  split(.$cyl) %>%
  map(~ lm(mpg ~ wt, data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared")

mtcars %>%
  split(.$cyl) %>%
  map(~ lm(mpg ~ wt, data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared")

mtcars %>%
  split(.$cyl) %>%
  map(~ lm(mpg ~ wt, data = .)) %>%
  map(summary) 

library(broom)
mtcars %>%
  split(.$cyl) %>%
  map(~ lm(mpg ~ wt, data = .)) %>%
  map_df(tidy) 

# You could also use map_dfr which binds the outputs into rows of a dataframe.
mtcars %>%
  split(.$cyl) %>%
  map(~ lm(mpg ~ wt, data = .)) %>%
  map(summary) 

mtcars %>%
  split(.$cyl) %>%
  map(~ lm(mpg ~ wt, data = .)) %>%
  map(summary) %>%
  map_dfr("r.squared")

mtcars %>%
  split(.$cyl) %>%
  map(~ lm(mpg ~ wt, data = .)) %>%
  map(summary) %>%
  map_dfc("coefficients")

# over columns

mtcars1 <- mtcars %>%
  mutate(f.cyl = factor(cyl), f.am = factor(am)) %>% 
  select(mpg, disp, hp)
mtcars1

mtcars1 %>% 
  map(~ lm(.x ~ cyl * am, data = mtcars))

mtcars1 %>% 
  map(~ lm(.x ~ cyl * am, data = mtcars)) %>%
  map_dfr(~ broom::tidy(.), .id = 'source')

mtcars1 %>% 
  map(~ lm(.x ~ cyl * am, data = mtcars)) %>%
  map_dfc(~ broom::tidy(.), .id = 'source')

mtcars %>%
  mutate(cyl = factor(cyl),
         am = factor(am)) %>%
  select(mpg, disp, hp) %>%
  map(~ lm(.x ~ cyl * am, data = mtcars)) %>%
  map_dfr(~ broom::tidy(.), .id = 'source') %>%
  mutate(p.value = round(p.value, 5))

# mtcars %>%
  mutate(cyl = factor(cyl),
         am = factor(am)) %>%
  select(mpg, disp, hp) %>%
  map(~ lm(.x ~ cyl * am, data = mtcars)) %>%
  map_dfr(~ broom::tidy(), .id = 'source') %>% ### error if only tidy()
  mutate(p.value = round(p.value, 5))

#  mtcars %>%
    mutate(cyl = factor(cyl),
           am = factor(am)) %>%
    select(mpg, disp, hp) %>%
    map(~ lm(.x ~ cyl * am, data = mtcars)) %>%
    map_dfr(~ broom::tidy, .id = 'source') %>%  ### error if tidy
    mutate(p.value = round(p.value, 5))
  