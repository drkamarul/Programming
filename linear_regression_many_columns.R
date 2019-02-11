# linear regression for many columns

library(purrr)
library(dplyr)
library(broom)

mtcars
mtcars %>% 
  select(mpg, disp, hp) %>%
  map(~lm(.x ~ cyl, data = mtcars)) %>%
  map(summary) %>%
  map_df(tidy)

# https://purrr.tidyverse.org/reference/map.html 

mtcars %>%
  split(.$cyl)

mtcars %>%
  split(.$cyl) %>%
  map(~lm(mpg ~ disp, data = .x))

mtcars %>%
  split(.$cyl) %>%
  map(~lm(mpg ~ disp, data = .x)) %>%
  map(summary) %>%
  map(broom::tidy) 
  
mtcars %>%
  split(.$cyl) %>%
  map(~lm(mpg ~ disp, data = .x)) %>%
  map(summary) %>%
  map_df(broom::tidy) 
