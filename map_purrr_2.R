a <- rnorm(100)
b <- rnorm(100)
c <- rnorm(100)
d <- rnorm(100)
df <- tibble(
  a,b,c,d
)
df
map(df, ~ mean(.x))
df %>% map(~ mean(.x))

# map(df, ~ lm(a ~ .x, data = .))?
# lm(a ~ b, data = df)?

# https://sebastiansauer.github.io/EDIT-multiple_lm_purrr_EDIT/

library(purrr)  
library(ggplot2)
library(dplyr)
library(broom)
library(knitr)  # for kable
data(Fair, package = "Ecdat") # extramarital affairs dataset
glimpse(Fair)
head(Fair)

# workflow
# oc = nbaffairs


result_lm <- summary(lm(nbaffairs ~ age, data = Fair))
result_lm
names(result_lm) #pay attention to r.squared

map(Fair, ~ lm(nbaffairs ~ .x, data = Fair))

Fair %>% 
  map(~ lm(nbaffairs ~ .x, data = Fair)) %>%
  map(summary)

Fair %>% 
  map(~ lm(nbaffairs ~ .x, data = Fair)) %>%
  map(confint)
  
Fair %>% 
  map(~ lm(nbaffairs ~ .x, data = Fair)) %>%
  map(summary) %>%
  map("coefficients")

Fair %>% 
  map(~ lm(nbaffairs ~ .x, data = Fair)) %>%
  map(summary) %>%
  map(tidy)

Fair %>%
  dplyr::select(-nbaffairs) %>%
  map(~ lm(nbaffairs ~ .x, data = Fair)) %>%
  map(summary) %>%
  map_dbl("r.squared") 

# sort r squared

Fair %>%
  dplyr::select(-nbaffairs) %>%
  map(~ lm(nbaffairs ~ .x, data = Fair)) %>%
  map(summary) %>%
  map_dbl("r.squared") %>% 
  map(tidy)

Fair %>%
  dplyr::select(-nbaffairs) %>%
  map(~ lm(nbaffairs ~ .x, data = Fair)) %>%
  map(summary) %>%
  map_dbl("r.squared") %>% 
  tidy

Fair %>%
  dplyr::select(-nbaffairs) %>%
  map(~ lm(nbaffairs ~ .x, data = Fair)) %>%
  map(summary) %>%
  map_dbl("r.squared") %>% 
  tidy %>%
  dplyr::arrange(desc(x)) %>% 
  rename(uni_r_squared = x) -> r2s 


# VS dplyr::arrange(desc(x))
kable(r2s) 


# improved methods
head(Fair)

Fair %>%
  dplyr::select(-nbaffairs) %>%
  map(~ lm(nbaffairs ~ .x, data = Fair)) %>%
  map(summary) %>%
  map(broom::tidy) %>%
  map_df("p.value") %>% #into data frame
  round(3) %>%
  mutate(variable = c("intercept", "predictor"))
  

Fair %>%
  dplyr::select(-nbaffairs) %>%
  map_df(~ glance(lm(nbaffairs ~ .x, data = Fair))[c(4,1,5)]) %>%
  mutate(covariate = names(Fair)[1:8])
  
Fair %>%
  dplyr::select(-nbaffairs) %>%
  map_df(~ glance(lm(nbaffairs ~ .x, data = Fair))[c(4,1,5)])  %>%
  map_df(broom::tidy) 

Fair %>%
  dplyr::select(-nbaffairs) %>%
  map(~ (lm(nbaffairs ~ .x, data = Fair))) %>%
  map_df(confint) 

test_res <- lm(nbaffairs ~ sex, data = Fair)
summary(test_res)
confint(test_res)
names(test_res)
