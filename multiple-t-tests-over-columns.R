
# iterate t test over multiple columsn

#### Data ####

set.seed(333)
a <- rnorm(20, 10, 1)
b <- rnorm(20, 15, 2)
c <- rnorm(20, 20, 3)
grp <- rep(c('m', 'y'),10)
test_data <- data.frame(a, b, c, grp)

test_data

#### Method 1 ####

library(tidyverse)

test_data %>% 
  select_if(is.numeric) %>%
  map_df(~ broom::tidy(t.test(. ~ grp)))

#### Method 2 ####

formulas <- paste(names(test_data)[1:(ncol(test_data)-1)], "~ grp")

output <- t(sapply(formulas, function(f) {      
  res <- t.test(as.formula(f))
  c(res$estimate, p.value=res$p.value)      
}))

output
