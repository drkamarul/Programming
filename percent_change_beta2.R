
change_beta <- function(mod1, mod2, yourdata) {
mod1 <- lm(mod1, data = yourdata)
mod2 <- lm(mod2, data = yourdata)

library(broom)
mod1_t <- tidy(mod1)
mod1_t$estimate
mod2_t <- tidy(mod2)
mod2_t$estimate

cbind(mod1_t$estimate, mod2_t$estimate)

library(dplyr)
mod1_2 <- left_join(mod1_t, mod2_t, by = 'term')
print(mod1_2)

mod1_t$estimate
mod2_t$estimate
mod1_2$estimate.x - mod1_2$estimate.y
(mod1_2$estimate.x - mod1_2$estimate.y) / 100 

tab1 <- cbind(mod1_t$estimate,
              mod2_t$estimate,
              mod1_2$estimate.x - mod1_2$estimate.y,
              (mod1_2$estimate.x - mod1_2$estimate.y) / 100 )

tab1 

colnames(tab1) <- c('beta_model_1', 'beta_model_2', 'beta_change', "percent change")
tab1
rownames(tab1) <- mod1_t$term
tab1
print(round(tab1, digits = 2))
}

change_beta(mpg ~ disp + hp, mpg ~ hp, mtcars)
