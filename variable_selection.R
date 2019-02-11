# Variable Selection 

## For linear regression
### Package olsrr

library("olsrr")

# perform linear regression

ols_regress(mpg ~ disp + hp + wt + qsec, data = mtcars)
mmodel <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars) 
summary(mmodel)

# residuals
## res vs fitted
ols_plot_resid_fit(mmodel)

## dfbetas
ols_plot_dfbetas(mmodel)

## Plot to detect non-linearity, influential observations and outliers.
ols_plot_resid_spread(mmodel)
ols_plot_resid_fit_spread(mmodel)


# collinearity
ols_coll_diag(mmodel)

# variables selection
ols_step_both_aic(mmodel)
ols_step_both_p(mmodel)

# stepwise AIC backward
ols_step_backward_aic(mmodel)
plot(ols_step_backward_aic(mmodel))

# MASS::stepAIC = stepwise
# Ref: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4842399/ 
# https://www.youtube.com/watch?v=BneY21nS5is 



library(summarytools)
library(MASS) 
head(birthwt)
str(birthwt)

## need to factor some variables
bwt <- with(birthwt, {
  race <- factor(race, labels = c("white", "black", "other"))
  ptd <- factor(ptl > 0)
  ftv <- factor(ftv)
  levels(ftv)[-(1:2)] <- "2+"
  data.frame(low = factor(low), age, lwt, race, smoke = (smoke > 0),
             ptd, ht = (ht > 0), ui = (ui > 0), ftv)
})
str(bwt)

## logistic regression model
fullm <- glm(low ~ ., family = binomial(link = 'logit'), data = bwt)

## summarize model
summary(fullm)

#### stepwise AIC - both forward and backward ####
stepm <- stepAIC(fullm, trace = TRUE)
stepm; broom::tidy(fullm)
stepm$anova

#### stepwise forward ####
stepf <- stepAIC(fullm, direction = 'forward', trace = TRUE)
stepf ; summary(fullm)
stepf$anova

# Because the forward stepwise regression begins with full model, there are no additional variables that can be added

stepb <- stepAIC(fullm, direction = 'backward', trace = TRUE)
stepb ; broom::tidy(fullm)
stepb$anova

#### Reference ####

#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4842399/

#### using step() ####

summary(fullm)
no.age <- update(fullm, .~ . - age)
summary(no.age)

add.age <- update(no.age, .~ . + age)
summary(add.age)

#### using step ####

full.model <- fullm
const.model <- glm(low ~ 1, family = binomial(link = 'logit'), data = bwt)
summary(const.model)

step(const.model, scope = list(upper = full.model, lower = ~1), 
     direction = 'both', trace = TRUE)

step(const.model, scope = list(upper = full.model, lower = ~1), 
     direction = 'forward', trace = TRUE)

step(full.model, direction = 'backward', trace = TRUE)

#### dropterm() and addterm() ####

dropterm(full.model, test = 'Chisq', trace = TRUE)
addterm(const.model, scope = full.model, test = 'Chisq', trace = TRUE)
