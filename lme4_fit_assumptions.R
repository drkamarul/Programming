# ref : https://cran.r-project.org/web/packages/lme4/vignettes/lmer.pdf
# practice lme4

library(lme4)
str(sleepstudy)
head(sleepstudy, 15)

# summary of data
summary(sleepstudy)
table(sleepstudy$Subject)

# plot
library(lattice)
xyplot(Reaction ~ Days | Subject, data = sleepstudy)
library(ggplot2)
ggplot(data = sleepstudy, aes( x = Days, y = Reaction)) + geom_point() + facet_wrap( ~ Subject) +
  stat_smooth(method = 'lm')

# fitting RS + RI
fm1 <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
summary(fm1)

# the results of model fm1 of the sleepstudy data (Section 1.2) using summary(fm1), one would
# see that the estimated correlation between the slope for Days and the intercept is fairly low
# (0.066) (See Section 5.2.2 below for more on how to extract the random-effects covariance
# matrix.) We may use double-bar notation to 
# NOW, fit a model that excludes a correlation parameter:

fm2 <- lmer(Reaction ~ Days + (Days || Subject), data = sleepstudy)
summary(fm2)

# get covariance matrix

RX <- getME(fm1, "RX")
sigma2 <- sigma(fm1)^2
sigma2 * chol2inv(RX)

# or 
vcov(fm1)

# update model
fm3 <- update(fm1, . ~ . - (Days | Subject) + (1 | Subject))
summary(fm3)

# get residuals
qres <- quantile(residuals(fm1, "pearson", scaled = TRUE))
qres

#
as.data.frame(VarCorr(fm1))

# obtain fixed effet and random effect
fixef(fm1)
ranef(fm1)

# diagnostics

 # lme4 provides tools for generating most of the standard graphical 
# diagnostic plots (with the exception of those incorporating influence measures, 
# e.g., Cook's distance and leverage plots), in a way modeled on the diagnostic graphics of nlme

#  standard fitted vs. residual plots
plot(fm1, type = c('p', 'smooth'))
 
# sclae-location plot
plot(fm1, sqrt(abs(resid(.))) ~ fitted(.), type = c("p", "smooth"))

# quantile-quantile plots (from lattice),
qqmath(fm1, id = 0.05)

# Finally, posterior predictive simulation (Gelman and Hill 2006) 
iqrvec <- sapply(simulate(fm1, 1000), IQR)
obsval <- IQR(sleepstudy$Reaction)
post.pred.p <- mean(obsval >= c(obsval, iqrvec))
post.pred.p # 0.7902 - The (one-tailed) posterior predictive p value of 0.78 
            # indicates that the model represents the data adequately, 
            # at least for this summary statistic.

# model comparison
anova(fm1, fm2, fm3)

# p-value
# see help

# confint
confint(fm1)
# lme4 provides confidence intervals (using confint) via
# Wald approximations (for fixed-effect parameters only), 
