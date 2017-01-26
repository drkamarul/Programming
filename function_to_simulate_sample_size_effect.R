### Function to simulate the effect of 
##  increasing sample size

test_nnn <- function(nnn, mean_nnn, sd_nnn) {
  print(nnn)
  par(mfrow = c(3,2))
  lapply(nnn, function(x) {
    hist(rnorm(x, mean = mean_nnn, sd =sd_nnn),
         main = paste("sample size=",x),
         xlab = paste("mean=",mean_nnn,"sd=",sd_nnn))
    
  })
  par(mfrow = c(1,1))
}

nnn <- c(10,20,30, 40, 100, 1000)
mean_nnn <- 100
sd_nnn <- 2

test_nnn(nnn, mean_nnn , sd_nnn )
