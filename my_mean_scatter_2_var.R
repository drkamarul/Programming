library(ggplot2)
library(dplyr)
my_mean_plot2 <- function(mydata, var1, var2) {
  var1 <- enquo(var1)
  var2 <- enquo(var2)
  myplot <- ggplot(mydata, aes(!!var1,!!var2)) + geom_point()
  print(myplot)
  mymean <- mydata %>% summarise(meanvar1 = mean(!!var1), 
                                 meanvar2 = mean(!!var2))
  mymean
}

paste_name <- function(var1) {
  var1 <- enquo(var1)
  print(paste0("this is", "=", var1))
}
paste_name(kim)
