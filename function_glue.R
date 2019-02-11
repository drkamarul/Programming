mylm <- function(mydata , myform){
  myform1 <- glue::glue({myform})
  print(myform1)
  summary(lm(myform, data = mydata))
}
mylm(mydata = mtcars, "mpg ~ disp")

mylm2 <- function(mydata , myform){
  myform2 <- as.formula(myform)
  print(myform2)
  summary(lm(myform2, mydata))
}
mylm2(mtcars, mpg ~ disp)

