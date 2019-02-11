myselect <- function(mydata, myvar) {
  mydata <- dplyr::select(mydata,myvar)
  mydata
}
myselect(mtcars, c("mpg", "disp"))
myselect(mtcars, c(mpg, disp)) # error
c("mpg", "disp")

# error
myselect2 <- function(mydata, myvar) {
  my_var <- enquo(myvar)
  mydata <- dplyr::select(mydata, !!myvar)
  mydata
}
myselect2(mtcars, c(mpg, disp)) # erro

quo(c("mpg", "disp"))
glue("mpg","," ,"disp")


library(dplyr)
mysummary <- function(mydata, myvar) {
  dplyr::select(mydata,myvar) %>% purrr::map_dbl(mean)
}
mysummary(mtcars, c("mpg", "disp"))


library(dplyr)
mysummary2 <- function(mydata, myvar) {
  mydata.a <- dplyr::select(mydata,myvar)
  sd.a <- mydata.a %>% purrr::map_dbl(sd)
  print(paste0("SD")) ; print(sd.a)
  mean.a <- mydata.a %>% purrr::map_dbl(mean)
  print(paste0("Mean")) ; print(mean.a)
  bind_rows(mean.a, sd.a)
}
mysummary2(mtcars, c("mpg", "disp")) # does not work

bind_rows(mean.a, sd.a)

# using glue::glue
library(dplyr)
library(glue)
mysummary2 <- function(mydata, myvar) {
  dplyr::select(mydata,glue::glue({myvar})) %>% purrr::map_dbl(mean)
}

mysummary2(mtcars, "mpg")
mysummary2(mtcars, c("mpg", "disp"))  # This does not work

myglue <- function(oc,  iv){
  print(glue::glue({oc}, " ~ ", {iv}))
}
myglue("mpg", "disp")

#mutate(df1, y = a + x)
#mutate(df2, y = a + x)
#mutate(df3, y = a + x)
#mutate(df4, y = a + x)

mutate_y <- function(df) {
  mutate(df, y = a + x)
}

df1 <- tibble(x = 1:3)
df1
a <- 10
mutate_y(df1)

df <- tibble(
  g1 = c(1, 1, 2, 2, 2),
  g2 = c(1, 2, 1, 2, 1),
  a = sample(5),
  b = sample(5)
)
df

df %>%
  group_by(g1) %>%
  summarise(a = mean(a))
df %>%
  group_by(g2) %>%
  summarise(a = mean(a))

#error

group_sum <- function(mydata, mygroups){
  mydata %>%
    group_by(mygroups) %>%
    summarise(mean_a = mean(a))
}

# also error
group_sum2 <- function(mydata, mygroups){
  mydata %>%
    group_by(glue::glue({mygroups})) %>%
    summarise(mean_a = mean(a))
}
group_sum(df, g1) #error
group_sum(df, "g1") #error
group_sum2(df, "g1") #error


quo(g1)
quo(a + b + c)
quo("a")

group_sum(df, quo(g1))

# so use !!
group_sum2 <- function(mydata, mygroup){
  mydata %>%
    group_by(!!mygroup) %>%
    summarise(mean_a = mean(a))
}
df
group_sum2(df, quo(g1)) #use quo() to remove " "
group_sum2(df, "g1") #works
group_sum2(df, g1) #error in quos(), need to put "g1"

# compared to 
group_sum(df, quo(g1))
group_sum(df, "g1")
group_sum(df, g1)

# need remove the " " due to 'mygroups' is unknown 
# use quo
quo(g1)

group_sum3 <- function(mydata, mygroup){
  quo_my_group <- quo(mygroup)
  print(quo_my_group)

  mydata %>%
    group_by(!!quo_my_group) %>%
    summarise(mean_a = mean(a))
}
df
group_sum3(df, g1) #still error

#enquo
group_sum4 <- function(mydata, mygroup){
  quo_my_group <- enquo(mygroup)
  print(quo_my_group)

  mydata %>%
    group_by(!!quo_my_group) %>%
    summarise(mean_a = mean(a))
}
df
group_sum4(df, g1) # works

#
group_sum5 <- function(mydata, mygroup, myvars){
  quo_my_group <- enquo(mygroup)
  print(quo_my_group)

  mydata %>%
    select(myvars) %>%
    group_by(!!quo_my_group) %>%
    purrr::map(mean)
}
df
group_sum5(mydata = mtcars, mygroup = am, myvars = c("mpg", "disp")) # does not work
