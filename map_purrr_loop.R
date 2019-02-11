
#### Intro ####

# ref = https://jennybc.github.io/purrr-tutorial/bk00_vectors-and-lists.html


v_log <- c(TRUE, FALSE, FALSE, TRUE)
v_log
v_int <- 1:4
v_int


v_doub <- 1:4 * 1.2
v_doub

v_doub[2:3]

typeof(v_doub[100])

exp(v_doub)

l_doub <- as.list(v_doub)
exp(l_doub)

library(purrr)
l_doub

typeof(l_doub)

map(l_doub, exp) # "mapping the function exp() over the list  l_doub

library(repurrrsive)

typeof(got_chars)
got_chars[1:20]
got_chars

map(got_chars[1:3], "name")

map_chr(got_chars[1:6], "name")

map_df(got_chars[23:25],
       `[`, c("name", "playedBy"))

tibble::tibble(
  name = map_chr(got_chars[23:25], "name"), 
  id = map_int(got_chars[23:25], "id")
     )

tibble::tibble(
  name = map_chr(got_chars[23:25], "name"),
  id = map_int(got_chars[23:25], "id")
     )


nms <- got_chars[16:18] %>% map_chr('name') 
birth <- got_chars[16:18] %>% map_chr('born')
nms ; birth
str(nms)
str(birth)

map2(nms, birth, ~ paste(.x, "was born", .y))

df0 <- tibble::tibble(
  nms,
  birth
)

df0

df <- tibble::tibble(
  nms,
  connector = 'was born',
  birth
)
df

pmap(df, paste)

library(dplyr)
library(gapminder)
minigap <- gapminder %>% filter(country == "Germany" | country == "Canada",
                                year > 2000) %>% droplevels()
minigap

minigap %>% group_by(country) %>% summarize(lifeExp = mean(lifeExp))
minigap %>% group_by(country) %>%
  summarize_at(vars(lifeExp, gdpPercap), mean)

library(tidyr)
nested_df <- gapminder %>% 
  group_by(country, continent) %>%
  nest() %>%
  mutate(fit = map(data, ~ lm(lifeExp ~ year, data = .x)))


str(nested_df$fit[1:2], max.level = 1)
nested_df$fit[[1]]

nested_df %>% 
  mutate(coefs = map(fit, coef),
         intercept = map_dbl(coefs, 1),
         slope = map_dbl(coefs, 2)) %>%
  select(country, continent, intercept, slope)


#### practice 1 ####

output <- vector('double', ncol(mtcars))
output
for (i in seq_along(mtcars)) {
  output[[i]] <- mean(mtcars[[i]])
}
  results <- data.frame(var_mtcars = colnames(mtcars),output)
  print(results)

col11 <- vector('character', ncol(nycflights13::flights))
col22 <- vector('integer', ncol(nycflights13::flights))
data_op <- data.frame(col11, col22)
data_op
  for (i in seq_along(nycflights13::flights)) {
    output[[i]] <- mean(mtcars[[i]])
  }
  results <- data.frame(var_mtcars = colnames(mtcars),output)
  print(results)

  
  #### practice 2 ####
  
  library(purrr)
  library(repurrrsive)
  library(listviewer)
  listviewer::jsonedit(got_chars, mode = "view")  
  str(wesanderson)
  listview(wesanderson)
  
  
  str(got_chars, list.len = 3)
  str(gh_users, max.level = 1)
  
  # practice 3
  (3:5)^2
  sqrt(c(9, 16, 25))
    map(c(9, 16, 25), sqrt)
  
    map(got_chars[1:4], 'name')
    map(got_chars[5:8], 3)
    
    
    got_chars %>% map('name')
    got_chars %>% map(3)    
    
    
