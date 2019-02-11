# https://emoriebeck.github.io/R-tutorials/purrr/#

library(psych)
library(knitr)
library(kableExtra)
library(gridExtra)
library(plyr)
library(tidyverse)

for (i in letters[1:5]) {
  print(i)
}

ipip50 <- read.csv("https://raw.githubusercontent.com/emoriebeck/R-tutorials/master/purrr/ipip50_sample.csv", 
                   stringsAsFactors = F)
head(ipip50)
class(ipip50)

# Convert data frame to tibble
ipip502 <- as_tibble(ipip50)
ipip502

df <- expand.grid(
  Trait = c("E", "A", "C", "N", "O"),
  Outcome = c("BMI", "logMediInc", "exer"),
  stringsAsFactors = F
) %>%
  tbl_df

df

ipip50_composites <- ipip50 %>%
  gather(key = item, value = value, A_1:O_10)
head(ipip50_composites)

ipip50_composites2 <- ipip50_composites %>% separate(item, c("Trait", "item"), sep = "_") 
head(ipip50_composites2)

ipip50_composites3 <- ipip50_composites2 %>% 
  group_by(RID, gender, age, BMI, exer, logMedInc, Trait) %>%
  summarise(t.value = mean(value, na.rm = T))
head(ipip50_composites3, 15)


ipip50_composites4 <- ipip50_composites3 %>%
  gather(key = outcome, value = o.value, BMI:logMedInc) 
head(ipip50_composites4)

ipip50_nested <- ipip50_composites4 %>% 
  group_by(Trait, outcome) %>%
  nest()
ipip50_nested
# There are 15 rows based on Trait and outcome
ipip50_nested$data[1:3]

# What we want to do is to run a model using personality to predict our outcomes for each combination of trait and outcome
# Now that we have a data frame for each nested in a data frame

lm(o.value ~ t.value, data = ipip50_nested$data[[1]])
lm(o.value ~ t.value, data = ipip50_nested$data[[2]])
lm(o.value ~ t.value, data = ipip50_nested$data[[3]])

# More intelligently
ipip50_nested
ipip50_nested2 <- ipip50_nested %>%
  mutate(model = map(data, ~lm(o.value ~ t.value, data = .))) ## error here , why?
