library(tidyverse)
library(ggmosaic)  
library(broom)

# OK

happy %>% 
  select_if(is.factor) %>% 
  map(~round(prop.table(table(.x)), 2))

## Better results

data(happy)
View(happy)
summary(happy)
head(happy)
TABLE1A = happy %>%  
  select_if(is.factor) %>% 
  gather()
TABLE1A

TABLE1B = happy %>%  
  select_if(is.factor) %>% 
  gather() %>% 
  filter(!is.na(value)) %>%
  group_by(key, value) %>% 
  summarise(count = n())
TABLE1B

TABLE = happy %>%  
  select_if(is.factor) %>% 
  gather() %>% 
  filter(!is.na(value)) %>%
  group_by(key, value) %>% 
  summarise(count = n()) %>% 
  mutate(fre = count, perc = round(count/sum(count), 2), count = NULL)
TABLE
