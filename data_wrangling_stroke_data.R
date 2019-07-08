# Ref : https://suzan.rbind.io/2018/02/dplyr-tutorial-2/#mutate-if

# load library
library(haven)
library(tidyverse)
library(lubridate)
library(here)



# read data
stroke <- read_dta('fatalityData18April2014.dta')
stroke <- as_tibble(stroke)
class(stroke)
glimpse(stroke)
str(stroke)
dim(stroke)

# convert labelled var to factor
stroke <- stroke %>% mutate_if(is.labelled, funs(as_factor))
glimpse(stroke)

# select vars
stroke <- stroke %>% select(rn, doa, dod, status3b, sex2, icd10cat3, referral2cat, dm2, gcs, sbp, dbp, wbc)
summary(stroke)

# replace NA for dm2 with no
stroke <- stroke %>% replace_na(list(dm2 = 'no'))
# drop NA from gcs, sbp, dbp, wbc
stroke <- stroke %>% drop_na(gcs, sbp, dbp, wbc)
summary(stroke)

# days in ward
stroke <- stroke %>% mutate(time = interval(doa,dod), time2 = time/ddays(1))
summary(stroke)

# summary by status
stroke %>% group_by(status3b) %>% summarise_if(is.numeric, mean, na.rm = T)
stroke %>% group_by(status3b, sex2) %>% summarise(n = n())
stroke %>% group_by(status3b, icd10cat3) %>% summarise(n = n())
stroke %>% group_by(status3b, referral2cat) %>% summarise(n = n())

# rename
stroke <- stroke %>% rename(id = rn,  status = status3b, sex = sex2, str_type = icd10cat3, referral = referral2cat, dm = dm2)

# create a new dataset
# recode cat var
glimpse(stroke)
stroke2 <- stroke
glimpse(stroke2)

stroke2 %>% select(str_type, referral) %>% map(.,levels)
stroke2 <- stroke %>% mutate(str_type2 = fct_recode(str_type, 
                                                     'IS' = 'CI,Others', 
                                                     'HS' = 'SAH', 
                                                     'HS' = 'ICB, Other Haemorrhage'),
                             referral2 = fct_recode(referral,
                                                    'non-hospital' = 'GP/Home/Missing'))
stroke2 %>% select(str_type2, referral2) %>% map(.,levels)

# final stroke data

stroke_dat <- stroke2 %>% select(-(c(time, str_type)))
stroke_dat
# write_csv(stroke_dat, here::here('data','stroke_data.csv'))

# EDA

stroke_dat %>% group_by(status) %>% 
  summarise_at(vars(time2, gcs, sbp, dbp), funs(mean, min, max, sd))

