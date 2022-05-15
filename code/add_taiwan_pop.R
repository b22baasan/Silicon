# author: bbaasan
# Date: May 11, 2022
# version:
# Purpose: Adding Taiwan population in the world

library(tidyverse)

pop <- read_csv('raw_data/population/pop.csv')
taiwan <- read_csv('raw_data/population/taiwan.csv')

#population <- 
pop %>% select(-c(`Indicator Name`,`Indicator Code`)) %>% 
  full_join(taiwan) %>% drop_na() %>% 
  write.csv("processed_data/pop.csv")

