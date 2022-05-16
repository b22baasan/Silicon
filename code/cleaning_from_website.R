# author: bbaasan
# Date: May 9, 22
# version: 0.1
# Purpose: cleaning fabs and foundries using rvest

#
#test_main <- main %>% 
#  full_join(dioxide, by = 'CountryCode') %>% 
#  full_join(primary, by = 'CountryCode') %>% 
#  full_join(wafers, by = 'CountryCode') %>% 
#  mutate(region = as.factor(region)) %>% 
#  mutate(income = as.factor(income)) %>% 
#  drop_na()
#
#test_main %>% 
#  ggplot(aes(x=sd_qty, y=sd_value, color = income, fill=income))+
#  geom_point(alpha=.9)
#
#list.files('../raw_data/fabs_foundries')

library(tidyverse)
library(rvest)
library(dplyr)

col_link <- "https://en.wikipedia.org/wiki/List_of_semiconductor_fabrication_plants"

col_page <- read_html(col_link)

myfabs <- col_page %>% html_nodes("table#opensemifabs") %>% 
  html_table() %>% .[[1]]

fabs <- myfabs %>% 
  rename(company='Company', plant_name = 'Plant name',
         plant_loc = 'Plant location', plant_cost = 'Plant cost (in US$ billions)',
         start_prod = 'Started production', wf_size_mm = 'Wafer size (mm)',
         tech_node_nm = 'Process technology node (nm)',
         prod_cap_mnth = 'Production capacity (Wafers/Month)', 
         tech_prod = 'Technology / products') %>% 
  mutate(CountryCode = str_split_fixed(plant_loc, ',', n=2)[,1]) %>% 
  mutate(CountryCode = ifelse(CountryCode %in% 
                                c('Singapore', 'Singapore[1]', 'Singapore[26]'), 
                              'SGP', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'Singapore[1]', 'SGP', CountryCode)) %>% 
  mutate(CountryCode = ifelse(CountryCode == 'China', 'CHN', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode %in%
                                c('Japan', 'Japan[126][127]',"Japan[61]"), 
                              'JPN', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'Germany', 'DEU', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'France', 'FRA', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'Thailand', 'THA', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'UK', 'GBR', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'Sweden', 'SWE', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'Russia', 'RUS', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'Belarus', 'BLR', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'Argentina', 'ARG', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'Taiwan', 'TWN', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'Italy', 'ITA', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'Australia', 'AUS', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'Austria', 'AUT', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'Belgium', 'BEL', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'Brazil', 'BRA', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'Canada', 'CAN', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'Costa Rica', 'CRA', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'Czech Republic', 'CZE', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'Netherlands', 'NLD', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'Philippines', 'PHL', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'Malaysia', 'MSY', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'South Korea', 'KOR', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'North Korea', 'PRK', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'Hong Kong[320]', 'HKG', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'Finland', 'FIN', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'Hungary', 'HUN', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'India', 'IND', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'Indonesia', 'IDN', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'Ireland', 'IRL', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'Isreal', 'ISR', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'Mexico', 'MEX', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'Switzerland', 'CHE', CountryCode)) 

fabs %>% group_by(CountryCode) %>% count(tech_prod) %>% arrange(desc(n))
