# author: bbaasan
# Date:
# version:
# Purpose: cleaning and extracting global fabs and foundries 


library(tidyverse)

# market cap for largest semiconductor companies
list.files('../raw_data/')

# world top eight foundries and its marketcap 
top_foundries <- read_csv('../raw_data/LargeCompaniesMarketCap.csv')[-1, ] %>% 
  rename(rank = 1, name = 2, symbol = 3, marketcap=4, price=5, country = 6) %>% 
  mutate(CountryCode = country) %>% 
  mutate(CountryCode = ifelse(CountryCode == 'United States', 'USA', CountryCode)) %>% 
  mutate(CountryCode = ifelse(CountryCode == 'Taiwan', 'TWN', CountryCode)) %>% 
  mutate(CountryCode = ifelse(CountryCode == 'South Korea', 'KOR', CountryCode)) %>% 
  mutate(CountryCode = ifelse(CountryCode == 'Netherlands', 'NED', CountryCode)) %>% 
  mutate(CountryCode = ifelse(CountryCode == 'Japan', 'JPN', CountryCode)) %>% 
  mutate(CountryCode = ifelse(CountryCode == 'Germany', 'DEU', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'Switzerland', 'CHE', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'China', 'CHN', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'Germany', 'DEU', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'France', 'FRA', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'Hong Kong', 'HKG', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'Norway', 'NOR', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'Singapore', 'SGP', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'United Kingdom', 'GRB', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'Australia', 'AUS', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'Ireland', 'IRL', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'Israel', 'ISR', CountryCode)) %>%
  mutate(CountryCode = ifelse(CountryCode == 'Luxembourg', 'LUX', CountryCode)) 


top_foundries_dist <- top_foundries %>% 
  mutate(marketcap = as.numeric(marketcap)) %>% 
  mutate(price = as.numeric(price)) %>% 
  count(CountryCode) %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(x=reorder(CountryCode, -n), y = n)) +
  geom_bar(stat='identity')+
  labs(title = 'Largest Semiconductor Companies by Market Cap')+
  xlab('Country') + ylab('Total Count of Semiconductor Companies')

#final <- 
updated_main <- year2017 %>% 
  mutate(type = CountryCode) %>% 
  #select(-c(CountryName, CountryCode)) %>% 
  mutate(type = ifelse(type %in% c('CHN', 'TWN', 'KOR'), 'fab', 'foundry')) %>% 
  mutate(type = as.factor(type)) 

companies_cap <- top_foundries %>% 
  mutate(marketcap = as.numeric(marketcap)) %>% 
  group_by(CountryCode) %>% 
  summarise(cap = sum(marketcap)) 

  
num_companies <- top_foundries %>% 
  mutate(marketcap = as.numeric(marketcap)) %>% 
  mutate(price = as.numeric(price)) %>% 
  count(CountryCode)

final <- updated_main %>% 
  full_join(companies_cap, by = 'CountryCode') %>% 
  full_join(num_companies, by='CountryCode') %>% 
  rename(num_firms = n)

final.table <- final %>% group_by(type) %>% summarise(n=sum(w_qty)) %>% 
  drop_na()

final.table %>% 
  mutate(prop = n / sum(n))

# csv files for Tableau presentation
top_foundries %>% 
  mutate(marketcap = as.numeric(marketcap)) %>% 
  mutate(price = as.numeric(price)) %>% 
  count(CountryCode) %>% 
  rename('Country Code' = CountryCode, Count = n) #%>% 
  #write.csv('C:\\Users\\15712\\Desktop\\top_companies_country.csv', 
  #          row.names = FALSE)

companies_cap <- top_foundries %>% 
  mutate(marketcap = as.numeric(marketcap)) %>% 
  group_by(CountryCode) %>% 
  summarise(cap = sum(marketcap)) #%>% 
  #write.csv('C:\\Users\\15712\\Desktop\\companies_marketcap.csv')
