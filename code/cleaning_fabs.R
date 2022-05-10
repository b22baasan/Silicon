test_main <- main %>% 
  full_join(dioxide, by = 'CountryCode') %>% 
  full_join(primary, by = 'CountryCode') %>% 
  full_join(wafers, by = 'CountryCode') %>% 
  mutate(region = as.factor(region)) %>% 
  mutate(income = as.factor(income)) %>% 
  drop_na()

test_main %>% 
  ggplot(aes(x=sd_qty, y=sd_value, color = income, fill=income))+
  geom_point(alpha=.9)

list.files('../raw_data/fabs_foundries')
