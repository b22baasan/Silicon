setwd("~/Desktop/Github/Test/Silicon/code")

library(tidyverse)
library(igraph)
library(networkD3)

list.files('../raw_data')

pop <- read_csv('../raw_data/population/pop.csv',
         show_col_types = FALSE) %>% 
  select('Country Name', 'Country Code', '2017','2018','2019', '2020')

names(pop) <- c('CountryName', 'CountryCode','2017','2018','2019', '2020')

list.files('../raw_data/world')

eap <- read_csv('../raw_data/world/eastasiapacific.csv')
mena <- read_csv('../raw_data/world/middleeastnorthafrica.csv')
sa <- read_csv('../raw_data/world/southasia.csv')
eca <- read_csv('../raw_data/world/europecentralasia.csv')
lac <- read_csv('../raw_data/world/latinamericacaribbean.csv')
na <- read_csv('../raw_data/world/northamerica.csv')
ssa <- read_csv('../raw_data/world/subsaharianafrica.csv')

pop <- pop %>% 
  mutate(region = CountryCode) %>% 
  mutate(region = if_else(region %in% eap$CountryCode, 'eastasia_pacific', region)) %>% 
  mutate(region = if_else(region %in% mena$CountryCode, 'middleeastnafrica', region)) %>% 
  mutate(region = if_else(region %in% sa$CountryCode, 'southasia', region)) %>%
  mutate(region = if_else(region %in% eca$CountryCode, 'europecentralasia', region)) %>%
  mutate(region = if_else(region %in% lac$CountryCode, 'latinamericacaribbean', region)) %>%
  mutate(region = if_else(region %in% na$CountryCode, 'northamerica', region)) %>%
  mutate(region = if_else(region %in% ssa$CountryCode, 'subsaharianafrica', region)) %>% 
  filter(nchar(region) > 3)

pop

get_income <- function(text){
  file = read_csv(text)
  file = file$CountryCode
  file
}

list.files('../processed_data/income')

lowermiddle <- get_income('../raw_data/world/lowermiddle.csv')
unclassified <- get_income('../raw_data/world/unclassified.csv')
highincome <- get_income('../raw_data/world/highincome.csv')
lowincome <- get_income('../raw_data/world/lowincome.csv')
uppermiddle <- get_income('../raw_data/world/uppermiddle.csv')

my_csv <- pop %>% 
  mutate(income = CountryCode) %>% 
  mutate(income = if_else(income %in% lowermiddle, 'lowermiddle', income)) %>% 
  mutate(income = if_else(income %in% unclassified, 'unclassified', income)) %>%
  mutate(income = if_else(income %in% highincome, 'high', income)) %>%
  mutate(income = if_else(income %in% lowincome, 'low', income)) %>% 
  mutate(income = if_else(income %in% uppermiddle, 'uppermiddle', income)) %>% 
  arrange(CountryName) 

# write.csv(my_csv, '../processed_data/income/countries.csv', row.names = FALSE)

sd <- read_csv('../raw_data/comtrade/silicon_dioxide.csv') %>% 
  filter(Reporter != 'World') %>% filter(Partner != 'World')%>%
  filter(Year == 2020) %>% filter(`Trade Flow` == 'Export') %>% 
  select(`Reporter ISO`, `Partner ISO`, Qty, `Trade Value (US$)`) %>% 
  rename(ReporterISO=`Reporter ISO`,PartnerISO=`Partner ISO`, 
         Value =`Trade Value (US$)`) %>% drop_na()

gg <- igraph::graph_from_data_frame(sd, directed = FALSE)

print_all(gg)
#
#help(igraph)
#
## countries report more than a country
#partner[!(partner %in%  reporter)]
#
#print_all(gg)
#
#summary(my_g)
#
#E(my_g)
#
#sort(V(my_g)$name)
#
#print_all(my_g)
#
#sd %>% mutate(renames_all)
#
plot(gg, min_freq = 0.1, edge_alpha = 0.1, edge_size = 5, 
     vertex.size=3, vertex.label.size=1)

x <- as.data.frame(igraph::degree(gg, mode='all', normalized = TRUE))
x$betweenness <- igraph::betweenness(gg, normalized = TRUE)
x$eigen <- igraph::eigen_centrality(gg)$vector

names(x) <- c('degree', 'betweenness', 'eigen')
x$CountryCode <- row.names(x)
x 
my_csv %>% 
  select(CountryCode, '2020', region, income) %>% 
  left_join(x, by = 'CountryCode') %>% 
  select(CountryCode, degree, betweenness, eigen, '2020', income, region) %>% 
  rename(population = '2020') %>% drop_na()


