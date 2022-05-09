##########################
# Author:bbaasan
# Date: May 4, 22
# Purpose: create a table with all silicon indicators
# email: bbaasan@gmu.edu
##########################
# check wd 
getwd()

# check files and wd level
list.files('../')

library(tidyverse)

# silicon dioxide
sd <- read_csv('../raw_data/comtrade/silicon_dioxide.csv', show_col_types = FALSE) %>% 
  rename(sd_reporter_iso = `Reporter ISO`,
         year = Year,
         trade_flow = `Trade Flow`,
         sd_partner_iso = `Partner ISO`,
         sd_qty = Qty, sd_value = `Trade Value (US$)`) %>% 
  filter(year == '2018') %>% filter(trade_flow == 'Export') %>% 
  filter(sd_reporter_iso != 'WLD' & sd_partner_iso != 'WLD') %>% 
  select(sd_reporter_iso, sd_partner_iso, sd_qty, sd_value) %>% drop_na()

# silicon in primary forms
sp <- read_csv('../raw_data/comtrade/silicon_primaryform.csv', show_col_types = FALSE) %>% 
  rename(sp_reporter_iso = `Reporter ISO`,
         year = Year,
         trade_flow = `Trade Flow`,
         sp_partner_iso = `Partner ISO`,
         sp_qty = Qty, sp_value = `Trade Value (US$)`) %>%
  filter(year == '2018') %>% filter(trade_flow == 'Export') %>% 
  filter(sp_reporter_iso != 'WLD' & sp_partner_iso != 'WLD') %>% 
  select(sp_reporter_iso, sp_partner_iso, sp_qty, sp_value) %>% drop_na()

wf <- read_csv('../raw_data/comtrade/wafer.csv', show_col_types = FALSE) %>% 
  rename(wf_reporter_iso = `Reporter ISO`,
         year = Year,
         trade_flow = `Trade Flow`,
         wf_partner_iso = `Partner ISO`,
         wf_qty = Qty, wf_value = `Trade Value (US$)`) %>%
  # count(year) %>% 
  filter(year == '2018') %>% filter(trade_flow == 'Export') %>% 
  filter(wf_reporter_iso != 'WLD' & wf_partner_iso != 'WLD') %>% 
  select(wf_reporter_iso, wf_partner_iso, wf_qty, wf_value) %>% drop_na()

list.files('../raw_data/world')

main <- read_csv('../raw_data/world/countries.csv')

library(igraph)

sd_graph <- graph_from_data_frame(sd, directed = FALSE)
sp_graph <- graph_from_data_frame(sp, directed = FALSE)
wf_graph <- graph_from_data_frame(wf, directed = FALSE)

plot(sd_graph, min_freq = 0.1, edge_alpha = 0.1, edge_size = 5, 
     vertex.size=3, vertex.label.size=1)

# silicon dioxide
SD <- as.data.frame(igraph::degree(sd_graph, mode='all'))
SD$betweenness <- igraph::betweenness(sd_graph)
SD$eigen <- igraph::eigen_centrality(sd_graph)$vector

# qty 
sd_table <- sd %>%
  group_by(sd_reporter_iso) %>% 
  summarise(sd_qty = sum(sd_qty)) %>% 
  mutate(CountryCode = sd_reporter_iso) %>% 
  select(CountryCode, sd_qty)

# sd value
sd_tibble <- sd %>%
  group_by(sd_reporter_iso) %>% 
  summarise(sd_value = sum(sd_value)) %>% 
  mutate(CountryCode = sd_reporter_iso) %>% 
  inner_join(sd_table, by = 'CountryCode') %>% 
  select(CountryCode, sd_qty, sd_value)

class(sd_tibble)

names(SD) <- c('sd_degree', 'sd_betweenness', 'sd_eigen')
SD$CountryCode <- row.names(SD)

SD_tibble <- SD %>% 
  inner_join(sd_tibble, by = 'CountryCode') %>% 
  select(CountryCode, sd_qty, sd_value, sd_degree, sd_betweenness, sd_eigen)

SD_tibble %>% 
  ggplot(aes(x=sd_qty, y=sd_value, fill = sd_degree))+
  geom_point(alpha = .2, )

help(geom_point)

# silicon in primary forms
SP <- as.data.frame(igraph::degree(sp_graph, mode='all'))
SP$betweenness <- igraph::betweenness(sp_graph)
SP$eigen <- igraph::eigen_centrality(sp_graph)$vector
names(SP) <- c('sp_degree', 'sp_betweenness', 'sp_eigen')
SP$CountryCode <- row.names(SP)

# wafer
WF <- as.data.frame(igraph::degree(wf_graph, mode='all'))
WF$betweenness <- igraph::betweenness(wf_graph)
WF$eigen <- igraph::eigen_centrality(wf_graph)$vector
names(WF) <- c('wf_degree', 'wf_betweenness', 'wf_eigen')
WF$CountryCode <- row.names(WF)

# returns a table contains sd (silicon dioxide) aggregate data
sd_table <- function(df){
  # iso and qty
  ccode.qty <- df %>%
    group_by(sd_reporter_iso) %>% 
    summarise(sd_qty = sum(sd_qty)) %>% 
    mutate(CountryCode = sd_reporter_iso) %>% 
    select(CountryCode, sd_qty)
  
  # iso and sd value
  sd_tibble <- df %>%
    group_by(sd_reporter_iso) %>% 
    summarise(sd_value = sum(sd_value)) %>% 
    mutate(CountryCode = sd_reporter_iso) %>% 
    inner_join(ccode.qty, by = 'CountryCode') %>% 
    select(CountryCode, sd_qty, sd_value)
  
  sd_graph <- graph_from_data_frame(df, directed = FALSE)
  
  SD <- as.data.frame(igraph::degree(sd_graph, mode='all'))
  
  SD$betweenness <- igraph::betweenness(sd_graph)
  SD$eigen <- igraph::eigen_centrality(sd_graph)$vector
  
  names(SD) <- c('sd_degree', 'sd_betweenness', 'sd_eigen')
  SD$CountryCode <- row.names(SD)
  SD_tibble <- SD %>% 
    inner_join(sd_tibble, by = 'CountryCode') %>% 
    select(CountryCode, sd_qty, sd_value, sd_degree, sd_betweenness, sd_eigen)
  return(SD_tibble)
}

dioxide <- sd_table(sd)

sp_table <- function(df){
  # iso and qty
  ccode.qty <- df %>%
    group_by(sp_reporter_iso) %>% 
    summarise(sp_qty = sum(sp_qty)) %>% 
    mutate(CountryCode = sp_reporter_iso) %>% 
    select(CountryCode, sp_qty)
  
  # iso and sd value
  sp_tibble <- df %>%
    group_by(sp_reporter_iso) %>% 
    summarise(sp_value = sum(sp_value)) %>% 
    mutate(CountryCode = sp_reporter_iso) %>% 
    inner_join(ccode.qty, by = 'CountryCode') %>% 
    select(CountryCode, sp_qty, sp_value)
  
  sp_graph <- graph_from_data_frame(df, directed = FALSE)
  
  SP <- as.data.frame(igraph::degree(sp_graph, mode='all'))
  
  SP$betweenness <- igraph::betweenness(sp_graph)
  SP$eigen <- igraph::eigen_centrality(sp_graph)$vector
  
  names(SP) <- c('sp_degree', 'sp_betweenness', 'sp_eigen')
  SP$CountryCode <- row.names(SP)
  SP_tibble <- SP %>% 
    inner_join(sp_tibble, by = 'CountryCode') %>% 
    select(CountryCode, sp_qty, sp_value, sp_degree, sp_betweenness, sp_eigen)
  return(SP_tibble)
}

primary <- sp_table(sp)

wf_table <- function(df){
  # iso and qty
  ccode.qty <- df %>%
    group_by(wf_reporter_iso) %>% 
    summarise(wf_qty = sum(wf_qty)) %>% 
    mutate(CountryCode = wf_reporter_iso) %>% 
    select(CountryCode, wf_qty)
  
  # iso and sd value
  wf_tibble <- df %>%
    group_by(wf_reporter_iso) %>% 
    summarise(wf_value = sum(wf_value)) %>% 
    mutate(CountryCode = wf_reporter_iso) %>% 
    inner_join(ccode.qty, by = 'CountryCode') %>% 
    select(CountryCode, wf_qty, wf_value)
  
  wf_graph <- graph_from_data_frame(df, directed = FALSE)
  
  WF <- as.data.frame(igraph::degree(wf_graph, mode='all'))
  
  WF$betweenness <- igraph::betweenness(wf_graph)
  WF$eigen <- igraph::eigen_centrality(wf_graph)$vector
  
  names(WF) <- c('wf_degree', 'wf_betweenness', 'wf_eigen')
  WF$CountryCode <- row.names(WF)
  WF_tibble <- WF %>% 
    inner_join(wf_tibble, by = 'CountryCode') %>% 
    select(CountryCode, wf_qty, wf_value, wf_degree, wf_betweenness, wf_eigen)
  return(WF_tibble)
}
wafers <- wf_table(wf)

updated_main <- main %>% 
  full_join(dioxide, by = 'CountryCode') %>% 
  full_join(primary, by = 'CountryCode') %>% 
  full_join(wafers, by = 'CountryCode') %>% 
  select(CountryName, CountryCode, sd_degree, sd_betweenness, sd_eigen,
         sp_degree, sp_betweenness, sp_eigen,
         wf_degree, wf_betweenness, wf_eigen, region, income)

updated_main %>% 
  print(n= Inf)


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
  xlab('Country') + ylab('Total Count')

