# author: bbaasan
# Date: May 10, 22
# version:
# Purpose: adding TWN 

library(tidyverse)
library(igraph)

Taiwan <- 'Other Asia, nes'
TWN <- 'Other Asia, nes'

# silicon dioxide

get_commodity <- function(csvpath){
  read_csv(csvpath) %>% 
  mutate(Reporter = ifelse(Reporter == 'Other Asia, nes', 'Taiwan', Reporter)) %>% 
  mutate(Partner = ifelse(Partner == 'Other Asia, nes', 'Taiwan', Partner)) %>%
  select(Reporter, `Reporter ISO`, Partner, `Partner ISO`, 
         Qty, `Trade Value (US$)`, Year) %>%
  rename(reporter = Reporter, reporter_iso = `Reporter ISO`,
         partner = Partner, partner_iso = `Partner ISO`,
         qty = Qty, value = `Trade Value (US$)`, year = Year)
  }

sd <- get_commodity('../raw_data/comtrade/silicon_dioxide.csv')

sd$reporter_iso[is.na(sd$reporter_iso)] <- 'TWN'
sd$partner_iso[is.na(sd$partner_iso)] <- 'TWN'
sd 

# to confirm whether TWN is assigned in iso code
sd %>%
  count(reporter_iso) %>% print(n=Inf)
sd %>%  # confirm country name 
  count(reporter) %>% print(n=Inf)
sd %>%  # confirm country name 
  count(partner) %>% print(n=Inf)
sd %>%
  count(partner_iso) %>% print(n=Inf)

# thus far we have a table with firve values
# reporter. reporter_iso, partner, partner_iso, qty, value
# now we prepare data for igraph where reporter as "from" 
# partner as "to". It is saying that from is a node (vertex)
# connected through undirected edge.

sd_table <- function(df, whatyear){ 
  # selects four parameters:
  # reporter = vertex
  # partner = adjacent nodes
  # qty and value
  df <- df %>% 
    filter(partner_iso != 'WLD') %>% # drop aggregate: world 
    filter(year == whatyear) %>% #any year other than parameter year will be dropped
    select(reporter_iso, partner_iso, qty, value) 
  
  # by quantity
  ccode.qty <- df %>% group_by(reporter_iso) %>% summarise(qty = sum(qty))
  ccode.qty$CountryCode <- ccode.qty$reporter_iso
  
  # iso and sd value
  sd_tibble <- df %>% group_by(reporter_iso) %>% summarise(val = sum(value))
  sd_tibble$CountryCode <- sd_tibble$reporter_iso
  
  sd_graph <- graph_from_data_frame(df, directed = FALSE)
  
  SD <- as.data.frame(igraph::degree(sd_graph, mode='all'))
  
  SD$eigen <- igraph::eigen_centrality(sd_graph)$vector
  SD$betweenness <- igraph::betweenness(sd_graph)
  
  names(SD) <- c('degree', 'betweenness', 'eigen')
  
  SD$CountryCode <- row.names(SD)
  
  SD <- SD %>% 
    full_join(ccode.qty, by = 'CountryCode') %>%
    full_join(sd_tibble, by = 'CountryCode') %>% 
    select(CountryCode, qty, val, degree, betweenness, eigen) %>% 
    drop_na() %>% arrange(CountryCode)
  return(SD)
}

dioxide <- sd_table(sd, 2017) 
