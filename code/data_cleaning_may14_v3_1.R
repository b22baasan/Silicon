# author: bbaasan
# Date: May 10, 22
# version:
# Purpose: adding TWN 

library(tidyverse)
library(igraph)
my_func <- function(whatyear){
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

# thus far we have a table with five values -
# reporter. reporter_iso, partner, partner_iso, qty, value.
# Now, we prepare data for igraph where reporter as "from" 
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

dioxide <- sd_table(sd, whatyear) 

#####################################################################
sp <- get_commodity('../raw_data/comtrade/silicon_primary.csv')

sp$reporter_iso[is.na(sp$reporter_iso)] <- 'TWN'
sp$partner_iso[is.na(sp$partner_iso)] <- 'TWN'

primary <- sd_table(sp, whatyear)
#####################################################################
wf <- get_commodity('../raw_data/comtrade/wafers.csv')
wf$reporter_iso[is.na(wf$reporter_iso)] <- 'TWN'
wf$partner_iso[is.na(wf$partner_iso)] <- 'TWN'
wafers <- sd_table(wf, whatyear)
###########################################################################
main <- read_csv('../pop.csv') %>% 
  rename(CountryCode = `Country Code`,
         pop = `2017`) %>% 
  select(CountryCode, pop)

names(dioxide) <- c('CountryCode','d_qty','d_val','d_dgre','d_betw','d_eigen')
names(primary) <- c('CountryCode','p_qty','p_val','p_dgre','p_betw','p_eigen')
names(wafers) <- c('CountryCode','w_qty','w_val','w_dgre','w_betw','w_eigen')

mine <- main %>% 
  full_join(dioxide, by = 'CountryCode') %>% 
  full_join(primary, by = 'CountryCode') %>% 
  full_join(wafers, by = 'CountryCode') %>% 
  drop_na() %>%
  arrange(CountryCode) #%>% 
  #print(n=Inf)
return(mine)
}
year2017 <- my_func(2017)
year2018 <- my_func(2018)
year2019 <- my_func(2019)
year2020 <- my_func(2020)


pairs(year2017[, -1])

year2017[,-1]
