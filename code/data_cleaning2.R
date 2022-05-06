##########################
# Author:bbaasan
# Date: May 4, 22
# Purpose: create a table with all silicon indicators
# email: bbaasan@gmu.edu
##########################

library(tidyverse)

list.files('raw_data/comtrade')

sd <- read_csv('raw_data/comtrade/silicon_dioxide.csv', show_col_types = FALSE) %>% 
  rename(sd_reporter_iso = `Reporter ISO`,
         year = Year,
         trade_flow = `Trade Flow`,
         sd_partner_iso = `Partner ISO`,
         sd_qty = Qty, sd_value = `Trade Value (US$)`) %>% 
  filter(year == '2018') %>% filter(trade_flow == 'Export') %>% 
  filter(sd_reporter_iso != 'WLD' & sd_partner_iso != 'WLD') %>% 
  select(sd_reporter_iso, sd_partner_iso, sd_qty, sd_value) %>% drop_na()


sp <- read_csv('raw_data/comtrade/silicon_primaryform.csv', show_col_types = FALSE) %>% 
  rename(s_reporter_iso = `Reporter ISO`,
         year = Year,
         trade_flow = `Trade Flow`,
         s_partner_iso = `Partner ISO`,
         s_qty = Qty, s_value = `Trade Value (US$)`) %>%
  filter(year == '2018') %>% filter(trade_flow == 'Export') %>% 
  filter(s_reporter_iso != 'WLD' & s_partner_iso != 'WLD') %>% 
  select(s_reporter_iso, s_partner_iso, s_qty, s_value) %>% drop_na()

read_csv('raw_data/comtrade/wafer.csv', show_col_types = FALSE) %>% 
  rename(s_reporter_iso = `Reporter ISO`,
         year = Year,
         trade_flow = `Trade Flow`,
         s_partner_iso = `Partner ISO`,
         s_qty = Qty, s_value = `Trade Value (US$)`) %>%
  count(`Commodity Code`)
  filter(year == '2018') %>% filter(trade_flow == 'Export') %>% 
  filter(s_reporter_iso != 'WLD' & s_partner_iso != 'WLD') %>% 
  select(s_reporter_iso, s_partner_iso, s_qty, s_value) %>% drop_na()