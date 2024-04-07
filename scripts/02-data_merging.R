#### Preamble ####
# Purpose: 
# Author: Ziheng Zhong
# Date: 25 March 2024
# Contact: ziheng.zhong@mail.utoronto.ca
# License: MIT
# Pre-requisites: none



#### Workspace setup ####
library(dplyr)
library(tidyverse)
library(lubridate)



#### load data ####
state_1b_house_price_by_year <- read_csv("outputs/data/state_1b_house_price_by_year.csv")
state_2b_house_price_by_year <- read_csv("outputs/data/state_2b_house_price_by_year.csv")
state_3b_house_price_by_year <- read_csv("outputs/data/state_3b_house_price_by_year.csv")
state_4b_house_price_by_year <- read_csv("outputs/data/state_4b_house_price_by_year.csv")
state_5bplus_house_price_by_year <- read_csv("outputs/data/state_5bplus_house_price_by_year.csv")



#### merge data ####
state_1b_house_price_by_year <- state_1b_house_price_by_year %>% # Rename AvgHousePrice column in state_1b_house_price_by_year
  rename(AvgHousePrice_1b = AvgHousePrice)

merge_data <- left_join(state_1b_house_price_by_year, # Merge state_2b_house_price_by_year
                            state_2b_house_price_by_year %>% 
                              select(StateName, Year, AvgHousePrice) %>%
                              rename(AvgHousePrice_2b = AvgHousePrice), 
                            by = c("StateName", "Year"))

merge_data <- left_join(merge_data, # Merging state_3b_house_price_by_year
                        state_3b_house_price_by_year %>%
                          select(StateName, Year, AvgHousePrice) %>%
                          rename(AvgHousePrice_3b = AvgHousePrice),
                        by = c("StateName", "Year"))

merge_data <- left_join(merge_data, # Merging state_4b_house_price_by_year
                        state_4b_house_price_by_year %>%
                          select(StateName, Year, AvgHousePrice) %>%
                          rename(AvgHousePrice_4b = AvgHousePrice),
                        by = c("StateName", "Year"))

merge_data <- left_join(merge_data, # Merging state_5bplus_house_price_by_year
                        state_5bplus_house_price_by_year %>%
                          select(StateName, Year, AvgHousePrice) %>%
                          rename(AvgHousePrice_5bplus = AvgHousePrice),
                        by = c("StateName", "Year"))



#### save data ####
write_csv(
  x = merge_data,
  file = "outputs/data/merged_house_price.csv"
)
