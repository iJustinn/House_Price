#### Preamble ####
# Purpose: The code merges yearly house price data for different types of houses based on the number of bedrooms from various datasets, normalizes their structure by adding a uniform 'NumBedroom' column while removing the 'StateName' column, omits any missing values, and then saves the merged and cleaned dataset to a new CSV file.
# Author: Ziheng Zhong
# Date: 02 April 2024
# Contact: ziheng.zhong@mail.utoronto.ca
# License: MIT
# Pre-requisites: none



#### Workspace setup ####
library(arrow)
library(dplyr)
library(tidyverse)
library(lubridate)



#### load data ####
setwd("/Users/ijustin/Library/CloudStorage/Dropbox/Code/STA302/Research_Papers/House_Price")

state_1b_house_price_by_year <- read_csv("outputs/data/state_1b_house_price_by_year.csv")
state_2b_house_price_by_year <- read_csv("outputs/data/state_2b_house_price_by_year.csv")
state_3b_house_price_by_year <- read_csv("outputs/data/state_3b_house_price_by_year.csv")
state_4b_house_price_by_year <- read_csv("outputs/data/state_4b_house_price_by_year.csv")
state_5bplus_house_price_by_year <- read_csv("outputs/data/state_5bplus_house_price_by_year.csv")



#### merge data ####
# Change the column "StateName" to "NumBedroom" and set the value to 1 for all rows
state_1b_house_price_by_year <- state_1b_house_price_by_year %>%
  mutate(NumBedroom = 1) %>%
  select(-StateName) # This removes the original StateName column

state_2b_house_price_by_year <- state_2b_house_price_by_year %>%
  mutate(NumBedroom = 2) %>%
  select(-StateName) # This removes the original StateName column

state_3b_house_price_by_year <- state_3b_house_price_by_year %>%
  mutate(NumBedroom = 3) %>%
  select(-StateName) # This removes the original StateName column

state_4b_house_price_by_year <- state_4b_house_price_by_year %>%
  mutate(NumBedroom = 4) %>%
  select(-StateName) # This removes the original StateName column

state_5bplus_house_price_by_year <- state_5bplus_house_price_by_year %>%
  mutate(NumBedroom = 5) %>%
  select(-StateName) # This removes the original StateName column

merge_data <- bind_rows(state_1b_house_price_by_year, state_2b_house_price_by_year, state_3b_house_price_by_year, state_4b_house_price_by_year, state_5bplus_house_price_by_year)
merge_data <- na.omit(merge_data)



#### save data as Parquet ####
write_parquet(merge_data, "outputs/data/merged_house_price.parquet")
