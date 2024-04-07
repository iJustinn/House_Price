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
raw_data <- read_csv("inputs/data/3b_house_price.csv")



#### Cleaning ####
clean_house_price <- raw_data %>% # Delete the first row since it cause issues in further analysis (we focus on state but the first row is data for U.S. as a whole)
  slice(-1)

clean_house_price <- clean_house_price %>% # Remove specific columns
  select(-c(SizeRank, RegionID, RegionType, RegionName))

#### Further Cleaning ####
# By State - monthly
state_house_price_by_month <- clean_house_price %>%
  group_by(StateName) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) # Calculating means house price of each state excluding NA

# By State - yearly
state_house_price_by_year <- state_house_price_by_month %>%
  pivot_longer( # Gather the monthly columns into key-value pairs, convert the keys to Dates, extract the year, and then calculate the mean
    cols = starts_with("20"), # Assuming all your date columns start with '20'
    names_to = "Month",
    values_to = "HousePrice"
  ) %>%
  mutate(Year = year(ymd(Month))) %>%
  group_by(StateName, Year) %>%
  summarise(AvgHousePrice = mean(HousePrice, na.rm = TRUE), .groups = 'drop') # Calculating mean house price by year for each state



#### save data ####
write_csv(
  x = clean_house_price,
  file = "outputs/data/clean_3b_house_price.csv"
)

write_csv(
  x = state_house_price_by_month,
  file = "outputs/data/state_3b_house_price_by_month.csv"
)

write_csv(
  x = state_house_price_by_year,
  file = "outputs/data/state_3b_house_price_by_year.csv"
)
