#### Preamble ####
# Purpose: This code loads, cleans, aggregates, and saves various datasets of U.S. house prices, removing irrelevant rows and columns, and summarizing the data by state on a monthly and yearly basis, then stores the processed data for further analysis.
# Author: Ziheng Zhong
# Date: 02 April 2024
# Contact: ziheng.zhong@mail.utoronto.ca
# License: MIT
# Pre-requisites: none



#### Workspace setup ####
library(dplyr)
library(tidyverse)
library(lubridate)



#### load data ####
all_house_price <- read_csv("../../inputs/data/all_house_price.csv")



#### Cleaning ####
clean_house_price <- all_house_price %>% # Delete the first row since it cause issues in further analysis (we focus on state but the first row is data for U.S. as a whole)
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
  file = "outputs/data/clean_house_price.csv"
)

write_csv(
  x = state_house_price_by_month,
  file = "outputs/data/state_house_price_by_month.csv"
)

write_csv(
  x = state_house_price_by_year,
  file = "outputs/data/state_house_price_by_year.csv"
)



#### load data ####
oneb_house_price <- read_csv("../../inputs/data/1b_house_price.csv")



#### Cleaning ####
clean_house_price <- oneb_house_price %>% # Delete the first row since it cause issues in further analysis (we focus on state but the first row is data for U.S. as a whole)
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
  file = "outputs/data/clean_1b_house_price.csv"
)

write_csv(
  x = state_house_price_by_month,
  file = "outputs/data/state_1b_house_price_by_month.csv"
)

write_csv(
  x = state_house_price_by_year,
  file = "outputs/data/state_1b_house_price_by_year.csv"
)



#### load data ####
twob_house_price <- read_csv("../../inputs/data/2b_house_price.csv")



#### Cleaning ####
clean_house_price <- twob_house_price %>% # Delete the first row since it cause issues in further analysis (we focus on state but the first row is data for U.S. as a whole)
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
  file = "outputs/data/clean_2b_house_price.csv"
)

write_csv(
  x = state_house_price_by_month,
  file = "outputs/data/state_2b_house_price_by_month.csv"
)

write_csv(
  x = state_house_price_by_year,
  file = "outputs/data/state_2b_house_price_by_year.csv"
)



#### load data ####
threeb_house_price <- read_csv("../../inputs/data/3b_house_price.csv")



#### Cleaning ####
clean_house_price <- threeb_house_price %>% # Delete the first row since it cause issues in further analysis (we focus on state but the first row is data for U.S. as a whole)
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



#### load data ####
fourb_house_price <- read_csv("inputs/data/4b_house_price.csv")



#### Cleaning ####
clean_house_price <- fourb_house_price %>% # Delete the first row since it cause issues in further analysis (we focus on state but the first row is data for U.S. as a whole)
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
  file = "outputs/data/clean_4b_house_price.csv"
)

write_csv(
  x = state_house_price_by_month,
  file = "outputs/data/state_4b_house_price_by_month.csv"
)

write_csv(
  x = state_house_price_by_year,
  file = "outputs/data/state_4b_house_price_by_year.csv"
)



#### load data ####
fivebplus_house_price <- read_csv("../../inputs/data/5bplus_house_price.csv")



#### Cleaning ####
clean_house_price <- fivebplus_house_price %>% # Delete the first row since it cause issues in further analysis (we focus on state but the first row is data for U.S. as a whole)
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
  file = "outputs/data/clean_5bplus_house_price.csv"
)

write_csv(
  x = state_house_price_by_month,
  file = "outputs/data/state_5bplus_house_price_by_month.csv"
)

write_csv(
  x = state_house_price_by_year,
  file = "outputs/data/state_5bplus_house_price_by_year.csv"
)
