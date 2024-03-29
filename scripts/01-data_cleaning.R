#### Preamble ####
# Purpose: 
# Author: Ziheng Zhong
# Date: 25 March 2024
# Contact: ziheng.zhong@mail.utoronto.ca
# License: MIT
# Pre-requisites: none



#### Workspace setup ####

library(tidyverse)
library(dplyr)



#### load data ####
cali_house_price <- read_csv("inputs/data/cali_house_price.csv")



#### Cleaning ####
total_bedrooms_mean_value <- mean(cali_house_price$total_bedrooms, na.rm = TRUE) # Calculate the mean of the total_bedrooms column, excluding NA values

cali_house_price_clean <- cali_house_price %>%
  mutate(total_bedrooms = ifelse(is.na(total_bedrooms), total_bedrooms_mean_value, total_bedrooms)) # Replace NA values with the mean value in the total_bedrooms column



#### save data ####
write_csv(
  x = cali_house_price_clean,
  file = "outputs/data/cali_house_price_clean.csv"
)


