#### Preamble ####
# Purpose: 
# Author: Ziheng Zhong
# Date: 25 March 2024
# Contact: ziheng.zhong@mail.utoronto.ca
# License: MIT
# Pre-requisites: none



#### Workspace setup ####
library(maps)
library(mgcv)
library(ggmap)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(viridis)
library(lubridate)



#### load data ####
state_house_price_by_month <- read_csv("outputs/data/state_house_price_by_month.csv")
state_1b_house_price_by_month <- read_csv("outputs/data/state_1b_house_price_by_month.csv")
state_2b_house_price_by_month <- read_csv("outputs/data/state_2b_house_price_by_month.csv")
state_3b_house_price_by_month <- read_csv("outputs/data/state_3b_house_price_by_month.csv")
state_4b_house_price_by_month <- read_csv("outputs/data/state_4b_house_price_by_month.csv")
state_5bplus_house_price_by_month <- read_csv("outputs/data/state_5bplus_house_price_by_month.csv")



#### 5 Types of Houses Price Trend by Month ####
process_data <- function(data, label) { # Function to process each dataset
  data %>% 
    select(-StateName) %>%
    summarise_all(mean, na.rm = TRUE) %>%
    pivot_longer(everything(), names_to = "Date", values_to = "AverageHousePrice") %>%
    mutate(Trend = label,
           Date = as.Date(Date))
}

data_list <- list( # Processing all datasets
  one_bedroom = state_1b_house_price_by_month,
  two_bedroom = state_2b_house_price_by_month,
  three_bedroom = state_3b_house_price_by_month,
  four_bedroom = state_4b_house_price_by_month,
  five_plus_bedroom = state_5bplus_house_price_by_month
)

processed_data <- lapply(names(data_list), function(name) process_data(data_list[[name]], name))

combined_data <- bind_rows(processed_data) # Combining all processed data into one dataframe

ggplot(combined_data, aes(x = Date, y = AverageHousePrice, color = Trend)) + # Plotting
  geom_line() + 
  labs(title = "Trend of Average House Price from 2000 to 2024 by House Type",
       x = "Year", y = "Average House Price ($)") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_viridis_d(name = "House Type")



#### All Types of House Price Trend by Month ####
state_all <- state_house_price_by_month %>% select(-StateName) %>%  # Processing datasets
  summarise_all(mean, na.rm = TRUE) %>%
  pivot_longer(everything(), names_to = "Date", values_to = "AverageHousePrice")

state_all$Date <- as.Date(state_all$Date) # Convert Date from character to Date type

ggplot(state_all, aes(x = Date, y = AverageHousePrice)) + # Plotting
  geom_line(color = "blue") + 
  labs(title = "Trend of Average House Price from 2000 to 2024",
       x = "Year", y = "Average House Price ($)") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
