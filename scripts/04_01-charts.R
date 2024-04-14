#### Preamble ####
# Purpose: The code is designed to process and visualize trends in average house prices by month for different house types using R, integrating data manipulation and visualization libraries to prepare and plot the data.
# Author: Ziheng Zhong
# Date: 02 April 2024
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

data_list <- list(
  `1B` = state_1b_house_price_by_month,
  `2B` = state_2b_house_price_by_month,
  `3B` = state_3b_house_price_by_month,
  `4B` = state_4b_house_price_by_month,
  `5B+` = state_5bplus_house_price_by_month
)

processed_data <- lapply(names(data_list), function(name) process_data(data_list[[name]], name))

combined_data <- bind_rows(processed_data)

ggplot(combined_data, aes(x = Date, y = AverageHousePrice, color = Trend)) +
  geom_line() + 
  labs(x = "Year", y = "Average House Price ($)") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_viridis_d(name = "Number of Bedrooms")



#### All Types of House Price Trend by Month ####
state_all <- state_house_price_by_month %>% select(-StateName) %>%  # Processing datasets
  summarise_all(mean, na.rm = TRUE) %>%
  pivot_longer(everything(), names_to = "Date", values_to = "AverageHousePrice")

state_all$Date <- as.Date(state_all$Date) # Convert Date from character to Date type

extrema <- state_all %>%
  mutate(is_peak = ifelse(lag(AverageHousePrice, 1, default = first(AverageHousePrice)) < AverageHousePrice & 
                            lead(AverageHousePrice, 1, default = last(AverageHousePrice)) < AverageHousePrice,
                          TRUE, FALSE),
         is_trough = ifelse(lag(AverageHousePrice, 1, default = first(AverageHousePrice)) > AverageHousePrice & 
                              lead(AverageHousePrice, 1, default = last(AverageHousePrice)) > AverageHousePrice,
                            TRUE, FALSE)) %>%
  filter(is_peak | is_trough) %>%
  select(Date, AverageHousePrice, is_peak, is_trough)

extrema <- extrema %>%
  filter(format(Date, "%Y") %in% c("2007", "2012", "2022"))

ggplot(state_all, aes(x = Date, y = AverageHousePrice)) + # Plotting
  geom_line(color = "black") + 
  geom_text(data = extrema, aes(label = format(Date, "%Y")), nudge_y = 10000, check_overlap = TRUE, color = "black") +
  geom_vline(data = extrema, aes(xintercept = as.numeric(Date)), linetype = "dashed", color = "red") +
  labs(title = "Trend of Average House Price from 2000 to 2024",
       x = "Year", y = "Average House Price ($)") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
