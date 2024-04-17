#### Preamble ####
# Purpose: This code loads multiple datasets of U.S. house prices, converts state abbreviations to full names, generates heat maps for different types of houses, and plots the overall and bedroom-specific price trends by month.
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
state_house_price_by_year <- read_csv("outputs/data/state_house_price_by_year.csv")
state_1b_house_price_by_year <- read_csv("outputs/data/state_1b_house_price_by_year.csv")
state_2b_house_price_by_year <- read_csv("outputs/data/state_2b_house_price_by_year.csv")
state_3b_house_price_by_year <- read_csv("outputs/data/state_3b_house_price_by_year.csv")
state_4b_house_price_by_year <- read_csv("outputs/data/state_4b_house_price_by_year.csv")
state_5bplus_house_price_by_year <- read_csv("outputs/data/state_5bplus_house_price_by_year.csv")



#### Preparations ####
us_states_map <- map_data("state") # Getting map data for US states

state_abbrev_to_full <- c( # Define the mapping from state abbreviations to full names
  AL = "alabama", AK = "alaska", AZ = "arizona", AR = "arkansas", CA = "california",
  CO = "colorado", CT = "connecticut", DE = "delaware", FL = "florida", GA = "georgia",
  HI = "hawaii", ID = "idaho", IL = "illinois", IN = "indiana", IA = "iowa",
  KS = "kansas", KY = "kentucky", LA = "louisiana", ME = "maine", MD = "maryland",
  MA = "massachusetts", MI = "michigan", MN = "minnesota", MS = "mississippi",
  MO = "missouri", MT = "montana", NE = "nebraska", NV = "nevada", NH = "new hampshire",
  NJ = "new jersey", NM = "new mexico", NY = "new york", NC = "north carolina",
  ND = "north dakota", OH = "ohio", OK = "oklahoma", OR = "oregon", PA = "pennsylvania",
  RI = "rhode island", SC = "south carolina", SD = "south dakota", TN = "tennessee",
  TX = "texas", UT = "utah", VT = "vermont", VA = "virginia", WA = "washington",
  WV = "west virginia", WI = "wisconsin", WY = "wyoming", DC = "district of columbia"
)



#### US States Overall House Price Heat Map ####
overall_hm_data <- na.omit(state_house_price_by_year) # Delete all NAs
overall_hm_data$StateName <- tolower(state_abbrev_to_full[overall_hm_data$StateName]) # Convert StateName from abbreviation to full name

overall_hm_data <- overall_hm_data %>% # Taking average for house price based on states
  group_by(StateName) %>%
  summarise(AvgHousePrice = mean(AvgHousePrice, na.rm = TRUE))

overall_hm_map_merged_data <- merge(us_states_map, overall_hm_data, by.x = "region", by.y = "StateName") # Merge data ready for chart

ggplot() + # Generating heat map
  geom_polygon(data = overall_hm_map_merged_data, aes(x = long, y = lat, group = group, fill = AvgHousePrice), color = "white") +
  coord_fixed(1.3) +
  scale_fill_viridis(name = "Avg House Price", option = "plasma") +
  theme_minimal() +
  labs(title = "Average All Type Houses Price by State in the US",
       subtitle = "Data: Your Dataset",
       caption = "Visualization by Example") +
  theme(legend.position = "right")



#### US States 1b House Price Heat Map ####
oneb_hm_data <- na.omit(state_1b_house_price_by_year) # Delete all NAs
oneb_hm_data$StateName <- tolower(state_abbrev_to_full[oneb_hm_data$StateName]) # Convert StateName from abbreviation to full name

oneb_hm_data <- oneb_hm_data %>% # Taking average for house price based on states
  group_by(StateName) %>%
  summarise(AvgHousePrice = mean(AvgHousePrice, na.rm = TRUE))

oneb_hm_map_merged_data <- merge(us_states_map, oneb_hm_data, by.x = "region", by.y = "StateName") # Merge data ready for chart

ggplot() + # Generating heat map
  geom_polygon(data = oneb_hm_map_merged_data, aes(x = long, y = lat, group = group, fill = AvgHousePrice), color = "white") +
  coord_fixed(1.3) +
  scale_fill_viridis(name = "Avg House Price", option = "plasma") +
  theme_minimal() +
  labs(title = "Average 1b House Price by State in the US",
       subtitle = "Data: Your Dataset",
       caption = "Visualization by Example") +
  theme(legend.position = "right")



#### US States 2b House Price Heat Map ####
twob_hm_data <- na.omit(state_2b_house_price_by_year) # Delete all NAs
twob_hm_data$StateName <- tolower(state_abbrev_to_full[twob_hm_data$StateName]) # Convert StateName from abbreviation to full name

twob_hm_data <- twob_hm_data %>% # Taking average for house price based on states
  group_by(StateName) %>%
  summarise(AvgHousePrice = mean(AvgHousePrice, na.rm = TRUE))

twob_hm_map_merged_data <- merge(us_states_map, twob_hm_data, by.x = "region", by.y = "StateName") # Merge data ready for chart

ggplot() + # Generating heat map
  geom_polygon(data = twob_hm_map_merged_data, aes(x = long, y = lat, group = group, fill = AvgHousePrice), color = "white") +
  coord_fixed(1.3) +
  scale_fill_viridis(name = "Avg House Price", option = "plasma") +
  theme_minimal() +
  labs(title = "Average 2b House Price by State in the US",
       subtitle = "Data: Your Dataset",
       caption = "Visualization by Example") +
  theme(legend.position = "right")



#### US States 3b House Price Heat Map ####
threeb_hm_data <- na.omit(state_3b_house_price_by_year) # Delete all NAs
threeb_hm_data$StateName <- tolower(state_abbrev_to_full[threeb_hm_data$StateName]) # Convert StateName from abbreviation to full name

threeb_hm_data <- threeb_hm_data %>% # Taking average for house price based on states
  group_by(StateName) %>%
  summarise(AvgHousePrice = mean(AvgHousePrice, na.rm = TRUE))

threeb_hm_map_merged_data <- merge(us_states_map, threeb_hm_data, by.x = "region", by.y = "StateName") # Merge data ready for chart

ggplot() + # Generating heat map
  geom_polygon(data = threeb_hm_map_merged_data, aes(x = long, y = lat, group = group, fill = AvgHousePrice), color = "white") +
  coord_fixed(1.3) +
  scale_fill_viridis(name = "Avg House Price", option = "plasma") +
  theme_minimal() +
  labs(title = "Average 3b House Price by State in the US",
       subtitle = "Data: Your Dataset",
       caption = "Visualization by Example") +
  theme(legend.position = "right")



#### US States 4b House Price Heat Map ####
fourb_hm_data <- na.omit(state_4b_house_price_by_year) # Delete all NAs
fourb_hm_data$StateName <- tolower(state_abbrev_to_full[fourb_hm_data$StateName]) # Convert StateName from abbreviation to full name

fourb_hm_data <- fourb_hm_data %>% # Taking average for house price based on states
  group_by(StateName) %>%
  summarise(AvgHousePrice = mean(AvgHousePrice, na.rm = TRUE))

fourb_hm_map_merged_data <- merge(us_states_map, fourb_hm_data, by.x = "region", by.y = "StateName") # Merge data ready for chart

ggplot() + # Generating heat map
  geom_polygon(data = fourb_hm_map_merged_data, aes(x = long, y = lat, group = group, fill = AvgHousePrice), color = "white") +
  coord_fixed(1.3) +
  scale_fill_viridis(name = "Avg House Price", option = "plasma") +
  theme_minimal() +
  labs(title = "Average 4b House Price by State in the US",
       subtitle = "Data: Your Dataset",
       caption = "Visualization by Example") +
  theme(legend.position = "right")



#### US States 5b+ House Price Heat Map ####
fiveb_hm_data <- na.omit(state_5bplus_house_price_by_year) # Delete all NAs
fiveb_hm_data$StateName <- tolower(state_abbrev_to_full[fiveb_hm_data$StateName]) # Convert StateName from abbreviation to full name

fiveb_hm_data <- fiveb_hm_data %>% # Taking average for house price based on states
  group_by(StateName) %>%
  summarise(AvgHousePrice = mean(AvgHousePrice, na.rm = TRUE))

fiveb_hm_map_merged_data <- merge(us_states_map, fiveb_hm_data, by.x = "region", by.y = "StateName") # Merge data ready for chart

ggplot() + # Generating heat map
  geom_polygon(data = fiveb_hm_map_merged_data, aes(x = long, y = lat, group = group, fill = AvgHousePrice), color = "white") +
  coord_fixed(1.3) +
  scale_fill_viridis(name = "Avg House Price", option = "plasma") +
  theme_minimal() +
  labs(title = "Average 5b+ House Price by State in the US",
       subtitle = "Data: Your Dataset",
       caption = "Visualization by Example") +
  theme(legend.position = "right")



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
