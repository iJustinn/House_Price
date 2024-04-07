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
