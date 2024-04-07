#### Preamble ####
# Purpose: 
# Author: Ziheng Zhong
# Date: 25 March 2024
# Contact: ziheng.zhong@mail.utoronto.ca
# License: MIT
# Pre-requisites: none



#### Workspace setup ####
library(maps)
library(dplyr)
library(ggplot2)



#### Sumulation ####
set.seed(123)  # for reproducibility

average_house_prices <- runif(n = length(states), min = 100000, max = 700000) # Simulating data for average house prices for each US state
state_average_prices <- data.frame(State = states, AverageHousePrice = average_house_prices) 
simulation_data <- merge(us_states_map, state_average_prices, by.x = "region", by.y = "State")

head(simulation_data) # preview simulation data
tail(simulation_data)



#### Simulation Chart ####
us_states_map <- map_data("state") # Getting map data

ggplot() + # Creating the heatmap
  geom_polygon(data = simulation_data, aes(x = long, y = lat, group = group, fill = AverageHousePrice), color = "white") +
  coord_fixed(1.3) +
  viridis::scale_fill_viridis(name = "Avg House Price", option = "plasma") +
  theme_minimal() +
  labs(title = "Average House Price by State in the US",
       subtitle = "Simulation Data",
       caption = "caption") +
  theme(legend.position = "right")
