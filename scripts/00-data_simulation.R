#### Preamble ####
# Purpose: 
# Author: Ziheng Zhong
# Date: 25 March 2024
# Contact: ziheng.zhong@mail.utoronto.ca
# License: MIT
# Pre-requisites: none



#### Workspace setup ####
library(dplyr) # for data manipulation
library(ggplot2) # for data visualization, part of the tidyverse



#### Sumulation ####
set.seed(123)  # for reproducibility

simulate_dataset <- function(n) { # Function to simulate the dataset
  longitude <- rnorm(n, mean = -100, sd = 1)
  latitude <- rnorm(n, mean = 30, sd = 1)
  housing_median_age <- sample(1:50, n, replace = TRUE)
  total_rooms <- rpois(n, lambda = 100)
  households <- rpois(n, lambda = 100)
  median_income <- rlnorm(n, meanlog = 10, sdlog = 0.5) / 10000
  median_house_value <- rlnorm(n, meanlog = 15, sdlog = 1)
  ocean_proximity <- factor(
    sample(c("<1H OCEAN", "INLAND", "NEAR OCEAN", "NEAR BAY", "ISLAND"), n, replace = TRUE),
    levels = c("<1H OCEAN", "INLAND", "NEAR OCEAN", "NEAR BAY", "ISLAND")
  )
  
  return(data.frame(
    longitude,
    latitude,
    housing_median_age,
    total_rooms,
    total_bedrooms = total_rooms * runif(n, 0.5, 1), # assuming bedrooms are 50-100% of total rooms
    population = households * sample(2:5, n, replace = TRUE), # assuming each household has 2-5 members
    households,
    median_income,
    median_house_value,
    ocean_proximity
  ))
}

simulation_data <- simulate_dataset(100) # Generate the simulation_data

head(simulation_data) # Print the first few rows of the simulation_data
