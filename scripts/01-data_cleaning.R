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



cali_house_price_clean %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>% 
  walk2(names(.), ~cat("The mean value of", .y, ":", .x, "\n")) # Calculating and printing mean of each numeric column



columns_needed <- c("ocean_proximity") # Specify the needed columns

ocean_proximity_frequency <- data.frame(Variable = character(), Frequency = integer()) # Initialize an empty data frame for storing frequencies

for (j in columns_needed) {
  temp <- as.data.frame(table(cali_house_price_clean[[j]]))  # Calculate frequency using table and convert to data frame
  
  colnames(temp) <- c("Variable", "Frequency")  # Rename the columns accordingly
  
  ocean_proximity_frequency <- bind_rows(ocean_proximity_frequency, temp)  # Combine with the main dataset
}

print(ocean_proximity_frequency) # Show the resulting dataset



#### save data ####
write_csv(
  x = cali_house_price_clean,
  file = "outputs/data/cali_house_price_clean.csv"
)

write_csv(
  x = ocean_proximity_frequency,
  file = "outputs/data/ocean_proximity_frequency.csv"
)
