#### Preamble ####
# Purpose: 
# Author: Ziheng Zhong
# Date: 25 March 2024
# Contact: ziheng.zhong@mail.utoronto.ca
# License: MIT
# Pre-requisites: none



#### Workspace setup ####
library(mgcv)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(modelsummary)



#### load data ####
house_prices <- read_csv("../../outputs/data/merged_house_price.csv")



#### Multiple Regression Model ####
model_multi <- lm(AvgHousePrice ~ Year + NumBedroom, data = house_prices)
summary(model_multi)
modelsummary(model_multi) # Model Results



#### Polynomial Regression Model ####
model_poly <- lm(AvgHousePrice ~ poly(Year, 2) + poly(NumBedroom, 2), data = house_prices)
summary(model_poly)
modelsummary(model_poly) # Model Results



#### Generalized Additive Models ####
num_unique_years <- length(unique(house_prices$Year)) # Check the number of unique values for each variable
num_unique_bedrooms <- length(unique(house_prices$NumBedroom))

gam_model <- gam(AvgHousePrice ~ s(Year, k=num_unique_years-1) + s(NumBedroom, k=num_unique_bedrooms-1), data = house_prices) # Specify k as one less than the number of unique values
summary(gam_model)
modelsummary(gam_model) # Model Results

plot(gam_model, pages=1) # Visualization of fit
