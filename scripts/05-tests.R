#### Preamble ####
# Purpose: The code defines a test suite for validating data integrity, including checks for positive calendar years, four-digit year format, non-empty cause entries, and non-negative death counts across multiple datasets within a research project.
# Author: Ziheng Zhong
# Date: 25 March 2024
# Contact: ziheng.zhong@mail.utoronto.ca
# License: MIT
# Pre-requisites: none



#### Workspace setup ####
library(readr)
library(testthat)
library(tidyverse)



#### cali_house_price_clean testing ####
test_that("cali_house_price_clean testing", {
  cali_house_price_clean<- readr::read_csv("../../House_Price/outputs/data/cali_house_price_clean.csv")
  
  # Test if 'longitude' values are within expected range
  expect_true(all(cali_house_price_clean$longitude >= -180 & cali_house_price_clean$longitude <= 180), 
              info = "Longitude values are within expected range")
  
  # Test if 'latitude' values are within expected range
  expect_true(all(cali_house_price_clean$latitude >= -90 & cali_house_price_clean$latitude <= 90),
              info = "Latitude values are within expected range")
  
  # Test if 'housing_median_age' values are positive
  expect_true(all(cali_house_price_clean$housing_median_age > 0),
              info = "Housing median age values are positive")
  
  # Test if 'total_rooms' values are positive
  expect_true(all(cali_house_price_clean$total_rooms > 0),
              info = "Total rooms values are positive")
  
  # Test if 'total_bedrooms' values are positive
  expect_true(all(cali_house_price_clean$total_bedrooms > 0),
              info = "Total bedrooms values are positive")
  
  # Test if 'population' values are positive
  expect_true(all(cali_house_price_clean$population > 0),
              info = "Population values are positive")
  
  # Test if 'households' values are positive
  expect_true(all(cali_house_price_clean$households > 0),
              info = "Households values are positive")
  
  # Test if 'median_income' values are positive
  expect_true(all(cali_house_price_clean$median_income > 0),
              info = "Median income values are positive")
  
  # Test if 'median_house_value' values are positive
  expect_true(all(cali_house_price_clean$median_house_value > 0),
              info = "Median house value values are positive")
  
  # Test if 'ocean_proximity' contains expected categories
  expected_categories <- c("NEAR BAY", "ISLAND", "INLAND", "NEAR OCEAN", "<1H OCEAN")
  expect_true(all(cali_house_price_clean$ocean_proximity %in% expected_categories),
              info = "Ocean proximity contains specific categories")
})



#### ocean_proximity_frequency testing ####
test_that("ocean_proximity_frequency testing", {
  ocean_proximity_frequency<- readr::read_csv("../../House_Price/outputs/data/ocean_proximity_frequency.csv")
  
  # Test that the dataset has the expected columns
  expected_cols <- c("Variable", "Frequency")
  expect_equal(colnames(ocean_proximity_frequency), expected_cols, 
               info = "Dataset has the correct columns")
  
  # Test that the 'Variable' column contains specific expected categories
  expected_categories <- c("NEAR BAY", "ISLAND", "INLAND", "NEAR OCEAN", "<1H OCEAN")
  expect_true(all(ocean_proximity_frequency$Variable %in% expected_categories),
              info = "Variable column contains all expected categories")
  
  # Test that the 'Frequency' column contains only positive integers
  expect_true(all(ocean_proximity_frequency$Frequency > 0 & ocean_proximity_frequency$Frequency == floor(ocean_proximity_frequency$Frequency)),
              info = "Frequency column contains only positive integers")
  
  # Test that the dataset does not contain any NA values
  expect_true(!any(is.na(ocean_proximity_frequency)),
              info = "Dataset contains no NA values")
})












