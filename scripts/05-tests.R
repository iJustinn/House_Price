#### Preamble ####
# Purpose: The code is designed for validating the integrity and constraints of house price data across various datasets using R's testing frameworks.
# Author: Ziheng Zhong
# Date: 02 April 2024
# Contact: ziheng.zhong@mail.utoronto.ca
# License: MIT
# Pre-requisites: none



#### Workspace setup ####
library(readr)
library(testthat)
library(tidyverse)



#### merged_house_price testing ####
test_that("merged_house_price testing", {
  merged_house_price<- readr::read_csv("../../House_Price/outputs/data/merged_house_price.csv")
  
  # Test if 'Year' values are positive
  expect_true(all(merged_house_price$Year > 0),
              info = "Year values are positive")
  
  # Test if 'AvgHousePrice' values are positive
  expect_true(all(merged_house_price$AvgHousePrice > 0),
              info = "AvgHousePrice values are positive")
  
  # Test if 'NumBedroom' values are positive
  expect_true(all(merged_house_price$NumBedroom > 0),
              info = "NumBedroom values are positive")
  
  # Test if 'NumBedroom' values are within range
  expect_true(all(merged_house_price$NumBedroom < 6),
              info = "NumBedroom values are within range")
  
})



#### overall data testing ####
test_that("state_house_price_by_year testing", {
  state_house_price_by_year <- readr::read_csv("../../House_Price/outputs/data/state_house_price_by_year.csv")
  state_house_price_by_month <- readr::read_csv("../../House_Price/outputs/data/state_house_price_by_month.csv")
  
  # Test if 'Year' values are positive
  expect_true(all(state_house_price_by_year$Year > 0, na.rm = TRUE),
              info = "Year values are positive")
  
  # Test if 'AvgHousePrice' values are positive
  expect_true(all(state_house_price_by_year$AvgHousePrice > 0, na.rm = TRUE),
              info = "AvgHousePrice values are positive")
  
  # Test if 'StateName' values are positive
  test_that("StateName has 50 unique values", {
    num_unique_states <- length(unique(state_house_price_by_year$StateName))
    expect_equal(num_unique_states, 50, 
                 info = "StateName has 50 unique values")
  })
  test_that("StateName has 50 unique values", {
    num_unique_states <- length(unique(state_house_price_by_month$StateName))
    expect_equal(num_unique_states, 50, 
                 info = "StateName has 50 unique values")
  })
  
  # Test if total number of columns is correct
  test_that("Correct total number of columns", {
    expect_equal(ncol(state_house_price_by_month), 291, 
                 info = "Correct total number of columns")
  })
  
})



#### 1b data testing ####
test_that("state_1b_house_price_by_year testing", {
  state_1b_house_price_by_year<- readr::read_csv("../../House_Price/outputs/data/state_1b_house_price_by_year.csv")
  state_1b_house_price_by_month<- readr::read_csv("../../House_Price/outputs/data/state_1b_house_price_by_month.csv")
  
  # Test if 'Year' values are positive
  expect_true(all(state_1b_house_price_by_year$Year > 0, na.rm = TRUE),
              info = "Year values are positive")
  
  # Test if 'AvgHousePrice' values are positive
  expect_true(all(state_1b_house_price_by_year$AvgHousePrice > 0, na.rm = TRUE),
              info = "AvgHousePrice values are positive")
  
  # Test if 'StateName' values are positive
  test_that("StateName has 50 unique values", {
    num_unique_states <- length(unique(state_1b_house_price_by_year$StateName))
    expect_equal(num_unique_states, 50, 
                 info = "StateName has 50 unique values")
  })
  test_that("StateName has 50 unique values", {
    num_unique_states <- length(unique(state_1b_house_price_by_month$StateName))
    expect_equal(num_unique_states, 50, 
                 info = "StateName has 50 unique values")
  })
  
  # Test if total number of columns is correct
  test_that("Correct total number of columns", {
    expect_equal(ncol(state_1b_house_price_by_month), 291, 
                 info = "Correct total number of columns")
  })
  
})



#### 2b data testing ####
test_that("state_2b_house_price_by_year testing", {
  state_2b_house_price_by_year<- readr::read_csv("../../House_Price/outputs/data/state_2b_house_price_by_year.csv")
  state_2b_house_price_by_month<- readr::read_csv("../../House_Price/outputs/data/state_2b_house_price_by_month.csv")
  
  # Test if 'Year' values are positive
  expect_true(all(state_2b_house_price_by_year$Year > 0, na.rm = TRUE),
              info = "Year values are positive")
  
  # Test if 'AvgHousePrice' values are positive
  expect_true(all(state_2b_house_price_by_year$AvgHousePrice > 0, na.rm = TRUE),
              info = "AvgHousePrice values are positive")
  
  # Test if 'StateName' values are positive
  test_that("StateName has 50 unique values", {
    num_unique_states <- length(unique(state_2b_house_price_by_year$StateName))
    expect_equal(num_unique_states, 50, 
                 info = "StateName has 50 unique values")
  })
  test_that("StateName has 50 unique values", {
    num_unique_states <- length(unique(state_2b_house_price_by_month$StateName))
    expect_equal(num_unique_states, 50, 
                 info = "StateName has 50 unique values")
  })
  
  # Test if total number of columns is correct
  test_that("Correct total number of columns", {
    expect_equal(ncol(state_2b_house_price_by_month), 291, 
                 info = "Correct total number of columns")
  })
  
})



#### 3b data testing ####
test_that("state_3b_house_price_by_year testing", {
  state_3b_house_price_by_year <- readr::read_csv("../../House_Price/outputs/data/state_3b_house_price_by_year.csv")
  state_3b_house_price_by_month <- readr::read_csv("../../House_Price/outputs/data/state_3b_house_price_by_month.csv")
  
  # Test if 'Year' values are positive
  expect_true(all(state_3b_house_price_by_year$Year > 0, na.rm = TRUE),
              info = "Year values are positive")
  
  # Test if 'AvgHousePrice' values are positive
  expect_true(all(state_3b_house_price_by_year$AvgHousePrice > 0, na.rm = TRUE),
              info = "AvgHousePrice values are positive")
  
  # Test if 'StateName' values are positive
  test_that("StateName has 50 unique values", {
    num_unique_states <- length(unique(state_3b_house_price_by_year$StateName))
    expect_equal(num_unique_states, 50, 
                 info = "StateName has 50 unique values")
  })
  test_that("StateName has 50 unique values", {
    num_unique_states <- length(unique(state_3b_house_price_by_month$StateName))
    expect_equal(num_unique_states, 50, 
                 info = "StateName has 50 unique values")
  })
  
  # Test if total number of columns is correct
  test_that("Correct total number of columns", {
    expect_equal(ncol(state_3b_house_price_by_month), 291, 
                 info = "Correct total number of columns")
  })
  
})



#### 4b data testing ####
test_that("state_4b_house_price_by_year testing", {
  state_4b_house_price_by_year <- readr::read_csv("../../House_Price/outputs/data/state_4b_house_price_by_year.csv")
  state_4b_house_price_by_month <- readr::read_csv("../../House_Price/outputs/data/state_4b_house_price_by_month.csv")
  
  # Test if 'Year' values are positive
  expect_true(all(state_4b_house_price_by_year$Year > 0, na.rm = TRUE),
              info = "Year values are positive")
  
  # Test if 'AvgHousePrice' values are positive
  expect_true(all(state_4b_house_price_by_year$AvgHousePrice > 0, na.rm = TRUE),
              info = "AvgHousePrice values are positive")
  
  # Test if 'StateName' values are positive
  test_that("StateName has 50 unique values", {
    num_unique_states <- length(unique(state_4b_house_price_by_year$StateName))
    expect_equal(num_unique_states, 50, 
                 info = "StateName has 50 unique values")
  })
  test_that("StateName has 50 unique values", {
    num_unique_states <- length(unique(state_4b_house_price_by_month$StateName))
    expect_equal(num_unique_states, 50, 
                 info = "StateName has 50 unique values")
  })
  
  # Test if total number of columns is correct
  test_that("Correct total number of columns", {
    expect_equal(ncol(state_4b_house_price_by_month), 291, 
                 info = "Correct total number of columns")
  })
  
})



#### 5b+ data testing ####
test_that("state_5bplus_house_price_by_year testing", {
  state_5bplus_house_price_by_year <- readr::read_csv("../../House_Price/outputs/data/state_5bplus_house_price_by_year.csv")
  state_5bplus_house_price_by_month <- readr::read_csv("../../House_Price/outputs/data/state_5bplus_house_price_by_month.csv")
  
  # Test if 'Year' values are positive
  expect_true(all(state_5bplus_house_price_by_year$Year > 0, na.rm = TRUE),
              info = "Year values are positive")
  
  # Test if 'AvgHousePrice' values are positive
  expect_true(all(state_5bplus_house_price_by_year$AvgHousePrice > 0, na.rm = TRUE),
              info = "AvgHousePrice values are positive")
  
  # Test if 'StateName' values are positive
  test_that("StateName has 50 unique values", {
    num_unique_states <- length(unique(state_5bplus_house_price_by_year$StateName))
    expect_equal(num_unique_states, 50, 
                 info = "StateName has 50 unique values")
  })
  test_that("StateName has 50 unique values", {
    num_unique_states <- length(unique(state_5bplus_house_price_by_month$StateName))
    expect_equal(num_unique_states, 50, 
                 info = "StateName has 50 unique values")
  })
  
  # Test if total number of columns is correct
  test_that("Correct total number of columns", {
    expect_equal(ncol(state_5bplus_house_price_by_month), 291, 
                 info = "Correct total number of columns")
  })
  
})
