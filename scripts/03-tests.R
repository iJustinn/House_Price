#### Preamble ####
# Purpose: The code defines a test suite for validating data integrity, including checks for positive calendar years, four-digit year format, non-empty cause entries, and non-negative death counts across multiple datasets within a research project.
# Author: Ziheng Zhong
# Date: 25 March 2024
# Contact: ziheng.zhong@mail.utoronto.ca
# License: MIT
# Pre-requisites: none



#### Workspace setup ####



#### load data ####
setwd("/Users/ijustin/Library/CloudStorage/Dropbox/Code/STA302/Research_Papers/Mortality_Rate_in_Alberta")
raw_data <- readr::read_csv("inputs/data/raw_data.csv")
top_5_cause_data <- read_csv("outputs/data/top_5_cause_data.csv")
summarized_top_5_cause_data <- read_csv("outputs/data/summarized_top_5_cause_data.csv")



#### raw_data testing ####


