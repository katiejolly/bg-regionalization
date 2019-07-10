library(tidyverse)

median_income <- read_csv("data/ACS_15_5YR_B19013_with_ann.csv",
  skip = 1
) %>%
  janitor::clean_names() %>%
  rename(estimate_median_income = estimate_median_household_income_in_the_past_12_months_in_2015_inflation_adjusted_dollars,
         moe_median_income = margin_of_error_median_household_income_in_the_past_12_months_in_2015_inflation_adjusted_dollars) %>%
  mutate_at(vars(contains("median")), as.numeric) %>%
  mutate(cv_median_income = (moe_median_income / 1.645)/estimate_median_income)

median_age <- read_csv("data/ACS_17_5YR_B01002_with_ann.csv", 
                       skip = 1) %>%
  janitor::clean_names() %>%
  select(1:5) %>%
  rename(moe_median_age_total = margin_of_error_median_age_total) %>%
  mutate(cv_median_age_total = (moe_median_age_total / 1.645) / estimate_median_age_total)

population <- read_csv("data/ACS_17_5YR_B01003_with_ann.csv", 
                       skip = 1) %>%
  janitor::clean_names() %>%
  rename(estimate_population = estimate_total, moe_population = margin_of_error_total) %>%
  mutate(cv_population = (moe_population / 1.645) / estimate_population)

race <- read_csv("data/ACS_17_5YR_B03002_with_ann.csv", 
                 skip = 1) %>%
  janitor::clean_names() 

race <- race %>%
  mutate(percent_white_alone = estimate_not_hispanic_or_latino_white_alone / estimate_total,
         percent_black_alone = estimate_not_hispanic_or_latino_black_or_african_american_alone / estimate_total,
         percent_asian_alone = estimate_not_hispanic_or_latino_asian_alone / estimate_total,
         percent_latino_alone = estimate_hispanic_or_latino / estimate_total)