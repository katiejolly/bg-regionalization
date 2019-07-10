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

race_full <- read_csv("data/ACS_17_5YR_B03002_with_ann.csv", 
                 skip = 1) %>%
  janitor::clean_names() 

race <- race_full %>%
  mutate(percent_white_alone = estimate_not_hispanic_or_latino_white_alone / estimate_total,
         percent_black_alone = estimate_not_hispanic_or_latino_black_or_african_american_alone / estimate_total,
         percent_asian_alone = estimate_not_hispanic_or_latino_asian_alone / estimate_total,
         percent_latino_alone = estimate_hispanic_or_latino / estimate_total,
         moe_white_alone = (1 / estimate_total)*(sqrt(margin_of_error_not_hispanic_or_latino_white_alone^2 + (percent_white_alone ^2 * margin_of_error_total ^2))),
         cv_white_alone = (moe_white_alone / 1.645) / percent_white_alone,
         moe_black_alone = (1 / estimate_total)*(sqrt(margin_of_error_not_hispanic_or_latino_black_or_african_american_alone^2 + (percent_black_alone ^2 * margin_of_error_total ^2))),
         cv_black_alone = (moe_black_alone / 1.645) / percent_black_alone,
         moe_asian_alone = (1 / estimate_total)*(sqrt(margin_of_error_not_hispanic_or_latino_asian_alone^2 + (percent_asian_alone ^2 * margin_of_error_total ^2))),
         cv_asian_alone = (moe_asian_alone / 1.645) / percent_asian_alone,
         moe_latino_alone = (1 / estimate_total)*(sqrt(margin_of_error_hispanic_or_latino^2 + (percent_latino_alone ^2 * margin_of_error_total ^2))),
         cv_latino_alone = (moe_latino_alone / 1.645) / percent_latino_alone) %>%
  select(1:3, 46:57)

built <- read_csv("data/ACS_17_5YR_B25034_with_ann.csv", 
                                       skip = 1) %>%
  janitor::clean_names()


poverty_full <- read_csv("data/ACS_17_5YR_C17002_with_ann.csv", 
                    skip = 1) %>%
  janitor::clean_names()

# poverty <- poverty_full %>% 
#   select()
#   mutate(moe_under_185 = tidycensus::moe_sum(moe = c(margin_of_error_total_under_50, margin_of_error_total_50_to_99, margin_of_error_total_1_00_to_1_24, margin_of_error_total_1_25_to_1_49, margin_of_error_total_1_50_to_1_84), estimate = c(estimate_total_under_50, estimate_total_50_to_99, estimate_total_1_00_to_1_24, estimate_total_1_25_to_1_49, estimate_total_1_50_to_1_84), na.rm = TRUE))
# 
