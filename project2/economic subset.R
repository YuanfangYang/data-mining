library(readr)
library(dplyr)

setwd("/Users/linzewen/Downloads/")

# read datasets
cases <- read_csv("COVID-19_cases_plus_census.csv")

# select the features
economic <- cases |> 
  filter(state == 'CA') |>
  select(county_fips_code, county_name, state, total_pop, confirmed_cases, 
         deaths,income_per_capita,
         income_less_10000, income_10000_14999, income_15000_19999,
         income_20000_24999, income_25000_29999, income_30000_34999,
         income_35000_39999, income_40000_44999, income_45000_49999,
         income_50000_59999, income_60000_74999, income_75000_99999,
         income_100000_124999, income_125000_149999, income_150000_199999,
         income_200000_or_more)

# convert to ratio data
economic_ratio <- economic
economic_ratio[, 8:ncol(economic)] <- economic[, 8:ncol(economic)] / economic$total_pop

colnames(economic_ratio)[8:ncol(economic_ratio)] <- paste(colnames(economic_ratio)[8:ncol(economic_ratio)], "percentage", sep = "_")

economic_ratio <- economic_ratio |>
  mutate(deaths = deaths / total_pop) |>
  rename(mortality_rate = deaths)

economic_ratio <- economic_ratio |>
  mutate(confirmed_cases = confirmed_cases / total_pop) |>
  rename(confirmed_rate = confirmed_cases)

write.csv(economic_ratio, "economic subset.csv", row.names = FALSE)
