library(readr)
library(dplyr)

setwd("/Users/linzewen/Downloads/")

# read datasets
cases <- read_csv("COVID-19_cases_plus_census.csv")
gmr <- read_csv("Global_Mobility_Report.csv",
                col_types =  cols(sub_region_2 = col_character()))
CA <- read_csv("COVID-19_cases_CA.csv")

# filter out features from COVID-19_cases_CA.csv
CA <- CA |> filter(county_fips_code != "00000")
CA <- CA |> select(- state, - state_fips_code)
CA <- CA |> 
  filter(date >= as.Date("2020-02-15") & date <= as.Date("2021-01-22"))

# filter out features and CA data from Global_Mobility_Report.csv
CA_gmr <- gmr |>
  filter(country_region == "United States", sub_region_1 == "California")
CA_gmr <- CA_gmr |> select(sub_region_2, census_fips_code, date,
                           retail_and_recreation_percent_change_from_baseline,
                           grocery_and_pharmacy_percent_change_from_baseline,
                           parks_percent_change_from_baseline,
                           transit_stations_percent_change_from_baseline,
                           workplaces_percent_change_from_baseline,
                           residential_percent_change_from_baseline)
CA_gmr <- CA_gmr |>
  filter(!is.na(census_fips_code))

# filter out features and CA data from COVID-19_cases_plus_census.csv
CA_cases <- cases |> filter(state == "CA")
CA_cases <- CA_cases |> select(county_name, county_fips_code, date, total_pop)


# combine first two datasets
combined_data <- left_join(CA, CA_cases, by = c("county_fips_code", 
                                                "county_name","date"))
# Group by 'county_name' and fill in missing 'total_pop' for each county with the first available value for that county.
combined_data <- combined_data |>
  group_by(county_name) |>
  mutate(total_pop = first(na.omit(total_pop))) |>
  ungroup() 

# combine the last dataset
final_dataset <- left_join(combined_data, CA_gmr, by = c( "date",
                                                          "county_name" = "sub_region_2",
                                                          "county_fips_code" = "census_fips_code"))

write.csv(final_dataset,"final_dataset.csv", row.names = FALSE)
