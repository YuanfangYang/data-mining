library(ggplot2)
library(dplyr)
library(ggrepel)
library(ggcorrplot)
library(DT)
library(readr)
library(tidyr)
library(GGally)

# read the global mobility report dataset
gmr <- read_csv("/Users/linzewen/Downloads/Global_Mobility_Report.csv",
                col_types =  cols(sub_region_2 = col_character()))

gmr <- gmr |> mutate_if(is.character, factor)
dim(gmr)

head(gmr)
summary(gmr)

# filter out the main features
selected_gmr <- gmr |> select(country_region,
                              retail_and_recreation_percent_change_from_baseline,
                              grocery_and_pharmacy_percent_change_from_baseline,
                              parks_percent_change_from_baseline,
                              transit_stations_percent_change_from_baseline,
                              workplaces_percent_change_from_baseline,
                              residential_percent_change_from_baseline)

# find out the number of NA of each country/region
na_summary <- selected_gmr |>
  group_by(country_region) |>
  summarise(total_na = sum(across(everything(), ~sum(is.na(.)), .names = "na_{.col}")))

na_summary
summary(na_summary)

ggplot(na_summary, aes(x = country_region, y = total_na)) +
  geom_col() + labs(x = "Country Region", y = "Total NA",
                    title = "Total NA Values by Country Region")

# Sort data in ascending order of the number of NA
na_summary_sorted <- na_summary |>
  arrange(total_na)

# find out the countries/region has no missing data
country_region_with_no_na <- na_summary |>
  filter(total_na == 0)

country_region_with_no_na_vector <- country_region_with_no_na$country_region

# filter out the data of all countries without missing data
filtered_region_data <- gmr |>
  filter(country_region %in% country_region_with_no_na_vector)

# eliminate duplicate data and useless feature
duplicates <- filtered_region_data[duplicated(filtered_region_data), ]
filtered_region_data <- filtered_region_data |> select(-census_fips_code)

# draw a bar plot to show how much data there is in each area
ggplot(filtered_region_data, aes(x = country_region)) +
  geom_bar() + labs(title = "Number of Data Points per Country/Region",
                    x = "Country/Region", y = "Count")

# Filter out the data of Hong Kong
gmr_HK <- gmr |>
  filter(country_region == "Hong Kong")|>
  rename(
    retail_and_recreation = retail_and_recreation_percent_change_from_baseline,
    grocery_and_pharmacy = grocery_and_pharmacy_percent_change_from_baseline,
    parks = parks_percent_change_from_baseline,
    transit_stations = transit_stations_percent_change_from_baseline,
    workplaces = workplaces_percent_change_from_baseline,
    residential = residential_percent_change_from_baseline
  )
summary(gmr_HK)

gmr_HK_long <- gmr_HK |>
  gather(key = "category", value = "percent_change",
         retail_and_recreation, grocery_and_pharmacy, parks,
         transit_stations, workplaces, residential)

# Identifying outliers
ggplot(gmr_HK_long, aes(x = category, y = percent_change, fill = category)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplot of Mobility Changes Across Different Features in Hong Kong",
       x = "Category",
       y = "Percent Change from Baseline") +
  scale_fill_brewer(palette = "Pastel1") 

# Draw a time series plot
ggplot(gmr_HK_long, aes(x = date, y = percent_change, color = category)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  theme_minimal() +
  labs(title = "Mobility Features Over Time for Hong Kong",
       y = "Percent Change from Baseline",
       color = "Mobility Category")

# US Part
US_data <- gmr |> filter(country_region == "United States")

# drop useless feature and rename columns
US_data <- US_data |> 
  select(sub_region_1, sub_region_2, census_fips_code, date, 
         retail_and_recreation = retail_and_recreation_percent_change_from_baseline, 
         grocery_and_pharmacy = grocery_and_pharmacy_percent_change_from_baseline, 
         parks = parks_percent_change_from_baseline, 
         transit_stations = transit_stations_percent_change_from_baseline, 
         workplaces = workplaces_percent_change_from_baseline, 
         residential = residential_percent_change_from_baseline)

# find out the number of NA according to each county
US_na_summary <- US_data |>
  group_by(sub_region_2) |>
  summarise(total_na = sum(across(everything(), ~sum(is.na(.)), .names = "na_{.col}")))

# find out groups has no missing data
US_data_with_no_na <- US_na_summary |>
  filter(total_na == 0)

US_data_with_no_na_vector <- US_data_with_no_na$sub_region_2

# filter out the data of all groups without missing data 
filtered_group_data <- US_data |>
  filter(sub_region_2 %in% US_data_with_no_na_vector)

# eliminate duplicate data
us_duplicates <- filtered_group_data[duplicated(filtered_group_data), ]

# TX_data <- filtered_group_data |> filter(sub_region_1 == "Texas")
CA_data <- filtered_group_data |> filter(sub_region_1 == "California")

# summary(TX_data)
summary(CA_data)

# Bexar_data <-TX_data |> filter(sub_region_2 == "Bexar County")
# 
# Bexar_long <- Bexar_data |>
#   gather(key = "category", value = "percent_change", 
#          retail_and_recreation, grocery_and_pharmacy, parks, 
#          transit_stations, workplaces, residential)
# 
# ggplot(Bexar_long, aes(x = date, y = percent_change, color = category)) + 
#   geom_line() +
#   geom_smooth(se = FALSE) + 
#   theme_minimal() +
#   labs(title = "Mobility Features Over Time for Bexar County",
#        y = "Percent Change from Baseline",
#        color = "Mobility Category")

# filter out the data of Los Angeles County
LA_data <-CA_data |> filter(sub_region_2 == "Los Angeles County")
summary(LA_data)

LA_long <- LA_data |>
  gather(key = "category", value = "percent_change", 
         retail_and_recreation, grocery_and_pharmacy, parks, 
         transit_stations, workplaces, residential)

# Identifying outliers
ggplot(LA_long, aes(x = category, y = percent_change, fill = category)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplot of Mobility Changes Across Different Features in LA",
       x = "Category",
       y = "Percent Change from Baseline") +
  scale_fill_brewer(palette = "Pastel1") 

# Draw a time series plot
ggplot(LA_long, aes(x = date, y = percent_change, color = category)) + 
  geom_line() +
  geom_smooth(se = FALSE) + 
  theme_minimal() +
  labs(title = "Mobility Features Over Time for Los Angeles County",
       y = "Percent Change from Baseline",
       color = "Mobility Category")

# Compare the trend changes in grocery and pharmacy between Hong Kong and LA
LA_grocery <- LA_data |>
  select(date, grocery_and_pharmacy) |>
  mutate(region = "Los Angeles")

HK_grocery <- gmr_HK |>
  select(date, grocery_and_pharmacy) |>
  mutate(region = "Hong Kong")

combined_grocery_data <- bind_rows(LA_grocery, HK_grocery)

ggplot(combined_grocery_data, aes(x = date, y = grocery_and_pharmacy, color = region)) +
  geom_line() +
  geom_smooth(aes(group = region), method = "loess", se = FALSE) +
  theme_minimal() +
  labs(title = "Comparison of Grocery and Pharmacy Mobility between Los Angeles and Hong Kong",
       y = "Percent Change from Baseline",
       color = "Region")

# correlation between different features in LA
LA_selected_data <- LA_data[, c("retail_and_recreation", "grocery_and_pharmacy",
                                "parks", "transit_stations", "workplaces", 
                                "residential")]

ggpairs(LA_selected_data)

