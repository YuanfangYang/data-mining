library(readr)
library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)
library(sf)
library(scales)

setwd("/Users/Yuanfang/Downloads/")

final <- read_csv("final_dataset.csv")

# When was the first case reported?
final <- final |>
  group_by(county_name) |>
  mutate(first_confirmed_case_date = date[which.max(confirmed_cases > 0)]) |>
  ungroup()

# How (densely) populated is the county
counties_map <- map_data("county")

counties_map <- counties_map |>
  group_by(subregion)  |>
  summarise(long_range = max(long) - min(long),
            lat_range = max(lat) - min(lat)) |>
  mutate(area = long_range * lat_range * (111 * 85)) |>
  ungroup()

counties_map_processed <- counties_map |>
  mutate(subregion = paste(toupper(substring(subregion, 1, 1)), substring(subregion, 2), sep = ""),
         subregion = paste(subregion, "County", sep = " "))

county_population <- final |>
  select(county_name, total_pop) |>
  distinct()  

combined_data <- left_join(county_population, counties_map_processed, by = c("county_name" = "subregion"))

combined_data <- combined_data |>
  mutate(population_density = total_pop / area)

final <- final |>
  left_join(combined_data |> select(county_name, population_density), by = "county_name")

summary(final)

# What resources does a county have (money, hospital)?
final <- final |>
  mutate(case_fatality_rate = (deaths / confirmed_cases) * 100)

final <- final |>
  mutate(infection_rate = confirmed_cases / total_pop,
         mortality_rate  = deaths / total_pop)

# What is the social distancing response and how long did it take after the first case
final <- final |>
  group_by(county_name) |>
  mutate(avg_activity_change_7_days = mean((retail_and_recreation_percent_change_from_baseline + 
                                              grocery_and_pharmacy_percent_change_from_baseline + 
                                              parks_percent_change_from_baseline +
                                              transit_stations_percent_change_from_baseline + 
                                              workplaces_percent_change_from_baseline + 
                                              residential_percent_change_from_baseline) / 6, na.rm = TRUE)) |>
  ungroup() |>
  mutate(avg_activity_change_7_days = ifelse(is.na(avg_activity_change_7_days), 0, avg_activity_change_7_days))

# Summarize daily cumulative deaths 
daily_totals <- final |>
  group_by(date) |>
  summarise(total_deaths = sum(deaths, na.rm = TRUE))

# Calculate daily new deaths
daily_increases <- daily_totals |>
  mutate(new_deaths = total_deaths - lag(total_deaths, default = first(total_deaths)))

final <- final |>
  left_join(select(daily_increases, date, new_deaths), by = "date")
            
# Plot daily new deaths
ggplot(daily_increases, aes(x = as.Date(date), y = new_deaths)) +
  geom_line() + 
  geom_point() + 
  labs(title = "Daily New Deaths", x = "Date", y = "New Deaths") +
  theme_minimal() 


daily_totals <- final |>
  group_by(date) |>
  summarise(total_confirmed_cases = sum(confirmed_cases, na.rm = TRUE))

daily_confirmed_increases <- daily_totals |>
  mutate(new_confirmed_cases = total_confirmed_cases - lag(total_confirmed_cases, default = first(total_confirmed_cases)))

final <- final |> 
  left_join(select(daily_confirmed_increases, date, new_confirmed_cases), by = "date")

ggplot(daily_confirmed_increases, aes(x = as.Date(date), y = new_confirmed_cases)) +
  geom_line() + 
  geom_point() + 
  labs(title = "Daily New Cases", x = "Date", y = "New Cases") +
  theme_minimal() 

summary(final)

# Map Visualization of First Confirmed Date
counties <- map_data("county")

counties_ca <- counties |>
  filter(region == "california") |>
  mutate(county = tools::toTitleCase(subregion), 
         county = paste(county, "County", sep = " "))

map_data_ca <- left_join(counties_ca, final, by = c("county" = "county_name"))

ggplot(map_data_ca, aes(long, lat, group = group, fill = first_confirmed_case_date)) +
  geom_polygon() +
  scale_fill_date(low = "lightyellow", high = "darkred", na.value = NA, 
                  name = "First Case Date", 
                  date_labels = "%b %d, %Y", 
                  date_breaks = "1 month") + 
  theme_minimal() +
  labs(title = "First Confirmed Case by County in California")

# Map of Pop Density
ggplot(map_data_ca, aes(long, lat, group = group, fill = population_density)) +
  geom_polygon() +
  scale_fill_continuous(name = "Population Density", 
                        low = "lightblue", high = "darkblue", na.value = "grey") +
  labs(title = "Population Density by County in California") +
  coord_fixed(1.3) + 
  theme_void()

# Scatter Plot of Confirmed Cases vs. Population Density 
ggplot(final, aes(x = population_density, y = confirmed_cases)) +
  geom_point(aes(color = population_density), alpha = 0.5) + 
  scale_color_gradient(low = "blue", high = "red") + 
  labs(x = "Population Density (per square km)", 
       y = "Confirmed Cases",
       title = "Confirmed Cases vs. Population Density") +
  theme_minimal() +
  theme(legend.position = "none")

# Visualization of 2021/1/22 Case Fatality Rate 
specific_date <- as.Date("2021-01-22")

specific_date_data <- final[final$date == specific_date, ]

ggplot(specific_date_data, aes(x = case_fatality_rate)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(x = "Case Fatality Rate (%)", y = "Frequency", 
       title = "Distribution of Case Fatality Rates on January 22") +
  theme_minimal()

map_data_cfr <- left_join(counties_ca, specific_date_data, by = c("county" = "county_name"))

ggplot(map_data_cfr, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = case_fatality_rate), color = "white") +
  scale_fill_continuous(name = "Case Fatality Rate (%)", 
                        low = "yellow", high = "red", 
                        na.value = "grey") + # 未知数据用灰色表示
  labs(title = "Case Fatality Rate by County in California on January 22") +
  coord_fixed(1.3) +
  theme_void() +
  theme(legend.position = "bottom")

# Visualization of Mortality Rate
ggplot(final, aes(x = date, y = mortality_rate, color = county_name)) +
  geom_line() +
  labs(x = "Date", y = "Mortality Rate", title = "Mortality Rate Over Time by County") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Visualization of Infection Rate
ggplot(final, aes(x = date, y = infection_rate, color = county_name)) +
  geom_line() +
  labs(x = "Date", y = "Infection Rate", title = "Infection Rate Over Time by County") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Map of avg_activity_change_7_day
ggplot(map_data_ca, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = avg_activity_change_7_days), color = "white") +
  scale_fill_continuous(name = "Avg Activity Change (%)", 
                        low = "green", high = "red", 
                        na.value = "grey") +
  labs(title = "Average Activity Change by County") +
  coord_fixed(1.3) +
  theme_void() +
  theme(legend.position = "bottom")
