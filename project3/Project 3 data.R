library(tidyverse)
library(pheatmap)
library(FSelector)
library(ggplot2)
library(dplyr)
library(maps)

setwd("/Users/Yuanfang/Downloads/")

cases <- read_csv("COVID-19_cases_plus_census.csv")

cases <- cases %>% mutate(
  cases_per_10000 = confirmed_cases / total_pop * 10000,
  deaths_per_10000 = deaths / total_pop * 10000,
  death_per_case = ifelse(confirmed_cases == 0, 0, deaths / confirmed_cases)
)

cases_sel <- cases %>% select(state, county_name, total_pop,
                              nonfamily_households, male_pop,        
                              female_pop, median_age, white_pop, 
                              black_pop, asian_pop, hispanic_pop, amerindian_pop,
                              commuters_by_public_transportation, 
                              households, median_income, housing_units, 
                              vacant_housing_units, 
                              percent_income_spent_on_rent,
                              employed_pop, unemployed_pop, 
                              in_school, in_undergrad_college,
                              cases_per_10000, deaths_per_10000, death_per_case)

# Delete missing values
missing_values_per_row <- rowSums(is.na(cases_sel))
cases_clean <- cases_sel[complete.cases(cases_sel), ]
summary(cases_clean)

# Normalize by population 
cases_sel_norm <- cases_clean %>% mutate(
  nonfamily_households = nonfamily_households / total_pop * 100,
  male_pop = male_pop / total_pop * 100,
  female_pop = female_pop / total_pop * 100,
  white_pop = white_pop / total_pop * 100, 
  black_pop = black_pop / total_pop * 100, 
  asian_pop = asian_pop / total_pop * 100, 
  hispanic_pop = hispanic_pop / total_pop * 100, 
  amerindian_pop = amerindian_pop / total_pop * 100,
  commuters_by_public_transportation = commuters_by_public_transportation / total_pop * 100, 
  households = households / total_pop * 100, 
  housing_units= housing_units / total_pop * 100, 
  vacant_housing_units = vacant_housing_units / total_pop * 100, 
  employed_pop = employed_pop / total_pop * 100, 
  unemployed_pop = unemployed_pop / total_pop * 100, 
  in_school = in_school / total_pop * 100, 
  in_undergrad_college = in_undergrad_college / total_pop * 100
)
summary(cases_sel_norm)

# Create a heatmap
data_for_heatmap <- cases_sel_norm %>% select(where(is.numeric))
cor_matrix <- cor(data_for_heatmap, use = "complete.obs")
pheatmap(cor_matrix, clustering_distance_rows = "euclidean",
         clustering_distance_cols = "euclidean", clustering_method = "complete",
         color = colorRampPalette(c("blue", "white", "red"))(50),
         main = "Correlation Matrix Heatmap")

# Add classification and count classes
cases_sel_norm <- cases_sel_norm %>% mutate(bad = as.factor(deaths_per_10000 > 12))
class_counts <- cases_sel_norm %>% count(bad)
print(class_counts)

# identify predictive features
cases_sel_norm$bad <- factor(cases_sel_norm$bad)
selected_features <- cases_sel_norm %>%
  select(-c(deaths_per_10000, death_per_case, cases_per_10000))
weights <- chi.squared(bad ~ ., data = selected_features) %>%
  as_tibble(rownames = "feature") %>%
  arrange(desc(attr_importance))
print(weights)

ggplot(weights, aes(x = attr_importance, y = reorder(feature, attr_importance))) +
  geom_bar(stat = "identity") +
  labs(title = "Feature Importance Using Chi-squared Test",
       x = "Importance Score",
       y = "Feature") +
  theme_minimal()

features_with_zero_importance <- weights %>%
  filter(attr_importance == 0) %>%
  pull(feature)
cases_sel_norm_filtered <- cases_sel_norm %>%
  select(-all_of(features_with_zero_importance))
print(names(cases_sel_norm_filtered))

# change the state and county name format
state_names <- c(AL = "alabama", AK = "alaska", AZ = "arizona", AR = "arkansas", CA = "california",
                 CO = "colorado", CT = "connecticut", DE = "delaware", FL = "florida", GA = "georgia",
                 HI = "hawaii", ID = "idaho", IL = "illinois", IN = "indiana", IA = "iowa",
                 KS = "kansas", KY = "kentucky", LA = "louisiana", ME = "maine", MD = "maryland",
                 MA = "massachusetts", MI = "michigan", MN = "minnesota", MS = "mississippi", MO = "missouri",
                 MT = "montana", NE = "nebraska", NV = "nevada", NH = "new hampshire", NJ = "new jersey",
                 NM = "new mexico", NY = "new york", NC = "north carolina", ND = "north dakota",
                 OH = "ohio", OK = "oklahoma", OR = "oregon", PA = "pennsylvania", RI = "rhode island",
                 SC = "south carolina", SD = "south dakota", TN = "tennessee", TX = "texas", UT = "utah",
                 VT = "vermont", VA = "virginia", WA = "washington", WV = "west virginia", WI = "wisconsin", WY = "wyoming")
cases_sel_norm_filtered <- cases_sel_norm_filtered %>%
  mutate(
    state = ifelse(state %in% names(state_names), tolower(state_names[state]), tolower(state))
  )
cases_sel_norm_filtered <- cases_sel_norm_filtered %>%
  mutate(
    county_name = tolower(gsub(" County", "", county_name)),  
    state = tolower(state)
  )

write.csv(cases_sel_norm_filtered, "P3.csv", row.names = FALSE)

# Select training set
cases_sel_norm_filtered$bad <- as.factor(cases_sel_norm_filtered$bad)
cases_sel_norm_filtered$state <- as.factor(cases_sel_norm_filtered$state)

# Calculate the proportion of 'bad' class for each state
state_bad_proportions <- cases_sel_norm_filtered %>%
  group_by(state) %>%
  summarise(
    total_count = n(),
    bad_count = sum(bad == TRUE),
    bad_proportion = bad_count / total_count
  )

training_set <- cases_sel_norm_filtered %>%
  filter(state %in% c("california", "michigan", "louisiana", "new mexico"))
training_set %>% pull(bad) %>% table()
write.csv(training_set, "training_set.csv", row.names = FALSE)

testing_set <-  cases_sel_norm_filtered %>% filter(!(state %in% c("california", "michigan", "louisiana", "new mexico")))
testing_set %>% pull(bad) %>% table()
write.csv(testing_set, "testing_set.csv", row.names = FALSE)

# Ground truth map
counties <- as_tibble(map_data("county"))
map_data <- left_join(counties, cases_sel_norm_filtered, by = c("region" = "state", "subregion" = "county_name"))

# Exclude the training set states from the visualization
map_data_excl_training <- map_data %>%
  mutate(bad = ifelse(region %in% c("california", "michigan", "louisiana", "new mexico"), "NA", as.character(bad)))

ggplot(map_data_excl_training) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = as.factor(bad)), color = "black", size = 0.1) +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "grey")) +
  coord_fixed(1.3) +
  labs(title = "USA 'bad' classification map (Excluding Training States)", fill = "Classification") +
  theme_minimal()
