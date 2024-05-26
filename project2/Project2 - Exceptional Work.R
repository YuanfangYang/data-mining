library(dbscan)
library(readxl)
library(ggplot2)
library(dplyr)
library(factoextra)
library(tidyverse)
library(cluster)
library("openxlsx")


library("ggrepel")
library("sf")
library("maps")
library("mapdata")

# Load the dataset 
data <- read_excel("CA race only COVID-19_cases_plus_census.xlsx")
str(data)

# Perform any necessary preprocessing steps, remove some columns
data <- data %>%
  select(-county_fips_code, -state, -state_fips_code, -date)
str(data)

# Prepare the data for all race
prepared_data_race <- data[, c(
                                "white_pop",	
                                "black_pop",	
                                "asian_pop",	
                                "hispanic_pop",	
                                "amerindian_pop",	
                                "other_race_pop",	
                                "two_or_more_races_pop")]
# Scale the data
prepared_data_race_scaled <- as.data.frame(scale(prepared_data_race))
str(prepared_data_race_scaled)

# Choose parameters for DBSCAN
# Display k-nearest neighbors (kNN) distance plot 
kNNdistplot(prepared_data_race_scaled, k = 3)
abline(h = 3, col = "red")

# Display DBSCAN cluster plot
db <- dbscan(prepared_data_race_scaled, eps = 2, minPts = 2)
fviz_cluster(db, prepared_data_race_scaled, geom = "point")

# Visual silhouette plot for DBSCAN
d <- dist(prepared_data_race_scaled)
plot(silhouette(db$cluster, d))
fviz_silhouette(silhouette(db$cluster, d))


# Visual the DBSCAN on map
# Load county map data
counties <- as_tibble(map_data("county"))

counties_CA <- counties %>%
  filter(region == "california") %>% 
  rename(c(county = subregion))

counties_CA

CA_data <- data %>% mutate(county = county_name %>% 
                             str_to_lower() %>% str_replace('\\s+county\\s*$', ''))
CA_data



race_hot <- counties_CA %>% left_join(CA_data %>% 
                                        mutate(cluster = factor(db$cluster)))

# Group the data by county to show each county name only once
county_label <- race_hot %>%
  group_by(county_name) %>%
  summarise_all(mean)  # Taking mean of lat and long to get a single point per county

# Plot the map with county polygons and county names
ggplot() +
  geom_polygon(data = race_hot, aes(long, lat, group = group, fill = cluster)) +
  geom_text(data = county_label, aes(long, lat, label = county_name), size = 3, color = "black", check_overlap = TRUE) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "Clusters", subtitle = "DBSCAN map related to races")

# Display top 5 counties with highest population
top_5_county <- data %>%
  arrange(desc(total_pop)) %>%
  head(5)

# Plot the top 5 counties
ggplot(top_5_county, aes(x = reorder(county_name, -total_pop), y = total_pop/1000)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "County", y = "Population in 1000", title = "Top 5 county by Population") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()

# PAM
d <- dist(prepared_data_race_scaled)
str(d)
p <- pam(d, k = 4)


medoids <- as_tibble(prepared_data_race_scaled[p$medoids, ], rownames = "cluster")
medoids

# Display PAM cluster plot
fviz_cluster(c(p, list(data = prepared_data_race_scaled)), geom = "point", ellipse.type = "norm")




race_hot_PAM <- counties_CA %>% left_join(CA_data %>% 
                                        mutate(cluster = factor(p$cluster)))
print(p$cluster)
# Group the data by county to show each county name only once
county_label_PAM <- race_hot_PAM %>%
  group_by(county_name) %>%
  summarise_all(mean)  

county_label_PAM
write.xlsx(race_hot_PAM, "race_hot_PAM_output.xlsx", rowNames = FALSE)
# Plot the map with county polygons and county names
ggplot() +
  geom_polygon(data = race_hot_PAM, aes(long, lat, group = group, fill = cluster)) +
  geom_text(data = county_label_PAM, aes(long, lat, label = county_name), size = 3, color = "black", check_overlap = TRUE) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "Clusters", subtitle = "PAM map related to races")

# Visual silhouette plot for PAM
d <- dist(prepared_data_race_scaled)
plot(silhouette(p$cluster, d))
fviz_silhouette(silhouette(p$cluster, d))

# Simpson Diversity Index
data_without_county <- data %>% 
  select(-county_name, -total_pop)

data_proportions <- data_without_county %>%
  mutate_all(~ ./sum(.)) 

Simpson <- 1 - rowSums(data_proportions^2)  # Calculate Simpson Diversity Index

# Calculate the Simpson Diversity Index for each county
Shannon <- -rowSums(data_proportions * log(data_proportions))  


# Add back the county name column
data_proportions_with_county <- cbind(data$county_name, data_proportions)

# Add diversity indices to the dataframe
data_proportions_with_county$Simpson <- Simpson

print(data_proportions_with_county)

write.xlsx(data_proportions_with_county, "data_proportions_with_county.xlsx", rowNames = FALSE)
