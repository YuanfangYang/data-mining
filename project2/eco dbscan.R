library(dbscan)
library(dplyr)
library(cluster)
library(ggplot2)
library(factoextra)
library(readr)
library(tidyverse)
library(sf)
library(tigris)
library(proxy)

eco <- read_csv("economic subset.csv")

county_names <- eco$county_name
rates <- eco |>
  select(confirmed_rate, mortality_rate)

# select economic features
eco_cluster <- eco[, -(1:6)]

# define normalize function
normalize <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

# normalize features
eco_normalized <- as.data.frame(lapply(eco_cluster, normalize))

kNNdistplot(eco_normalized, k = 3)
abline(h = 0.74, col = "red")

set.seed(123)
dbscan_result <- dbscan(eco_normalized, eps = 0.74, minPts = 4)

print(dbscan_result)
eco_normalized$cluster <- dbscan_result$cluster

fviz_cluster(dbscan_result, eco_normalized, geom = "point")

# calculate silhouette score
eco_normalized$cluster_adjusted <- eco_normalized$cluster
eco_normalized$cluster_adjusted[eco_normalized$cluster_adjusted == -1] <- 0

silhouette_result_adjusted <- silhouette(eco_normalized$cluster_adjusted, dist(eco_normalized))

avg_sil_width_adjusted <- mean(silhouette_result_adjusted[, 'sil_width'])
print(paste("Average silhouette width (with noise as a cluster):", avg_sil_width_adjusted))

fviz_silhouette(silhouette_result_adjusted) + theme_minimal() +
  labs(title = "Silhouette Plot for DBSCAN Clustering (With Noise as a Cluster)")



# map
counties_ca <- counties(state = "CA", class = "sf")
county_names <- str_replace(county_names, " County", "")
clusters_df <- data.frame(county = county_names, cluster = eco_normalized$cluster)
map_data <- left_join(counties_ca, clusters_df, by = c("NAME" = "county"))

ggplot(data = map_data) +
  geom_sf(aes(fill = as.factor(cluster)), color = NA) + 
  scale_fill_viridis_d(option = "C") + 
  labs(title = "DBSCAN Clusters of California Counties", fill = "Cluster") +
  theme_minimal()

# average rates
eco_normalized <- bind_cols(eco_normalized, rates)

cluster_averages <- eco_normalized |>
  group_by(cluster) |>
  summarise(avg_confirmed_rate = mean(confirmed_rate, na.rm = TRUE),
            avg_mortality_rate = mean(mortality_rate, na.rm = TRUE))

print(cluster_averages)

# external validation(jaccard)
threshold <- mean(rates$mortality_rate, na.rm = TRUE)
eco_normalized$death_rate_label <- ifelse(rates$mortality_rate > threshold, 1, 0)
cluster_binary_labels <- as.integer(eco_normalized$cluster > 0)
data_for_jaccard <- data.frame(eco_normalized$death_rate_label, cluster_binary_labels)
jaccard_distances <- proxy::dist(data_for_jaccard, method = "Jaccard")
jaccard_similarity <- 1 - as.matrix(jaccard_distances)
jaccard_index <- mean(jaccard_similarity)
print(paste("Jaccard Index:", jaccard_index))

