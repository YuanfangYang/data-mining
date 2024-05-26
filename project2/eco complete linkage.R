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

d <- dist(eco_normalized, method = "euclidean")

hc <- hclust(d, method = "complete")

plot(hc)
k <- 3
fviz_dend(hc, k)

clusters <- cutree(hc, k)
cluster_complete <- eco_normalized |>
  mutate(cluster = factor(clusters))

fviz_cluster(list(data = eco_normalized, cluster = cutree(hc, k)), geom = "point")

# calculate silhouette score
sil <- silhouette(clusters, d)

avg_sil_width <- mean(sil[, "sil_width"])
print(paste("Average silhouette width:", avg_sil_width))

fviz_silhouette(sil) + theme_minimal() +
  labs(title = paste("Silhouette Plot for k =", length(unique(clusters))))

eco_normalized$cluster <- as.factor(clusters)

# visualization
cluster_averages <- eco_normalized |>
  group_by(cluster) |>
  summarise(across(starts_with("income"), mean, na.rm = TRUE)) |>
  pivot_longer(cols = -cluster, names_to = "feature", values_to = "value") 

ggplot(cluster_averages, aes(x = feature, y = value, fill = cluster)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +  
  labs(x = "Feature", y = "Average Value", fill = "Cluster") +
  theme_minimal() +
  theme(legend.position = "bottom") +  
  facet_wrap(~ cluster, scales = "free", ncol = 1)

# map
counties_ca <- tigris::counties(state = "CA", class = "sf")
county_names <- str_replace(county_names, " County", "")
clusters_df <- data.frame(county = county_names, cluster = clusters)
map_data <- left_join(counties_ca, clusters_df, by = c("NAME" = "county"))

ggplot(data = map_data) +
  geom_sf(aes(fill = as.factor(cluster)), color = NA) + 
  scale_fill_viridis_d(option = "C") + 
  labs(title = "Complete linkage Clusters of California Counties", fill = "Cluster") +
  theme_minimal()

# average rates
rates$cluster <- clusters

average_rates <- rates |>
  group_by(cluster) |>
  summarise(
    avg_confirmed_rate = mean(confirmed_rate, na.rm = TRUE),
    avg_mortality_rate = mean(mortality_rate, na.rm = TRUE)
  )
print(average_rates)

# external validation
threshold <- mean(rates$mortality_rate, na.rm = TRUE)
death_rate_labels <- ifelse(rates$mortality_rate > threshold, 1, 0)
cluster_labels <- as.integer(clusters > 0) 
jaccard_distance <- proxy::dist(rbind(death_rate_labels, cluster_labels), method = "Jaccard")
jaccard_similarity <- 1 - as.matrix(jaccard_distance)[1,2]
print(paste("Jaccard Similarity Index:", jaccard_similarity))
