library(dbscan)
library(readxl)
library(ggplot2)
library(dplyr)
library(factoextra)
library(tidyverse)
library(cluster)

library("ggrepel")
library("sf")
library("maps")
library("mapdata")

# read datasets
cases <- read_csv("COVID-19_cases_plus_census.csv")

# select the features
race <- cases |> 
  filter(state == 'CA') |>
  select(county_fips_code, county_name, state, total_pop, confirmed_cases, 
         deaths,white_pop,black_pop,asian_pop,hispanic_pop,amerindian_pop,other_race_pop,two_or_more_races_pop)



# convert to ratio data

race_ratio <- race


race_ratio[, 7:ncol(case)] <- case[, 7:ncol(case)] / (case$total_pop * 1000)


colnames(race_ratio)[7:ncol(race_ratio)] <- paste(colnames(race_ratio)[7:ncol(race_ratio)], "per_1000", sep = "_")

race_ratio <- race_ratio |>
  mutate(deaths = deaths / total_pop) |>
  rename(mortality_rate = deaths)

race_ratio <- race_ratio |>
  mutate(confirmed_cases = confirmed_cases / total_pop) |>
  rename(confirmed_rate = confirmed_cases)

write.csv(race_ratio, "race_data_subset.csv", row.names = FALSE)

data <- read_csv("race_data_subset.csv")




race_cluster <- data[, -(1:6)]


normalize <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

# normalize features
race_normalized <- as.data.frame(lapply(race_cluster, normalize))


county_names <- data$county_name






# k-mean find out the appropriate number of clusters with wss
set.seed(123)
wss <- numeric(20)

for(i in 1:20){
  kmeans_result <- kmeans(race_normalized, centers = i, nstart = 20)
  wss[i] <- kmeans_result$tot.withinss
}

plot(1:20, wss, type = "b", xlab = "Number of Cluster", ylab = "WSS", 
     main = "Elbow Method related to race")


# k-means
k <- 3
kmeans_result <- kmeans(race_normalized, centers = k)
print(kmeans_result$size)
print(kmeans_result$centers)

# calculate silhouette score
sil_score <- silhouette(kmeans_result$cluster, dist(race_normalized))
avg_sil_score <- mean(sil_score[, 'sil_width'])
print(avg_sil_score)

# visualization
fviz_cluster(kmeans_result, data = race_normalized, 
             centroids = TRUE, repel = TRUE, ellipse.type = "norm")

centroids_df <- as.data.frame(kmeans_result$centers)
centroids_long <- centroids_df |>
  mutate(cluster = factor(1:nrow(centroids_df))) |> 
  pivot_longer(cols = -cluster, names_to = "feature", values_to = "value") 

ggplot(centroids_long, aes(x = feature, y = value, fill = cluster)) +
  geom_bar(stat = "identity") +
  coord_flip() + 
  facet_wrap(~ cluster, scales = "free_x", ncol = 1) +  
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, hjust = 0.5)) +  
  labs(y = "Feature", x = "Value", fill = "Cluster")

fviz_silhouette(sil_score)


race_normalized$county <- county_names
race_normalized$county<- tolower(str_replace(race_normalized$county, " County", ""))

kmeans_result <- kmeans(race_normalized[, -which(names(race_normalized) == "county")], centers = k)
race_normalized$cluster <- kmeans_result$cluster

counties_ca <- tigris::counties(state = "CA", class = "sf")

geo_data <- counties_ca |> 
  mutate(NAME = tolower(NAME)) |>
  left_join(race_normalized, by = c("NAME" = "county"))

ggplot(data = geo_data) +
  geom_sf(aes(fill = factor(cluster)), color = NA) +
  scale_fill_viridis_d(option = "C") +
  labs(title = "KMeans Clusters in California", fill = "Cluster") +
  theme_minimal()


# average rates
rates <- data |>
  select(confirmed_rate, mortality_rate)
rates$cluster <- kmeans_result$cluster
average_rates <- rates |>
  group_by(cluster) |>
  summarize(avg_confirmed_rate = mean(confirmed_rate, na.rm = TRUE),
            avg_mortality_rate = mean(mortality_rate, na.rm = TRUE))

print(average_rates)

# external validation
threshold <- mean(rates$mortality_rate, na.rm = TRUE)
death_rate_labels <- ifelse(rates$mortality_rate > threshold, 1, 0)
binary_cluster_labels <- ifelse(kmeans_result$cluster == 1, 1, 0)
intersection <- sum(death_rate_labels == 1 & binary_cluster_labels == 1)
union <- sum(death_rate_labels == 1 | binary_cluster_labels == 1)
jaccard_index <- intersection / union
print(paste("Jaccard Index:", jaccard_index))












# Hierarchical Clustering

library(dplyr)
library(cluster)
library(ggplot2)
library(factoextra)
library(readr)
library(tidyverse)
library(sf)
library(tigris)
library(proxy)



d <- dist(race_normalized, method = "euclidean")

hc <- hclust(d, method = "complete")

plot(hc)
k <- 3
fviz_dend(hc, k)

clusters <- cutree(hc, k)
cluster_complete <- race_normalized |>
  mutate(cluster = factor(clusters))

fviz_cluster(list(data = race_normalized, cluster = cutree(hc, k)), geom = "point")

# calculate silhouette score
sil <- silhouette(clusters, d)

avg_sil_width <- mean(sil[, "sil_width"])
print(paste("Average silhouette width:", avg_sil_width))

fviz_silhouette(sil) + theme_minimal() +
  labs(title = paste("Silhouette Plot related to race"))

race_normalized$cluster <- as.factor(clusters)

# visualization
cluster_averages <- race_normalized |>
  group_by(cluster) |>
  summarise(across(ends_with("1000"), mean, na.rm = TRUE)) |>
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
  labs(title = "Complete linkage Clusters of CA related to race", fill = "Cluster") +
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








































