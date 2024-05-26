library(readr)
library(cluster)
library(ggplot2)
library(factoextra)
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

# find out the appropriate number of clusters with wss
set.seed(123)
wss <- numeric(20)

for(i in 1:20){
  kmeans_result <- kmeans(eco_normalized, centers = i, nstart = 20)
  wss[i] <- kmeans_result$tot.withinss
}

plot(1:20, wss, type = "b", xlab = "Number of Cluster", ylab = "WSS", 
     main = "Elbow Method for Choosing optimal k")


# k-means
k <- 3
kmeans_result <- kmeans(eco_normalized, centers = k)
print(kmeans_result$size)
print(kmeans_result$centers)

# calculate silhouette score
sil_score <- silhouette(kmeans_result$cluster, dist(eco_normalized))
avg_sil_score <- mean(sil_score[, 'sil_width'])
print(avg_sil_score)

# visualization
fviz_cluster(kmeans_result, data = eco_normalized, 
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


eco_normalized$county <- county_names
eco_normalized$county<- tolower(str_replace(eco_normalized$county, " County", ""))

kmeans_result <- kmeans(eco_normalized[, -which(names(eco_normalized) == "county")], centers = k)
eco_normalized$cluster <- kmeans_result$cluster

# map
counties_ca <- tigris::counties(state = "CA", class = "sf")

geo_data <- counties_ca |> 
  mutate(NAME = tolower(NAME)) |>
  left_join(eco_normalized, by = c("NAME" = "county"))

ggplot(data = geo_data) +
  geom_sf(aes(fill = factor(cluster)), color = NA) +
  scale_fill_viridis_d(option = "C") +
  labs(title = "KMeans Clusters in California", fill = "Cluster") +
  theme_minimal()

# average rates
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
