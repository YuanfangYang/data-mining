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

sil_width_values <- numeric(length = 10)  

for(k in 2:10) {
  pam_fit <- pam(eco_normalized, k)
  sil_width <- silhouette(pam_fit$clustering, dist(eco_normalized))
  sil_width_values[k] <- mean(sil_width[, 'sil_width'])
}

plot(2:10, sil_width_values[-1], type = 'b', xlab = "Number of Clusters", ylab = "Average Silhouette Width",
     main = "Average Silhouette Width for Different Numbers of Clusters")

# pam
k <- 4
pam_result <- pam(eco_normalized, k)
print(pam_result)
table(pam_result$clustering)

# calculate silhouette score
sil_width <- silhouette(pam_result$clustering, dist(eco_normalized))
avg_sil_width <- mean(sil_width[, 'sil_width'])
print(paste("Average silhouette width:", avg_sil_width))
fviz_silhouette(sil_width)

# visualization
fviz_cluster(pam_result, data = eco_normalized, 
             centroids = TRUE, repel = TRUE, ellipse.type = "norm")

# map
county_names <- str_replace(county_names, " County", "")
clusters_df <- data.frame(county = tolower(county_names), cluster = pam_result$clustering)
counties_ca <- tigris::counties(state = "CA", class = "sf") %>%
  mutate(NAME = tolower(NAME))
geo_data <- left_join(counties_ca, clusters_df, by = c("NAME" = "county"))
ggplot(data = geo_data) +
  geom_sf(aes(fill = as.factor(cluster)), color = NA) +
  scale_fill_viridis_d(option = "C") +
  labs(title = "PAM Clusters in California Counties", fill = "Cluster") +
  theme_minimal()

# average rates
rates$cluster <- pam_result$clustering
average_rates <- rates |>
  group_by(cluster) |>
  summarise(avg_confirmed_rate = mean(confirmed_rate, na.rm = TRUE),
            avg_mortality_rate = mean(mortality_rate, na.rm = TRUE))
print(average_rates)

# external validation
threshold <- mean(rates$mortality_rate, na.rm = TRUE)
death_rate_labels <- ifelse(rates$mortality_rate > threshold, 1, 0)
high_risk_cluster <- which.max(aggregate(mortality_rate ~ cluster, data = rates %>% mutate(cluster = pam_result$clustering), mean)$mortality_rate)
binary_cluster_labels <- as.integer(pam_result$clustering == high_risk_cluster)
jaccard_distance <- proxy::dist(rbind(death_rate_labels, binary_cluster_labels), method = "Jaccard")
jaccard_similarity <- 1 - as.matrix(jaccard_distance)[1, 2]
print(paste("Jaccard Similarity Index:", jaccard_similarity))

