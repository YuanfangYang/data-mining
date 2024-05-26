install.packages("timechange")
library("tidyverse")
library("ggplot2")
library("dplyr")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("readr")
library(readr)


CA_cases <- read_csv("COVID-19_cases_CA.csv")
# Check missing values
missing_values <- sum(is.na(CA_cases))
print( missing_values)

# Check duplicate rows
duplicate_rows <- sum(duplicated(CA_cases))
print( duplicate_rows)

# Check outliers for confirmed_cases
Q1_confirmed_cases <- quantile(CA_cases$confirmed_cases, 0.25)
Q3_confirmed_cases <- quantile(CA_cases$confirmed_cases, 0.75)
print(Q1_confirmed_cases)
print(Q3_confirmed_cases)

ggplot(CA_cases, aes(x = "", y = confirmed_cases)) +
  geom_boxplot() +
  labs(title = "Boxplot of COVID-19 confirmed_cases in California", y = "confirmed_cases", x = "") 



confirmed_cases_outliers <- CA_cases %>%
  mutate(outlier = ifelse(confirmed_cases < quantile(confirmed_cases, 0.25) - 1.5 * IQR(confirmed_cases) | confirmed_cases > quantile(confirmed_cases, 0.75) + 1.5 * IQR(confirmed_cases), 1, 0)) %>%
  filter(outlier == 1)


print(paste("Number of outliers in confirmed_cases: ", nrow(confirmed_cases_outliers)))






# Check outliers for death



Q1_deaths <- quantile(CA_cases$deaths, 0.25)
Q3_deaths <- quantile(CA_cases$deaths, 0.75)
print(Q1_deaths)
print(Q3_deaths)

ggplot(CA_cases, aes(x = "", y = deaths)) +
  geom_boxplot() +
  labs(title = "Boxplot of COVID-19 Deaths in California", y = "Deaths", x = "") 


deaths_outliers <- CA_cases %>%
  mutate(outlier = ifelse(deaths < quantile(deaths, 0.25) - 1.5 * IQR(deaths) | deaths > quantile(deaths, 0.75) + 1.5 * IQR(deaths), 1, 0)) %>%
  filter(outlier == 1)


print(paste("Number of outliers in deaths: ", nrow(deaths_outliers)))









# Statistic for Confirmed Cases
confirmed_cases_range <- range(CA_cases$confirmed_cases)
confirmed_cases_mean <- mean(CA_cases$confirmed_cases)
confirmed_cases_median <- median(CA_cases$confirmed_cases)

print(confirmed_cases_range)
print(confirmed_cases_mean)
print(confirmed_cases_median)

# Statistic for Deaths
deaths_range <- range(CA_cases$deaths)
deaths_mean <- mean(CA_cases$deaths)
deaths_median <- median(CA_cases$deaths)

print(deaths_range)
print(deaths_mean)
print(deaths_median)


# Visualisation:Cumulative number of confirmed cases and deaths

library(ggplot2)
library(lubridate)

daily_totals <- aggregate(cbind(confirmed_cases, deaths) ~ date, data = CA_cases, sum)

ggplot(daily_totals, aes(x = date)) +
  geom_line(aes(y = confirmed_cases, colour = "Confirmed Cases")) +
  geom_line(aes(y = deaths, colour = "Deaths")) +
  scale_colour_manual(values = c("Confirmed Cases" = "orange", "Deaths" = "purple"))  +
  xlab("date") +
  ylab("number")+
  ggtitle("Cumulative number of confirmed cases and deaths ")



# Visualisation:Daily New COVID-19 Cases and Deaths in California

library(ggplot2)
library(dplyr)
library(readr)

CA_cases <- CA_cases %>%
  
  group_by(date) %>% 
  summarise(
    confirmed_cases = sum(confirmed_cases), 
    deaths = sum(deaths) 
  ) %>%
  mutate(
    new_cases = c(NA, diff(confirmed_cases)), 
    new_deaths = c(NA, diff(deaths)) 
  ) %>%
  na.omit()

ggplot(CA_cases, aes(x = as.Date(date))) +
  geom_line(aes(y = new_cases, colour = "New Cases")) +
  geom_line(aes(y = new_deaths, colour = "New Deaths")) +
  scale_colour_manual(values = c("New Cases" = "orange", "New Deaths" = "purple")) +
  labs(title = "Daily New COVID-19 Cases and Deaths in California",
       x = "Date",
       y = "Number of New Cases/Deaths") 

 








#Explore correlation Matrix

library(corrplot)

correlation_matrix <- cor(CA_cases[, c("confirmed_cases", "deaths")])
print(correlation_matrix)
corrplot(correlation_matrix, method="color") 


