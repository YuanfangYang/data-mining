## Xingya Wang ID: 48739514
#COVID-19_cases_plus_census.csv

install.packages("corrplot")
install.packages("officer")
install.packages("flextable")
install.packages("maps")
install.packages("mapdata")
install.packages("ggrepel")

library("tidyverse")
library("ggplot2")
library("dplyr")
library("corrplot")
library("ggcorrplot")
library("officer")
library("flextable")
library("ggrepel")

library("maps")
library("mapdata")

cases <- read_csv("COVID-19_cases_plus_census.csv")
cases <- cases %>% mutate(total_income = total_pop * income_per_capita)
summary(cases)


cases_col <- cases %>% select(state, 
                              confirmed_cases, 
                              deaths, 
                              total_pop, 
                              median_age,
                              poverty,
                              worked_at_home,
                              total_income)

summary(cases_col)
grouped_cases_col <- cases_col %>%
  group_by(state) %>%
  summarise(median_age = median(median_age),
            total_income = sum(total_income),
            total_pop = sum(total_pop),
            poverty = sum(poverty),
            worked_at_home = sum(worked_at_home),
            confirmed_cases = sum(confirmed_cases),
            deaths = sum(deaths))

# Add 4 more columns
grouped_cases_col <- grouped_cases_col %>% 
  mutate(avg_income = total_income / total_pop,
         infection_rate = confirmed_cases / total_pop * 100,
         mortality_rate = deaths / confirmed_cases * 1000)

# Remove total income column
grouped_cases_col <- grouped_cases_col %>%
  select(-total_income)

print(grouped_cases_col)
summary(grouped_cases_col)

summary_data <- summary(grouped_cases_col)
summary_df <- data.frame(grouped_cases_col)
doc <- read_docx()  # Create a new Word document
flex_table <- flextable(summary_df)
doc <- doc %>%
  body_add_flextable(flex_table)

print(doc, target = "summary_table.docx")

grouped_cases_col <- grouped_cases_col %>% mutate_if(is.character, factor)
dim(grouped_cases_col)
str(grouped_cases_col)

# Check missing values
missing_values <- colSums(is.na(grouped_cases_col))
print(missing_values)

# Check duplicate rows
duplicate_rows <- data[duplicated(grouped_cases_col), ]

print(duplicate_rows)

# Check outliers
# Select numeric variables for boxplot and add state column
boxplot_data <- grouped_cases_col %>%
  select(-state) %>% # Remove 'state' column for boxplot
  mutate(state = grouped_cases_col$state) # Add 'state' column back

# Boxplot for each numeric variable
boxplot(boxplot_data[, -ncol(boxplot_data)], main = "Boxplot of Numeric Variables")

# Check outliers using IQR method by state
outliers <- boxplot_data %>%
  gather(variable, value, -state) %>%
  group_by(variable) %>%
  mutate(outlier = ifelse(value < quantile(value, 0.25) - 1.5 * IQR(value) | value > quantile(value, 0.75) + 1.5 * IQR(value), 1, 0)) %>%
  filter(outlier == 1) %>%
  distinct(state) %>%
  ungroup()

print(outliers)


#Relationship between cases and deaths
ggplot(grouped_cases_col, mapping = aes(x = confirmed_cases, y = deaths, label = state)) + 
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = total_pop), color = "grey") + 
  geom_text_repel(data = subset(grouped_cases_col, deaths >= 1000)) 


# Does death per case depend on population?
ggplot(grouped_cases_col, mapping = aes(x= total_pop, y = mortality_rate, label = state)) + 
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = total_pop), color = "grey") + 
  geom_text_repel(data = subset(grouped_cases_col, mortality_rate > quantile(mortality_rate, .95) | mortality_rate < quantile(mortality_rate, .05)))


#COVID-19 top 10 states with highest infection rate
covid_data_filtered <- grouped_cases_col %>%
  arrange(desc(infection_rate)) %>%
  head(10)

# Create the bar chart using ggplot2
bar_chart <- ggplot(covid_data_filtered, aes(x = reorder(state, -infection_rate), y = infection_rate)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +  # Create bars for infection rate
  geom_text(aes(label = round(infection_rate, 2)), vjust = -0.5, size = 3) +  # Add labels for infection rate
  labs(title = "Top 10 states by Infection Rate",
       x = "State",
       y = "Infection Rate") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  # Rotate x-axis labels for better readability

# Print the bar chart
print(bar_chart)

# Display top 10 states with highest population
top_5_states <- grouped_cases_col %>%
  arrange(desc(total_pop)) %>%
  head(10)

# Plot the top 10 states
ggplot(top_5_states, aes(x = reorder(state, -total_pop), y = total_pop)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "State", y = "Population", title = "Top 10 States by Population") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()


#COVID-19 top 10 states with highest death rate
covid_data_filtered <- grouped_cases_col %>%
  arrange(desc(mortality_rate)) %>%
  head(10)

# Create the bar chart using ggplot2
bar_chart <- ggplot(covid_data_filtered, aes(x = reorder(state, -mortality_rate), y = mortality_rate)) +
  geom_bar(stat = "identity", fill = "red", alpha = 0.7) +  # Create bars for infection rate
  geom_text(aes(label = round(mortality_rate, 2)), vjust = -0.5, size = 3) +  # Add labels for infection rate
  labs(title = "Top 10 states by Mortality Rate",
       x = "State",
       y = "Mortality Rate") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  # Rotate x-axis labels for better readability

# Print the bar chart
print(bar_chart)

#COVID-19 top 10 states with highest avg income
covid_data_filtered <- grouped_cases_col %>%
  arrange(desc(avg_income)) %>%
  head(10)

# Create the bar chart using ggplot2
bar_chart <- ggplot(covid_data_filtered, aes(x = reorder(state, -avg_income), y = avg_income)) +
  geom_bar(stat = "identity", fill = "darkgreen", alpha = 0.7) +  # Create bars for infection rate
  geom_text(aes(label = round(avg_income, 2)), vjust = -0.5, size = 3) +  # Add labels for infection rate
  labs(title = "Top 10 states by avg income",
       x = "State",
       y = "Average Income") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  # Rotate x-axis labels for better readability

# Print the bar chart
print(bar_chart)
grouped_cases_col

#Explore correlation Matrix
correlation_matrix <- cor(data[, c("confirmed_cases", "deaths", "total_pop", "avg_income", "median_age", "poverty", "worked_at_home", "infection_rate", "mortality_rate")])
print(correlation_matrix)
corrplot(correlation_matrix, method = "color")

# CA
cases_CA_select <- cases %>% 
    filter(state == "CA")  %>% 
    select(county_name, confirmed_cases, deaths, total_pop)

cases_CA_select <- cases_CA_select %>%
  rename(county = county_name)
# Add two new features
cases_CA_select <- cases_CA_select %>% mutate(
  cases_per_1000 = confirmed_cases/total_pop*1000, 
  deaths_per_1000 = deaths/total_pop*1000, 
  death_per_case = deaths/confirmed_cases)

cases_CA_select

sorted_CA <- cases_CA_select  %>%
  arrange(desc(deaths_per_1000))

sorted_CA

# Load county map data
counties <- as_tibble(map_data("county"))

counties_CA <- counties %>%
  filter(region == "california") %>% 
  rename(c(county = subregion))

# Convert county_name to lower case and remove 'county' from the name
cases_CA <- cases_CA_select %>% 
  mutate(county = county %>% 
  str_to_lower() %>% 
  str_replace('\\s+county\\s*$', ''))

counties_CA <- counties_CA %>% 
  left_join(cases_CA %>% 
  select(county, cases_per_1000, deaths_per_1000, death_per_case), by = "county")

top_5_counties <- cases_CA %>%
  arrange(desc(cases_per_1000)) %>%
  select(county) %>%
  head(5)

top_5_counties
# Graph the map of CA
ggplot(counties_CA, aes(long, lat, label = county)) + 
  geom_polygon(aes(group = group, fill = cases_per_1000)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "COVID-19 Cases per 1000 People", subtitle = "California")

# Exceptional Work
cases_extra <- read_csv("all-states-history-updated.csv")
cases_extra <- cases_extra %>% select(date, 
                              state, 
                              deathIncrease, 
                              hospitalizedIncrease,
                              positiveIncrease,
                              totalTestResultsIncrease)

# Replace all negative number to zero
cases_extra <- cases_extra %>%
  mutate(across(everything(), ~ replace(., . < 0, 0)))
summary(cases_extra)
cases_extra

# Parse date
cases_extra <- cases_extra %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"),  # Convert character date to Date type
         date_formatted = format(date, "%Y-%m-%d")) 

cases_extra
summary(cases_extra)


# Tread analysis for daily
grouped_data_date <- cases_extra %>%
  group_by(date) %>%
  summarise(total_deaths = sum(deathIncrease),
            total_positive_tests = sum(positiveIncrease),
            total_hospitalizations = sum(hospitalizedIncrease),
            total_TestResults = sum(totalTestResultsIncrease))



str(grouped_data_date)
# graph the trend analysis
ggplot() +
  geom_line(data = grouped_data_date, aes(x = date, y = total_deaths, color = "Daily Death")) +
  geom_line(data = grouped_data_date, aes(x = date, y = total_positive_tests, color = "Daily Test Postive")) +  # Replace "new_value" with your actual data
  geom_line(data = grouped_data_date, aes(x = date, y = total_hospitalizations, color = "Daily hospitalizations")) +
  # geom_smooth(data = grouped_data_date, aes(x = date, y = total_deaths), se = FALSE) +  # Include smooth line for "Group 1" only (optional)
  labs(title = "Trend Analysis", 
       x = "Date", 
       y = "Total Deaths/Test Postive/hospitalizations", 
       color = "Data Group") +  # Label for color legend
  theme_bw()

# Tread analysis for each state
ggplot() +
  geom_line(data = grouped_data_date, aes(x = date, y = total_TestResults, color = "TestResults")) +
  # geom_smooth(data = grouped_data_date, aes(x = date, y = total_deaths), se = FALSE) +  # Include smooth line for "Group 1" only (optional)
  labs(title = "Trend Analysis", 
       x = "Date", 
       y = "Daily Test for COVID-19", 
       color = "Data Group") +  # Label for color legend
  theme_bw()

#Part 3 create a new feature that has daily confirmed cases
final_data <- read_csv("final_dataset.csv")

daily_totals <- final_data %>%
  group_by(date) %>%
  summarise(total_confirmed_cases = sum(confirmed_cases, na.rm = TRUE))

daily_increases <- daily_totals %>%
  mutate(new_confirmed_cases = total_confirmed_cases - lag(total_confirmed_cases, default = first(total_confirmed_cases)))

# Visualize the daily new confirmed cases in CA
ggplot(daily_increases, aes(x = as.Date(date), y = new_confirmed_cases)) +
  geom_line() + 
  geom_point() + 
  labs(title = "Daily New Cases", x = "Date", y = "New Cases") +
  theme_minimal() 


