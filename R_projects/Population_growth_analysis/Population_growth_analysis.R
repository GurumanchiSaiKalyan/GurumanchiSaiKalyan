library(ggplot2)
library(dplyr)
library(patchwork)
  

setwd("/Users/saikalyangurumanchi/Library/CloudStorage/OneDrive-CentralMichiganUniversity/Summer-581/Assignement-2")
#Importing the CSV data into R
City_by_population <- read.csv("World Populated Cities.csv")
View(City_by_population)

#Data Cleaning
#Viewing current column names
colnames(City_by_population)
#Changing column names
new_names <- c("Rank", "City", "Country", "Population_2024", "Population_2023", "Growth_Rate")
#Assigning the new column names to the data set
colnames(City_by_population) <- new_names 
#Checking the datatypes of the dataframe
glimpse(City_by_population) 
#Checking for N/A or missing values
sum(is.na(City_by_population)) 
#removing duplicates
City_by_population <- City_by_population[!duplicated(City_by_population), ] 
#For the first row the rank was 0 so added with 1 to the column make it as 1
City_by_population$Rank = City_by_population$Rank+1 
#creating new column to show the data in millions for Population_2024 column
City_by_population$Population_2024_Millions = round(City_by_population$Population_2024 / 1000000, 2)
#creating new column to show the data in millions for Population_2023 column
City_by_population$Population_2023_Millions = round(City_by_population$Population_2023 / 1000000, 2)
#Removing Population_2024 and Population_2023 columns from the dataset
City_by_population <- City_by_population[, !names(City_by_population) == "Population_2024"] 
City_by_population <- City_by_population[, !names(City_by_population) == "Population_2023"] 


#Comparison of Population in 2023 and 2024 for Top 10 Cities:
# Select the top 10 cities based on the population in 2024
top_10_cities_2024 <- City_by_population %>%
  arrange(desc(Population_2024_Millions)) %>%
  head(10)

# Plot for 2024
plot_2024 <- ggplot(top_10_cities_2024, aes(x = reorder(City, -Population_2024_Millions), y = Population_2024_Millions)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Top 10 Cities by Population in 2024",
       x = "City",
       y = "Population (Millions)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Plot for 2023
top_10_cities_2023 <- City_by_population %>%
  arrange(desc(Population_2023_Millions)) %>%
  head(10)

plot_2023 <- ggplot(top_10_cities_2023, aes(x = reorder(City, -Population_2023_Millions), y = Population_2023_Millions)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Top 10 Cities by Population in 2023",
       x = "City",
       y = "Population (Millions)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Combine the two plots side by side
combined_plot <- plot_2024 + plot_2023

# Display the combined plot
combined_plot


#Top 10 countries who has most populated cities
top_countries <- City_by_population %>%
  group_by(Country) %>%
  summarise(City_Count = n()) %>%
  arrange(desc(City_Count)) %>%
  top_n(10, City_Count)
ggplot(top_countries, aes(x = reorder(Country, -City_Count), y = City_Count)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Count of Individual Countries in Top 100 Populated Cities (2024)",
       x = "Country",
       y = "Number of Cities") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Histogram of growth rates
ggplot(City_by_population, aes(x = Growth_Rate)) +
  geom_histogram(binwidth = 0.005, fill = "yellow", color = "black") +
  labs(title = "Distribution of Growth Rates",
       x = "Growth Rate",
       y = "Frequency") +
  theme_minimal()


# finding the highest and lowest Growth_rate cities
highest_growth <- City_by_population[City_by_population$Growth_Rate == max(City_by_population$Growth_Rate), ]
lowest_growth <- City_by_population[City_by_population$Growth_Rate == min(City_by_population$Growth_Rate), ]

# Create a data frame for highest and lowest growth rate cities
highest_lowest_growth <- rbind(highest_growth, lowest_growth)

# Convert City to a factor with specified levels for ordering in the plot
highest_lowest_growth$City <- factor(highest_lowest_growth$City, levels = highest_lowest_growth$City)

# Create the bar plot
ggplot(highest_lowest_growth, aes(x = City, y = Growth_Rate, fill = Growth_Rate > 0)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("black", "grey"), guide = FALSE) +
  labs(title = "Highest and Lowest Growth Rate Cities",
       x = "City",
       y = "Growth Rate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5))


#Scatter Plot of Population Growth Rate vs. Population in 2024
ggplot(City_by_population, aes(x = Population_2024_Millions, y = Growth_Rate)) +
  geom_point(color = "purple") +
  labs(title = "Population Growth Rate vs. Population in 2024",
       x = "Population in 2024 (Millions)",
       y = "Growth Rate")

#Scatter Plot of Population Growth Rate vs. Population in 2023
  
ggplot(City_by_population, aes(x = Population_2023_Millions, y = Growth_Rate)) +
  geom_point(color = "blue") +
  labs(title = "Population Growth Rate vs. Population in 2023",
       x = "Population in 2023 (Millions)",
       y = "Growth Rate")



