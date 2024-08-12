#Sai Kalyan Gurumanchi Exploratory Data Analysis Assignment-1
# Install necessary packages
if (!require("readxl")) install.packages("readxl")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")

# Load the libraries
library(readxl)
library(ggplot2)
library(dplyr)

#1
# Load the data
sales_data <- read_excel("Sales_orders.xlsx") %>%

# Calculate statistics for 'Quantity'
mean_quantity <- mean(sales_data$Quantity)
median_quantity <- median(sales_data$Quantity)
std_quantity <- sd(sales_data$Quantity)
skewness_quantity <- skewness(sales_data$Quantity)

# Print the statistics
print(paste("Mean Quantity:", mean_quantity))
print(paste("Median Quantity:", median_quantity))
print(paste("Standard Deviation:", std_quantity))
print(paste("Skewness:", skewness_quantity))

# Histogram of Quantity
hist(sales_data$Quantity, breaks = 20, main = "Histogram of Quantity", xlab = "Quantity", col = "orange")


# Boxplot of Quantity
boxplot(sales_data$Quantity, main = "Boxplot of Quantity", ylab = "Quantity", col = "purple")



#2
# Create Box Plot for 'Quantity' by 'Sub-category'
ggplot(sales_data, aes(x = `Sub-Category`, y = Quantity)) +
  geom_boxplot(aes(fill = 'Sub-Category')) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))  
  labs(title = "Box Plot of Quantity by Subcategory", x = "Subcategory", y = "Quantity")


#3.1
profit_summary <- sales_data %>%
  group_by(`Sub-Category`) %>%
  summarise(
    Mean_Profit = mean(Profit),
    Median_Profit = median(Profit),
    SD_Profit = sd(Profit)
  )

# Print summary statistics
print(profit_summary)

#3.2
ggplot(sales_data, aes(x = `Sub-Category`, y = Profit)) +
  geom_boxplot(fill = "orange", color = "black", alpha = 1) +
  ggtitle("Box Plot of Profit by SubCategory") +
  xlab("Sub-Category") +
  ylab("Profit") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))

