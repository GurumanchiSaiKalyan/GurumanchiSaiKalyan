library(ggplot2) #For visualization
library(dplyr) #Data Manipulation
options(scipen = 999) # Helps Scientific notation in numeric outputs

setwd("/Users/saikalyangurumanchi/Library/CloudStorage/OneDrive-CentralMichiganUniversity/Acadamics/Summer-581/Final_project")
Sales_2020_2021 <- read.csv("sales_06_FY2020-21.csv")

#Data Cleaning

#Converting the date into "%Y-%m-%d" formate
Sales_2020_2021$order_date <- as.Date(Sales_2020_2021$order_date, format="%Y-%m-%d")

#filtering the sales data of 2021 year
Sales_2021 <- Sales_2020_2021 %>%
  filter(format(order_date, "%Y") == "2021")

#View(Sales_2021)
no_of_rows <- nrow(Sales_2021)
print(no_of_rows) #177213 rows

#Viewing Column names and datatype 
glimpse(Sales_2021)


#Removing the column names that are not required
Sales_2021 <- Sales_2021[, !colnames(Sales_2021) %in% c(
  'item_id', 'sku', 'bi_st', 'order_date', 'ref_num', 
  'Middle.Initial', 'Name.Prefix', 'full_name', 'price', 'qty_ordered', 'Discount_Percent', 'County', 'Place.Name')]

glimpse(Sales_2021)

#Changing the column names
new_names <- c('order_id', 'status', 'price', 'discount_amount', 'price_after_discount', 'category', 'payment_type', 'customer_id'
               , 'year', 'month', 'first_name', 'last_name', 'sex', 'age', 'E-mail', 'customer_since', 'SSN', 'phone_num','city', 'state'
               , 'zip', 'region', 'user_name')
names(Sales_2021) <- new_names #Assigning the new column name to the Sales_2021 dataset

glimpse(Sales_2021)

#Reordering the columns in the order
Sales_2021 <- Sales_2021 %>% select('user_name','customer_id','first_name', 'last_name', 'sex', 'age', 'order_id', 'category', 'price','discount_amount',
                                    'price_after_discount', 'payment_type', 'month', 'year', 'status', 'customer_since', 'SSN', 'phone_num', 'city', 'state',
                                    'zip', 'region','E-mail')



glimpse(Sales_2021)

#Changing the values in payment_type and status column which seems same

#Checking for unique values in the payment_type column 
unique_payment_type <- unique(Sales_2021$payment_type)
print(unique_payment_type)

# Transform the payment_types column into categories 
Sales_2021 <- Sales_2021 %>%
  mutate(payment_type = case_when(
    payment_type %in% c('jazzwallet', 'Easypay', 'customercredit') ~ 'Digital Wallets',
    payment_type %in% c('Payaxis', 'Easypay_MA', 'apg', 'Easypay') ~ 'Payment Gateways',
    payment_type %in% c('easypay_voucher', 'jazzvoucher') ~ 'Voucher Systems',
    payment_type == 'bankalfalah' ~ 'Banking Services',
    TRUE ~ payment_type  # Keep the original value if none of the conditions match
  ))

# View the updated data set
head(Sales_2021, 10)

# Categorize the status column
unique_status <- unique(Sales_2021$status)
print(unique_status)

Sales_2021 <- Sales_2021 %>%
  mutate(status = case_when(
    status %in% c("complete", "received", "closed", "paid") ~ "Completed or Fulfilled",
    status %in% c("processing", "pending", "payment_review", "holded") ~ "In Progress or Awaiting Action",
    status %in% c("canceled", "order_refunded", "refund") ~ "Refund or Cancellation",
    TRUE ~ status  # Fallback category for any unexpected values
  ))

# View the updated data set
head(Sales_2021, 10)

#Changing the customer_since column to date datatype and changing the format 
Sales_2021$customer_since <- as.Date(Sales_2021$customer_since, format = "%m/%d/%Y")
Sales_2021$customer_since <- format(Sales_2021$customer_since, "%Y")

# Function to mask data, showing only the last 4 digits
mask_data <- function(x) {
  if (is.character(x)) {
    masked <- sub(".+(....)$", "XXX-XX-\\1", x)
    return(masked)
  }
  return(x)
}

#Masking the SSN data and showing the last 4 digits of the data
Sales_2021$SSN <- sapply(Sales_2021$SSN, mask_data)

#Changing the data in month column from month-YYYY to Month formate
Sales_2021$month <- sub("([A-Za-z]+)-\\d+", "\\1", Sales_2021$month)

head(Sales_2021, 10)

# Find duplicates based on all columns
duplicates <- Sales_2021[duplicated(Sales_2021), ]
number_of_duplicatesrecords <- nrow(duplicates)
print(number_of_duplicatesrecords)


# Remove duplicates
Sales_2021 <- Sales_2021[!duplicated(Sales_2021), ]

#Checking No of rows after removing duplicate data
no_of_rows <- nrow(Sales_2021)
print(no_of_rows) #170021 rows

# Check for null values in each column
null_values <- sapply(Sales_2021, function(x) sum(is.na(x)))

# Print the result
print(null_values) # There is no missing data in the Data set

head(Sales_2021, 20)

#Data Analysis

#Demographics
# Age distribution
ggplot(Sales_2021, aes(x = age)) + 
  geom_histogram(binwidth = 5, fill = "blue", color = "black") + 
  labs(title = "Distribution of Age", x = "age", y = "Count")

# Gender distribution
ggplot(Sales_2021, aes(x = sex, fill = sex)) + 
  geom_bar() + 
  labs(title = "Distribution of Gender", x = "Gender", y = "Count")

#Monthly Sales Analysis of 2021 using bar chart

monthly_sales <- Sales_2021 %>%
  filter(year == 2021) %>%
  group_by(month) %>%
  summarize(total_sales = sum(price_after_discount))

# Plotting
ggplot(monthly_sales, aes(x = month, y = total_sales, fill = month)) +
  geom_col() +
  labs(title = "Total Sales by Month for 2021", x = "Month", y = "Total Sales (Price After Discount)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x labels for clarity


# Category-wise sales
ggplot(Sales_2021, aes(x = category, y = price_after_discount, fill = category)) + 
  geom_bar(stat = "summary", fun = mean) + 
  labs(title = "Average Sales by Category", x = "Category", y = "Average Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Total sales by region
ggplot(Sales_2021, aes(x = region, y = price_after_discount, fill = region)) +
  geom_bar(stat = "summary", fun = sum) +
  labs(title = "Total Sales by Region", x = "Region", y = "Total Sales After Discount") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Summarize data to find the highest discount by region
highest_discounts <- Sales_2021 %>%
  group_by(region) %>%
  summarize(highest_discount = max(discount_amount), .groups = 'drop')

# Optional: Create a bar plot of the highest discounts by region
ggplot(highest_discounts, aes(x = region, y = highest_discount, fill = region)) +
  geom_col() +
  labs(title = "Highest Discounts by Region", x = "Region", y = "Highest Discount Amount") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Geographic Distribution of Sales by state
ggplot(Sales_2021, aes(x = state, y = price_after_discount, fill = state)) +
  geom_bar(stat = "identity") +
  labs(title = "Sales by State", x = "State", y = "Total Sales") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

#We can see the texas state has the highest sales, lets check which top 10 cities in texas has the highest sales
#Filtering top 10 cities data in texas
texas_sales_10_cities <- Sales_2021 %>%
  filter(state == "TX") %>%
  group_by(city) %>%
  summarize(total_sales = sum(price_after_discount)) %>%
  arrange(desc(total_sales)) %>%
  slice_head(n = 10) 

# Create a line graph of sales for the top 10 cities in Texas
ggplot(texas_sales_10_cities, aes(x = city, y = total_sales, group = 1)) +
  geom_line() +
  geom_point() +  # Add points for better visibility of each city
  labs(title = "Line Graph of Sales by Top 10 Cities in Texas", x = "City", y = "Total Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Filtering records of texas city and calculating the sum of sales and find out the highest sold category with respective to gender
texas_sales <- Sales_2021 %>%
  filter(state == "TX") %>%
  group_by(sex, category) %>%
  summarize(total_sales = sum(price_after_discount), .groups = 'drop')

# Create a bar plot of sales by category and gender in Texas
ggplot(texas_sales, aes(x = category, y = total_sales, fill = sex)) +
  geom_col(position = "dodge") +
  labs(title = "Sales by Category and Gender in Texas", x = "Category", y = "Total Sales") +
  scale_fill_brewer(palette = "Paired") +  # Color palette for distinction
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Add a categorization column based on customer_since
sales_data <- Sales_2021 %>%
  mutate(customer_group = ifelse(customer_since < 2000, "Before 2000", "2000 and After"))

# Calculate average price_after_discount for each group
average_prices <- sales_data %>%
  group_by(customer_group) %>%
  summarize(average_price_after_discount = mean(price_after_discount), .groups = 'drop')

# Output the results
print(average_prices)

# Create a bar chart to visualize average prices
ggplot(average_prices, aes(x = customer_group, y = average_price_after_discount, fill = customer_group)) +
  geom_col() +  # geom_col is used for bar charts where bars start at 0
  labs(title = "Average Price After Discount by Customer Group", 
       x = "Customer Group", 
       y = "Average Price After Discount") +
  scale_fill_brewer(palette = "Pastel1") +  # Aesthetic color setting
  theme_minimal() +  # Minimal theme for a clean look
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))  # Adjust text alignment if needed


# State-wise Distribution of Order Statuses
status_distribution <- Sales_2021 %>%
  group_by(region, status) %>%
  summarize(count = n(), .groups = 'drop') %>%
  mutate(percentage = count / sum(count) * 100)

# Visualization Proportional Bar Chart
ggplot(status_distribution, aes(x = region, y = percentage, fill = status)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "State-wise Distribution of Order Statuses", x = "Region", y = "Percentage (%)")

Sales_2021$date <- as.Date(with(Sales_2021, paste(year, month, "01", sep = "-")), "%Y-%b-%d")

library(ggplot2)
ggplot(Sales_2021, aes(x = date, y = price_after_discount)) +
  geom_point(color = "blue", size = 3, alpha = 0.6) +
  labs(title = "Sales Trends Over Time", x = "Date", y = "Sales (After Discount)") +
  theme_minimal()

#Modeling

# Convert 'status' to a binary factor
Sales_2021$status_binary <- ifelse(Sales_2021$status == "Refund or Cancellation", 1, 0)

# Logistic regression model to predict order status
model <- glm(status_binary ~ price + discount_amount + category + payment_type + month + year + age + region, 
             data = Sales_2021, 
             family = binomial)

# Summary of the model
summary(model)

# Add predicted probabilities to the data frame
Sales_2021$predicted_probabilities <- predict(model, type = "response")

# Plot the predicted probabilities against one of the predictors, e.g., price
library(ggplot2)

ggplot(Sales_2021, aes(x = price, y = predicted_probabilities)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(title = "Predicted Probability of Refund or Cancellation vs. Price",
       x = "Price",
       y = "Predicted Probability of Refund or Cancellation")




