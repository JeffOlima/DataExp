
library(ggplot2)
library(dplyr)

# Load the CSV file into R
dataframe <- read.csv("./CriminalDataset.csv")

# Task A

# Categorical variables (Statistic, Statistic Label, Garda Station, Type of Offence and Unit)
# Discrete variables (TList, Year, Station Code, Identifier and Value)
# Continuous variables ( )

# Create a bar chart to Visualize the total occurrences for each type of offence

# Aggregate the VALUE variable by summing it up for each Identifier
aggregated_data <- dataframe %>%
  group_by(Type.of.Offence) %>%
  summarise(Total_Value = sum(VALUE))

# Create a bar chart
ggplot(aggregated_data, aes(x = reorder(Type.of.Offence, Total_Value), y = Total_Value)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Occurrences of Offenses by Type of Offence",
       x = "Type of Offence",
       y = "Total Occurrences") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(labels = scales::comma)

# Check for missing values in each column 
colSums(is.na(dataframe))

# Task B

# Select numerical columns
numerical_columns <- subset(dataframe, select = sapply(dataframe, is.numeric))

# Calculate mean for each numerical column
mean_values <- sapply(numerical_columns, function(x) mean(x, na.rm = TRUE))
mean_values

# Calculate median for each numerical column
median_values <- sapply(numerical_columns, function(x) median(x, na.rm = TRUE))
median_values

# Calculate minimum for each numerical column
min_values <- sapply(numerical_columns, function(x) min(x, na.rm = TRUE))
min_values

# Calculate maximum for each numerical column
max_values <- sapply(numerical_columns, function(x) max(x, na.rm = TRUE))
max_values

# Calculate standard deviation for each numerical column
sd_values <- sapply(numerical_columns, function(x) sd(x, na.rm = TRUE))
sd_values


# Task C

# Min-Max Normalization
min_max <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

normalized_data_min_max <- sapply(numerical_columns, min_max)
normalized_data_min_max

# Z-score Standardization
z_score <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

standardized_data_z_score <- sapply(numerical_columns, z_score)
standardized_data_z_score

# Robust Scalar
robust_scalar <- function(x) {
  (x - median(x, na.rm = TRUE)) / IQR(x, na.rm = TRUE)
}

robust_scaled_data <- sapply(numerical_columns, robust_scalar)
robust_scaled_data

# Task D
