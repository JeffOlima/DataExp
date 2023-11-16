
library(ggplot2)
library(dplyr)
library(tidyr)

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

# Filter data for a specific station and offense
filtered_data <- subset(dataframe, `Garda.Station` == "35301 Abbeyfeale, Limerick Division" & `Type.of.Offence` == "Attempts/threats to murder, assaults, harassments and related offences")

# Line plot
ggplot(filtered_data, aes(x = Year, y = VALUE, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "blue", size = 2) +
  labs(title = "Trend of Assaults Over Years",
       x = "Year", y = "Number of Occurances") +
  theme_minimal()

# Scatter plot
ggplot(filtered_data, aes(x = Year, y = VALUE)) +
  geom_point(color = "red", size = 3) +
  labs(title = "Scatter Plot of Assaults Over Years",
       x = "Year", y = "Number of Assaults") +
  theme_minimal()

# Heatmap plot

# Pivot the data to wide format
heatmap_data <- filtered_data %>%
  select(Year, `Type.of.Offence`, VALUE) %>%
  spread(key = `Type.of.Offence`, value = VALUE)

ggplot(data = heatmap_data, aes(x = `Attempts/threats to murder, assaults, harassments and related offences`, y = Year, fill = as.numeric(as.matrix(heatmap_data[, -1])))) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Heatmap of Assaults Over Years",
       x = "Type of Offence", y = "Year")

# Task E

# Task F 

# Create dummy variables using model.matrix
dummy_variables <- model.matrix(~ `Type.of.Offence` - 1, data = dataframe)

# Combine the dummy variables with the dataset
dataframe <- cbind(dataframe, dummy_variables)

# View column names
names(dataframe)

