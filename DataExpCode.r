
library(ggplot2)
library(dplyr)

# Load the CSV file into R
dataframe <- read.csv("./CriminalDataset.csv")

# Task A

# Categorical variables (Statistic, Statisc Label, Garda Station, Type of Offence and Unit)
# Discrete variables (TList, Year, Station Code, Identifier and Value)
# Continuous variables ( )

# Create a bar chart for categorical variables visualization (Visualize the total occurrences for each type of offence)

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

