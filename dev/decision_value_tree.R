# Install and load required packages
if (!require(collapsibleTree)) install.packages("collapsibleTree")
if (!require(dplyr)) install.packages("dplyr")
library(collapsibleTree)
library(dplyr)

# Read the CSV data
data <- read.csv("dtable.csv", check.names = FALSE) %>%
  select(-1)  # Remove the index column

# For Binary outcomes
binary_data <- data %>%
  filter(`Type of Outcome` == "Binary") %>%
  select(
    Type = `Type of Outcome`,
    Summary = `Summary Statistics for Primary Outcome`,
    Treatment = `Summary Statistics for Treatment Outcome`,
    Method = `Method for Populating CI`,
    Required = `Required Columns`
  )

# For Continuous outcomes - represent the correct structure
continuous_data <- data %>%
  filter(`Type of Outcome` == "Continuous") %>%
  transmute(
    Type = `Type of Outcome`,
    Summary = `Summary Statistics for Primary Outcome`,# This will correctly place Supplied/Calculated CI
    Treatment = `Summary Statistics for Treatment Outcome`  # This contains the required columns
    # Method = `Summary Statistics for Treatment Outcome`
  )

# Combine the data
Outcome <- bind_rows(binary_data, continuous_data) %>%
  filter(!is.na(Type))

# Create the collapsible tree
collapsibleTree(
  Outcome,
  hierarchy = c("Type", "Summary", "Treatment", "Method", "Required"),
  width = 1000,
  height = 800,
  fontSize = 14,
  tooltip = TRUE,
  nodeSize = "leafCount",
  collapsed = TRUE
)
