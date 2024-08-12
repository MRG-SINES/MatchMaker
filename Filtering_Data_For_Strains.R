# Load necessary libraries
library(tidyverse)

setwd("F:/Ph.D/Research Projects/Match Making/Skin Samples")

# Read the CSV file
data <- read.csv("Specie Level Classification Cleaned Skin.csv")

# Identify and filter duplicate taxa for the same SampleID and Method
duplicated_data <- data %>%
  group_by(SampleID, Method, taxon_name) %>%
  filter(n() > 1) %>%
  ungroup()

# Write the filtered data to a new CSV file
write.csv(duplicated_data, "_strain_skin.csv", row.names = FALSE)

