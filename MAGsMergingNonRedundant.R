# Load necessary libraries
library(dplyr)
library(tidyr)


setwd("F:/CAMI Dataset/Results/MAGs_Classification")

# List all CSV files in the working directory
csv_files <- list.files(pattern = ".csv")

# Initialize an empty data frame to store the merged data
merged_data <- data.frame()

# Loop through each CSV file
for (file in csv_files) {
  # Read the CSV file
  data <- read.csv(file)
  
  # Extract sample id, binner, and assembler from the file name
  file_info <- strsplit(gsub(".csv", "", file), "_")[[1]]
  sample_id <- file_info[1]
  binner <- file_info[2]
  assembler <- file_info[3]
  
  # Add sample id, binner, assembler, and classification columns to the data
  data <- mutate(data, SampleID = sample_id, Binner = binner, Assembler = assembler)
  
  # Convert "Classification" column to character
  data$Classification <- as.character(data$Classification)
  
  # Select only the relevant columns (SampleID, Binner, Assembler, Classification)
  data <- select(data, SampleID, Binner, Assembler, Classification)
  
  # Append the data to the merged_data data frame
  merged_data <- bind_rows(merged_data, data)
}

# Remove duplicate rows based on the "Classification" column
merged_data <- distinct(merged_data, Classification, .keep_all = TRUE)

# Save the merged data to a new CSV file
write.csv(merged_data, "merged_data.csv", row.names = FALSE)

# Print the merged data
print(merged_data)
