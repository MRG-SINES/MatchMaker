# Load necessary libraries
library(dplyr)
library(readr)

# Define the path to the folder containing the TSV files
folder_path <- "F:/Ph.D/Research Projects/Match Making/Skin Samples/CheckM/CheckM_Files"

# Get a list of all TSV files in the folder
file_list <- list.files(path = folder_path, pattern = "*.tsv", full.names = TRUE)

# Initialize an empty data frame to store the merged data
merged_df <- data.frame()

# Loop over each file in the file list
for (file_path in file_list) {
  # Extract the method from the filename
  method_name <- strsplit(basename(file_path), "_")[[1]][1]
  
  # Read the TSV file
  df <- read_tsv(file_path, col_types = cols())
  
  # Add the method column
  df$method <- method_name
  
  # Append the data frame to the merged data frame
  merged_df <- bind_rows(merged_df, df)
}

# Define the path to save the merged TSV file
output_file_path <- file.path(folder_path, "merged_output_file.tsv")

# Write the merged data frame to a TSV file
write_tsv(merged_df, output_file_path)

# Print a message indicating that the process is complete
cat("All files have been merged and saved to", output_file_path, "\n")

################################################Add sample id and method columns######################################################


# Load necessary libraries
library(dplyr)
library(readr)
library(stringr)

# Define the path to the folder containing the TSV files
folder_path <- "F:/Ph.D/Research Projects/Match Making/CheckM_Reports/All CheckM Files" # Replace with your folder path

# List all TSV files in the folder
tsv_files <- list.files(path = folder_path, pattern = "*.tsv", full.names = TRUE)

# Function to extract SampleID and Method from file name
extract_info_from_filename <- function(filename) {
  file_base <- basename(filename)
  parts <- str_split(file_base, "_")[[1]]
  sample_id <- parts[1]
  method <- paste(parts[2:3], collapse = "_")
  return(list(sample_id = sample_id, method = method))
}

# Read the first file to check column names
first_file <- tsv_files[1]
first_data <- read_tsv(first_file, skip = 0, col_names = TRUE)
print(colnames(first_data))

# Define the correct column names based on the first file
# Adjust the column names based on what you see in the print output
required_columns <- c("Bin Name", "Marker Lineage", "# Genomes", "# Markers", "# Marker Sets", "0", "1", "2", "3", "4", "5+", "Completeness", "Contamination")

# Initialize an empty list to store the data frames
data_list <- list()

# Loop over each file to read and process
for (file in tsv_files) {
  # Extract SampleID and Method
  info <- extract_info_from_filename(file)
  sample_id <- info$sample_id
  method <- info$method
  
  # Read the TSV file, skip the header row
  data <- read_tsv(file, skip = 0, col_names = TRUE)
  
  # Select only the required columns
  if (all(required_columns %in% colnames(data))) {
    data <- data %>%
      select(all_of(required_columns))
  } else {
    stop("Required columns are not present in the file: ", file)
  }
  
  # Add SampleID and Method columns
  data <- data %>%
    mutate(SampleID = sample_id, Method = method)
  
  # Append the data frame to the list
  data_list <- append(data_list, list(data))
}

# Combine all data frames into one
merged_data <- bind_rows(data_list)

# Define the result file path
result_file_path <- "F:/Ph.D/Research Projects/Match Making/CheckM_Reports/CheckM_Merged.csv"

# Write the merged data to a CSV file
write_csv(merged_data, result_file_path)
#############################################Calculating MAGs Quality #########################################

# Load necessary libraries
library(dplyr)
library(readr)

# Define the path to the result CSV file
setwd ("F:/Ph.D/Research Projects/Match Making/Skin Samples")

# Read the CSV file
data <- read.csv("CheckM_Merged.csv", header = TRUE, stringsAsFactors = FALSE)

# Read the result CSV file
result_data <- read_csv(result_file_path)

# Add the MAGs_Quality column based on the given conditions
result_data <- data %>%
  mutate(MAGs_Quality = case_when(
    Completeness > 90 & Contamination < 5 ~ "High",
    Completeness >= 50 & Contamination < 10 ~ "Medium",
    TRUE ~ "Low"
  ))

# Define the path for the updated result file
updated_result_file_path <- "F:/Ph.D/Research Projects/Match Making/Skin Samples/CheckM_Skin_result_with_quality.csv"

# Write the updated data to a new CSV file
write_csv(result_data, updated_result_file_path)





