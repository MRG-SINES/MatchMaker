# Load necessary library
library(stringr)
#################################RENAMING FILES WITH ASSEMBLER-BINNER COMBO NAME#################################
# Set the working directory to the folder containing your Excel files
setwd("F:/Ph.D/Research Projects/Match Making/Skin Samples/CSV+Assembler files/CSV Files/metaSPAdes-MetaBAT")

# Function to rename CSV files
rename_csv_files <- function(directory) {
  # List all CSV files in the directory
  csv_files <- list.files(path = directory, pattern = "\\.csv$", full.names = TRUE)
  
  # Loop through each file and rename it
  for (file in csv_files) {
    # Extract the sample ID part of the filename
    sample_id <- str_extract(basename(file), "^[^\\-]+")
    
    # Create the new filename
    new_filename <- paste0(sample_id, "_metaSPAdes-MetaBAT.csv")
    
    # Construct full path for the new filename
    new_file_path <- file.path(directory, new_filename)
    
    # Rename the file
    file.rename(file, new_file_path)
    
    # Print the old and new filenames
    cat("Renamed:", file, "to", new_file_path, "\n")
  }
}

# Specify the directory containing the CSV files
directory <- "F:/Ph.D/Research Projects/Match Making/Skin Samples/CSV+Assembler files/CSV Files/metaSPAdes-MetaBAT"
# Call the function to rename the files
rename_csv_files(directory)

########################################REPLACING - WITH _ FOR ALL CSV FILES###############################################################
# Load necessary library
library(stringr)

# Function to rename CSV files
rename_csv_files <- function(directory) {
  # List all CSV files in the directory
  csv_files <- list.files(path = directory, pattern = "\\.csv$", full.names = TRUE)
  
  # Loop through each file and rename it
  for (file in csv_files) {
    # Extract the base name of the file (without the path)
    base_name <- basename(file)
    
    # Replace "_UD" with "-UD" in the base name if it contains "IDBA_UD"
    if (grepl("IDBA_UD", base_name)) {
      new_base_name <- gsub("IDBA_UD", "IDBA-UD", base_name)
      
      # Construct full path for the new filename
      new_file_path <- file.path(directory, new_base_name)
      
      # Rename the file
      file.rename(file, new_file_path)
      
      # Print the old and new filenames
      cat("Renamed:", file, "to", new_file_path, "\n")
    }
  }
}

# Specify the directory containing the CSV files
directory <- "F:/Ph.D/Research Projects/Assembly Integration/Input files"

# Call the function to rename the files
rename_csv_files(directory)



# Load necessary library
library(stringr)

# Function to rename CSV files
rename_csv_files <- function(directory) {
  # List all CSV files in the directory
  csv_files <- list.files(path = directory, pattern = "\\.csv$", full.names = TRUE)
  
  # Loop through each file and rename it
  for (file in csv_files) {
    # Extract the base name of the file (without the path)
    base_name <- basename(file)
    
    # Replace the last underscore with a hyphen
    if (grepl("_", base_name)) {
      parts <- strsplit(base_name, "_")[[1]]
      new_base_name <- paste0(paste(parts[-length(parts)], collapse = "_"), "-", parts[length(parts)])
    } else {
      new_base_name <- base_name
    }
    
    # Construct full path for the new filename
    new_file_path <- file.path(directory, new_base_name)
    
    # Rename the file
    file.rename(file, new_file_path)
    
    # Print the old and new filenames
    cat("Renamed:", file, "to", new_file_path, "\n")
  }
}

# Specify the directory containing the CSV files
directory <- "F:/Ph.D/Research Projects/Assembly Integration/Input files"

# Call the function to rename the files
rename_csv_files(directory)


##############################################Checking Data type of User.Genome Column######################

# Load necessary library
library(dplyr)

# Set the working directory to the folder containing your Excel files
setwd("F:/Ph.D/Research Projects/Match Making/Skin Samples/CSV+Assembler files/CSV Files/GTDB-TK")

# Function to check the data type of the User.Genome column and collect non-character file names
check_user_genome_type <- function(df, file_name, non_character_files) {
  if ("User.Genome" %in% colnames(df)) {
    if (class(df$User.Genome) != "character") {
      cat("Data type of 'User.Genome' in", file_name, "is:", class(df$User.Genome), "\n")
      non_character_files <<- c(non_character_files, file_name)
    }
  } else {
    cat("Column 'User.Genome' does not exist in", file_name, "\n")
  }
  return(non_character_files)
}

# Function to read and check data types for all CSV files in a directory
check_data_types_in_directory <- function(directory) {
  # List all CSV files in the directory
  csv_files <- list.files(path = directory, pattern = "\\.csv$", full.names = TRUE)
  
  # Initialize list to store filenames with non-character User.Genome columns
  non_character_files <- c()
  
  # Read each file into a data frame and check the data type of User.Genome column
  for (file in csv_files) {
    df <- read.csv(file)
    non_character_files <- check_user_genome_type(df, basename(file), non_character_files)
  }
  
  # Write non-character filenames to a text file
  writeLines(non_character_files, file.path(directory, "non_character_user_genome_files.txt"))
}

# Specify the directory containing the CSV files
directory <- "F:/Ph.D/Research Projects/Match Making/Skin Samples/CSV+Assembler files/CSV Files/GTDB-TK"

# Check data types of User.Genome columns
check_data_types_in_directory(directory)


###########################################MERGING CSV GTDB-TK FILES#################################################################
# Install and load necessary packages
install.packages(c("readxl", "dplyr"))
library(readxl)
library(dplyr)

# Set the working directory to the folder containing your Excel files
setwd("F:/Ph.D/Research Projects/Match Making/Skin Samples/CSV+Assembler files/CSV Files/GTDB-TK")

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
  method <- file_info[2]
  
  # Add sample id, binner, assembler, and classification columns to the data
  data <- mutate(data, SampleID = sample_id, Method = method)
  
  # Convert "Classification" column to character
  data$Classification <- as.character(data$Classification)
  
  # Select only the relevant columns (SampleID, Binner, Assembler, Classification)
  data <- select(data, SampleID, Method, User.Genome, Classification)
  
  # Append the data to the merged_data data frame
  merged_data <- bind_rows(merged_data, data)
}

warnings()
# Save the merged data to a new CSV file
write.csv(merged_data, "GTDB-TK_Classification_Skin.csv", row.names = FALSE)

################################################Removing duplicated taxa#######################################################
# Install and load necessary packages
install.packages(c("readxl", "dplyr"))
library(readxl)
library(dplyr)

# Set the working directory to the folder containing your Excel files
setwd("F:/CAMI Dataset/Results/MAGs_Classification")

# List all CSV files in the working directory
csv_files <- list.files(pattern = ".csv")

# Initialize an empty data frame to store the merged data
merged_data <- data.frame()

# Loop through each CSV file
for (file in csv_files) {
  # Read the CSV file
  data <- read.csv(file, select = c("Classification", "User_Genome"))
  
  # Extract sample id, binner, and assembler from the file name
  file_info <- strsplit(gsub(".csv", "", file), "_")[[1]]
  sample_id <- file_info[1]
  method <- file_info[2]
  
  # Add sample id, binner, assembler, and classification columns to the data
  data <- mutate(data, SampleID = sample_id, Method = method)
  
  # Append the data to the merged_data data frame
  merged_data <- bind_rows(merged_data, data)
}

# Save the merged data to a new CSV file
write.csv(merged_data, "merged_data_Test_3_Samples.csv", row.names = FALSE)

# Remove duplicate rows based on the "Classification" column
merged_data <- distinct(merged_data, Classification, .keep_all = TRUE)

# Save the merged data to a new CSV file
write.csv(merged_data, "merged_data_non_Redundant.csv", row.names = FALSE)

# Print the merged data
print(merged_data)

