# Load necessary packages
library(dplyr)

setwd ("F:/Ph.D/Research Projects/Match Making/Skin Samples/")

# Read the "taxonomic abundance" file
taxonomic_abundance <- read.csv("combined_kaiju_reports.csv")

# Read the "merged data sample" file
merged_data_sample <- read.csv("GTDB-TK_Classification_Skin.csv")

# Extract the last word after "s__" from the "Classification" column
extract_last_word <- function(classification) {
  pattern <- "s__"
  index <- max(gregexpr(pattern, classification)[[1]])
  if (index == -1) {
    return("")
  } else {
    return(substring(classification, index + 3))
  }
}
merged_data_sample$taxon_name <- sapply(merged_data_sample$Classification, extract_last_word)

write.csv (merged_data_sample, "Specie Level Classification Cleaned Skin.csv")


###############################################Removing word Sample from Column######################################################
# Load necessary library
library(dplyr)

# Define the path to the input CSV file
input_csv <- "F:/Ph.D/Research Projects/Match Making/Skin Samples/combined_kaiju_reports.csv"

# Define the path to the output CSV file
output_csv <- "F:/Ph.D/Research Projects/Match Making/Skin Samples/combined_kaiju_reports_updated.csv"

# Read the CSV file into a data frame
df <- read.csv(input_csv, header = TRUE)

# Remove the word "Sample" from the SampleID column
df <- df %>%
  mutate(SampleID = gsub("SRR", "", SampleID))

# Write the modified data frame to a new CSV file
write.csv(df, output_csv, row.names = FALSE)

################################################Merging GTDB-TK and Kaiju Files#################################################################
setwd ("F:/Ph.D/Research Projects/CD/kaiju_report_CD")

# Load necessary packages
library(dplyr)

# Read the "Taxonomic abundance" file
taxonomic_abundance <- read.csv("Combined_kaiju_reports_CD.csv")

# Read the "Species Classification" file
species_classification <- read.csv("Specie Level Classification Cleaned Skin.csv")

# Merge the two data frames based on "Sample" and "taxon_name" columns
merged_data <- merge(taxonomic_abundance, species_classification, by = c("SampleID", "taxon_name"), all = FALSE)

write.csv (merged_data, "Kaiju_GTDB-TK-Merged_skin.csv")

# Select the required columns
output_data <- merged_data %>%
  select(SampleID, Bins, Method, taxon_name, percent, reads)

# Write the output data to a new CSV file
write.csv(output_data, "Kaiju_GTDB-TK-Merged_Final_Skin.csv", row.names = FALSE)

# Print the output data
print(output_data)
##################################Merging Kaiju Files###########################
setwd ("F:/Ph.D/Research Projects/CD/kaiju_report-Healthy")

# Load required libraries
library(dplyr)

# Define the directory containing the files
folder_path <- "F:/Ph.D/Research Projects/CD/kaiju_report-Healthy/"

# List all files ending with "-1-species"
files <- list.files(path = folder_path, pattern = "-1-species\\.kaijuReport$", full.names = TRUE)

# Function to read each file
read_kaiju_report <- function(file) {
  df <- read.delim(file, header = TRUE, sep = "\t")
  df$File <- basename(file)  # Add a column for the file name
  return(df)
}

# Read and combine all files
combined_df <- files %>%
  lapply(read_kaiju_report) %>%
  bind_rows()

# Define the output CSV file path
output_csv <- file.path(folder_path, "Combined_kaiju_reports_Healthy.csv")

# Write the combined data frame to a CSV file
write.csv(combined_df, output_csv, row.names = FALSE)

# Print a message indicating success
cat("Combined CSV file has been created at:", output_csv, "\n")
#########################Renaming File Column ############################################################
setwd ("F:/Ph.D/Research Projects/CD/kaiju_report_CD/")

# Load necessary library
library(dplyr)

# Read the CSV file into a data frame
df <- read.csv("F:/Ph.D/Research Projects/CD/kaiju_report_CD/Modified.csv")

# Modify the "File" column
df <- df %>%
  mutate(File = sub("-.*", "", File))

# Write the modified data frame back to a CSV file
write.csv(df, "Kaiju_CD.csv", row.names = FALSE)

