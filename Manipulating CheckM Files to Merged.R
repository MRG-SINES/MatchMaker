# Load necessary library
library(stringr)

setwd("F:/Ph.D/Research Projects/Match Making/Skin Samples/CheckM/metaSPAdes-MaxBin")
####################################Renaming Files From Folders ######################################################
# Get a list of all directories in the current working directory
dirs <- list.dirs(path = ".", full.names = TRUE, recursive = FALSE)

# Loop through each directory
for (dir in dirs) {
  # Get the folder name by extracting the base name of the directory path
  folder_name <- basename(dir)
  
  # List all files in the current directory
  files <- list.files(path = dir, full.names = TRUE)
  
  # Loop through each file
  for (file in files) {
    # Extract the base name of the file
    file_name <- basename(file)
    
    # Create the new file name with the prefix and folder name
    new_name <- paste0("metaSPAdes-MaxBin_", folder_name, "_", file_name)
    
    # Create the full path for the new file name
    new_file_path <- file.path(dir, new_name)
    
    # Rename the file
    file.rename(from = file, to = new_file_path)
  }
}

print("Files renamed successfully.")

#####################################Copying Files#####################################################
# Define the source directory containing subfolders
source_dir <- "F:/Ph.D/Research Projects/Match Making/Skin Samples/CheckM/metaSPAdes-MetaBAT"

# Define the destination directory where files will be copied
destination_dir <- "F:/Ph.D/Research Projects/Match Making/Skin Samples/CheckM/CheckM_Files"

# List all directories (folders) in the source directory
folders <- list.dirs(source_dir, full.names = TRUE)

# Loop through each folder
for (folder in folders) {
  # List files in the current folder
  files <- list.files(folder, full.names = TRUE)
  
  # Loop through each file in the folder
  for (file in files) {
    # Extract the file name
    file_name <- basename(file)
    
    # Construct the destination path
    destination_path <- file.path(destination_dir, file_name)
    
    # Copy the file to the destination directory
    file.copy(file, destination_path, overwrite = TRUE)
    
    # Optionally, print a message for each file copied
    cat("Copied file", file, "to", destination_path, "\n")
  }
}

# Optionally, print a message when all files are copied
cat("All files copied successfully.\n")
warnings()

