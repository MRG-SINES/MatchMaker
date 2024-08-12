library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(tidyr)

# Define the path to the main file and the directory containing the CSV files
main_file_path <- "F:/Ph.D/Research Projects/Match Making/Skin Samples/strain_skin.csv"
csv_files_directory <- "F:/Ph.D/Research Projects/Match Making/Skin Samples/CSV+Assembler files/CSV Files/GTDB-TK"

# Read the main file
main_df <- read_csv(main_file_path)

# Create a new column 'FastANI' initialized with NA
main_df <- main_df %>% mutate(FastANI = NA)

# Iterate over each row in the main dataframe
for (i in 1:nrow(main_df)) {
  # Combine SampleID and Method to create the file name
  file_name <- paste0(main_df$SampleID[i], "_", main_df$Method[i], ".csv")
  file_path <- file.path(csv_files_directory, file_name)
  
  # Check if the file exists
  if (file.exists(file_path)) {
    # Read the corresponding CSV file
    file_df <- read_csv(file_path)
    
    # Find the matching bin in the corresponding CSV file
    matching_row <- file_df %>% filter(`User Genome` == main_df$Bins[i])
    
    # If there is a match, extract the FastANI ANI value
    if (nrow(matching_row) > 0) {
      main_df$FastANI[i] <- matching_row$`FastANI ANI`
    }
  } else {
    # If the file does not exist, print a message (optional)
    message(paste("File does not exist:", file_path))
  }
}

# Save the updated main file with the populated FastANI column
write_csv(main_df, "F:/Ph.D/Research Projects/Match Making/Results/Strain_Analysis_Results_Skin.csv")

message("FastANI column has been populated successfully.")
##################################################Plotting Strains############################################################

# Load necessary libraries
library(dplyr)
library(readr)
library(ggplot2)
library (RColorBrewer)

#setwd("F:/Ph.D/Research Projects/Match Making/Skin Samples")
setwd("F:/Ph.D/Research Projects/Match Making/Results")

# Define the path to the CSV file
csv_file_path <- "Strain_Analysis_Results_v6.csv"

# Read the CSV file
df <- read_csv(csv_file_path)

# Modify the combinations column
df$Method <- df$Method %>%
  gsub("MaxBin", "MaxBin2.0", .) %>%
  gsub("MetaBAT", "MetaBAT2", .)

fill_color <- brewer.pal(8, "Spectral")[1:9]  # Using the first three colors from Paired
outline_color <- brewer.pal(8, "Spectral")[1:9] 

# Count the number of species instances recovered by each method
species_count <- df %>%
  group_by(Method) %>%
  summarise(num_species = n())

# Reorder the Method factor based on the number of species in descending order
species_count <- species_count %>%
  arrange(desc(num_species)) %>%
  mutate(Method = factor(Method, levels = Method))
write.csv(species_count, "strain_count.csv")

# Create the dot plot
dot_plot <- ggplot(species_count, aes(x = Method, y = num_species, color = Method)) +
  geom_point(size = 6) +
  scale_fill_manual(values = fill_color) +
  scale_color_manual(values = outline_color) +
  labs(x = " ", y = "No. of Strains", title = "") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# Print the plot
print(dot_plot)

###########################################filtered bubble plot######################################
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(viridisLite)

setwd("F:/Ph.D/Research Projects/Match Making/Results")

# Read the CSV file (replace "Strain_Analysis_Results_V5.csv" with your actual file path)
df <- read.csv("Strain_Analysis_Results_V6.csv", header = TRUE, stringsAsFactors = FALSE)

# Count the number of instances for each species-method pair
species_method_counts <- df %>%
  group_by(Method, Species) %>%
  summarise(count = n(), .groups = 'drop')

# Count the distinct number of species recovered by each method
distinct_species_counts <- df %>%
  group_by(Method) %>%
  summarise(distinct_species = n_distinct(Species), .groups = 'drop')


# Filter out species with a count of 2 or less
filtered_species_method_counts <- species_method_counts %>%
  filter(count > 8)

# Count the total number of species recovered by each method
method_counts <- filtered_species_method_counts %>%
  group_by(Method) %>%
  summarise(total_count = sum(count), .groups = 'drop') %>%
  arrange(desc(total_count))

# Convert Method to a factor with levels in descending order of total_count
filtered_species_method_counts$Method <- factor(filtered_species_method_counts$Method, levels = method_counts$Method)

# Reorder filtered_species_method_counts by total_count
filtered_species_method_counts <- filtered_species_method_counts %>%
  arrange(desc(count))

# Generate the Turbo color palette
num_methods <- length(unique(filtered_species_method_counts$Method))
turbo_palette <- viridisLite::viridis(num_methods)


# Create a bubble plot
bubble_plot <- ggplot(filtered_species_method_counts, aes(x = Species, y = Method, size = count, fill = count)) +
  geom_point(alpha = 0.7, shape = 21, color = "black") +
  #scale_size_continuous(range = c(3, 10), name = "Count") +
  #scale_fill_viridis_c(option = "turbo", name = "Count") +  # Use the turbo color palette for the fill
  #scale_size_continuous(range = c(3, 10), name = "Strains Count") +
  scale_size_continuous(breaks = c(11, 22, 33, 44), limits = c(11, 44), range = c(3, 11), name = "Strains Count") +
  scale_fill_viridis_c(option = "viridis", name = "Count", guide = "none") +
  guides(size = guide_legend(override.aes = list(fill = viridis(4)[1:4]))) +
  #guides(size = guide_legend(override.aes = list(fill = viridis::viridis(4)))) +
  labs(x = " ", y = " ", title = " ") +
  #theme_minimal() +
  theme_classic()+
  theme(axis.text.x = element_text(angle= 45, hjust = 1),
        legend.position = "right")+
  coord_flip()
# Print the bubble plot
print(bubble_plot)

###########################################Adding MAGs Quality Column#########################################################

# Load necessary libraries
library(dplyr)
library(readr)

# Define the paths to the CSV files
strain_analysis_file_path <- "F:/Ph.D/Research Projects/Match Making/Results/Strain_Analysis_Results_v6.csv"
checkm_merged_file_path <- "F:/Ph.D/Research Projects/Match Making/Results/CheckM_Merged.txt"
output_file_path <- "F:/Ph.D/Research Projects/Match Making/Results/Strain_Analysis_Results_with_Quality_v6.csv"


# Read the text file into a data frame
df <- read.table("CheckM_Merged.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# Read the CSV files
strain_analysis <- read_csv(strain_analysis_file_path)
checkm_merged <- read.table(checkm_merged_file_path)

# Append ".fa" to the Bins column in checkm_merged and remove "SRR" from SampleID
checkm_merged <- df %>%
  mutate(Bins = paste0(Bins, ".fa"),
         SampleID = gsub("^SRR", "", SampleID),
         MAGs_Quality = gsub("Quality", "", MAGs_Quality))


# Convert SampleID in strain_analysis to character to match the type in checkm_merged
strain_analysis <- strain_analysis %>%
  mutate(SampleID = as.character(SampleID))

# Merge the two data frames based on SampleID, Method, and Bins
merged_data <- strain_analysis %>%
  left_join(checkm_merged, by = c("SampleID", "Method", "Bins")) %>%
  select(SampleID, Method, Bins, Classification, Species, FastANI, MAGs_Quality.x)

# Write the merged data to a new CSV file
write_csv(merged_data, output_file_path)

# Print the first few rows of the merged data
print(head(merged_data))

#Save the plot as SVG
ggsave("Species_strain_skin.svg", width = 13.5, height = 8, dpi=600)
# Save the plot as PDF
ggsave("Species_Strain_skin.pdf",width = 13.5, height = 8, dpi=600)


#########################Plotting Quality ################################################
input_file_path <- "F:/Ph.D/Research Projects/Match Making/Results/Strain_Analysis_Results_with_Quality_v6.csv"

# Step 1: Read the CSV file
merged_data <- read_csv(input_file_path)

# Modify the combinations column
merged_data$Method <- merged_data$Method %>%
  gsub("MaxBin", "MaxBin2.0", .) %>%
  gsub("MetaBAT", "MetaBAT2", .)


#Step 2: Count the number of instances for each Species-Method pair
species_method_counts <- merged_data %>%
  group_by(Species, Method) %>%
  summarize(count = n(), .groups = 'drop')

#Step 3: Filter out species with a count of 2 or less
filtered_species <- species_method_counts %>%
  filter(count > 4)

# Filter the main data to include only the species with counts greater than 2
filtered_data <- merged_data %>%
  semi_join(filtered_species, by = c("Species", "Method"))

# Convert MAGs_Quality to a factor with specific levels
filtered_data <- filtered_data %>%
  mutate(MAGs_Quality = factor(MAGs_Quality, levels = c("High", "Medium", "Low")))

# Step 4: Calculate the number of HQ, MQ, and LQ per species per method
quality_counts <- filtered_data %>%
  group_by(Species, Method, MAGs_Quality) %>%
  summarize(count = n(), .groups = 'drop')

# Step 4: Calculate the number of HQ, MQ, and LQ per method and their percentages
quality_summary <- filtered_data %>%
  group_by(Method, MAGs_Quality) %>%
  summarize(count = n(), .groups = 'drop') %>%
  
  # Calculate the total counts per method
  group_by(Method) %>%
  mutate(total_count = sum(count)) %>%
  
  # Calculate the percentage of each quality category
  mutate(percentage = (count / total_count) * 100) %>%
  select(Method, MAGs_Quality, count, percentage)  # Select relevant columns for output

# Create the heatmap
heatmap_plot <- ggplot(quality_counts, aes(x = Species, y = MAGs_Quality, fill = count)) +
  geom_tile(color = NA) +
  scale_fill_gradientn(colours = brewer.pal(5, "Blues"), name = "Count") +
  labs(x = " ", y = "Strains Quality") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        legend.position = "right") +
  coord_flip()+
  #facet_wrap(~ Method, scales = "free_x")
  #facet_wrap(~ Method)
  facet_wrap(~ Method, ncol = length(unique(quality_counts$Method)))

# Print the plot
print(heatmap_plot)

######################################## Combining Plots########################################
library(ggpubr)
library(ggplot2)

# Adjust the size of each plot
plot_width <- 2  # You can adjust this value
plot_height <- 2  # You can adjust this value

# Remove legend and title from each plot
plot1 <- dot_plot + theme(legend.position = "none", plot.title = element_blank())
plot1
plot2 <- bubble_plot + theme(legend.position = "right", plot.title = element_blank())
plot2
plot3 <- heatmap_plot + theme(legend.position = "right", plot.title = element_blank())
plot3

combined_taxa1 <- ggarrange(plot1, plot2, nrow=1, ncol=2, widths = c(plot_height, plot_height, plot_width,  plot_width), labels=c( "A", "B"))
combined_taxa1

# Arrange plots with adjusted dimensions
combined_taxa2 <- ggarrange(
  combined_taxa1, plot3,
  nrow = 2, ncol = 1, 
  widths = c(plot_width, plot_width),  # Adjust widths of columns
  heights = c(plot_height),            # Adjust height of the single row
  labels = c(" ", "C")                 # Optional: Add labels to plots
)

combined_taxa2

#Save the plot as SVG
ggsave("Strain_Analysis.svg", width = 13.5, height = 8, dpi=600)
# Save the plot as PDF
ggsave("Strain_Analysis.pdf",width = 13.5, height = 8, dpi=600)
