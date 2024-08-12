
# Load necessary libraries
library(dplyr)
library(readr)
library(viridis)

# Define the paths to the CSV files
strain_analysis_file_path <- "F:/Ph.D/Research Projects/Match Making/Skin Samples/Kaiju_GTDB-TK-Merged_Final_Skin.csv"
checkm_merged_file_path <- "F:/Ph.D/Research Projects/Match Making/Skin Samples/CheckM_Skin_result_with_quality.csv"
output_file_path <- "F:/Ph.D/Research Projects/Match Making/Skin Samples/Kaiju_GTDB-TK_Results_with_Quality_skin.csv"

# Read the CSV files
strain_analysis <- read_csv(strain_analysis_file_path)
checkm_merged <- read_csv(checkm_merged_file_path)

# Append ".fa" to the Bins column in checkm_merged and remove "SRR" from SampleID
checkm_merged <- checkm_merged %>%
  mutate(Bins = paste0(Bins, ".fa"))
         #SampleID = gsub("^SRR", "", SampleID))


# Convert SampleID in strain_analysis to character to match the type in checkm_merged
checkm_merged <- checkm_merged %>%
  mutate(SampleID = as.character(SampleID))

# Merge the two data frames based on SampleID, Method, and Bins
merged_data <- strain_analysis %>%
  left_join(checkm_merged, by = c("SampleID", "Method", "Bins")) %>%
  select(SampleID, Method, Bins, Method, taxon_name, percent, reads, MAGs_Quality)

# Write the merged data to a new CSV file
write_csv(merged_data, output_file_path)

# Print the first few rows of the merged data
print(head(merged_data))

#################################################Presence Absence Plots#########################################################

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)

setwd ("F:/Ph.D/Research Projects/Match Making/Results")

# Read the CSV file
df <- read.csv("Kaiju_GTDB-TK-Merged_Final_V4.csv", header = TRUE, stringsAsFactors = FALSE)

#################################Low Abundance Species ############################################
# Filter data to include only species with percent <= 1 and >= 0.05, and Method != "Kaiju"
df_filtered_low <- df %>%
  filter(percent < 1 & percent >= 0.05, Method != "Kaiju")

# Modify the combinations column
df_filtered_low$Method <- df_filtered_low$Method %>%
  gsub("MaxBin", "MaxBin2.0", .) %>%
  gsub("MetaBAT", "MetaBAT2", .)

# Calculate the mean percent of each species per method
mean_percent_per_species <- df_filtered_low %>%
  group_by(Species, Method) %>%
  summarize(mean_percent = mean(percent), .groups = 'drop')

# Arrange species by mean percent in ascending order
taxa_order <- mean_percent_per_species %>%
  group_by(Species) %>%
  summarize(min_mean_percent = min(mean_percent), .groups = 'drop') %>%  # Get minimum mean percent for each species
  arrange(min_mean_percent) %>%  # Arrange taxa in ascending order of mean percent
  pull(Species)  # Extract taxa names

# Create a presence-absence matrix
presence_absence_low <- df_filtered_low %>%
  mutate(presence = 1) %>%  # Add a column to indicate presence
  select( Species, Method, presence) %>%  # Select necessary columns
  distinct() %>%  # Remove duplicate entries
  pivot_wider(names_from = Method, values_from = presence, values_fill = list(presence = 0))  # Pivot methods into columns and fill NAs with 0

# Write the presence-absence matrix to a CSV file
#write.csv(presence_absence_low, "presence_absence_of_Low_Species_per_method.csv")

# Convert to long format for ggplot2
presence_absence_long_low <- melt(presence_absence_low, id.vars = c("Species"))

# Create the heatmap
plot_taxa_low <- ggplot(presence_absence_long_low, aes(x = variable, y = Species)) +
  geom_tile(aes(fill = factor(value)), color = NA) +
  scale_fill_manual(values = c("0" = "gray", "1" = "blue4"), name = " Species Recovered") +
  labs(x = NULL, y = NULL, title = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        legend.position = "right") +
  #coord_flip() +
  scale_y_discrete(limits = rev(taxa_order))  # Set y-axis limits to arranged taxa names

# Print the heatmap
print(plot_taxa_low)

#Save the plot as SVG
ggsave("plot_taxa_low.svg", width = 13.5, height = 8, dpi=600)
# Save the plot as PDF
ggsave("plot_taxa_low.pdf",width = 13.5, height = 8, dpi=600)

######################################Plotting Quality Specific Quartiles##########################################
# Load necessary libraries
library(dplyr)
library(readr)
library(ggplot2)
library(ggpubr)
library(wesanderson)
library (knitr)

# Define the path to the CSV file
#input_file_path <- "F:/Ph.D/Research Projects/Match Making/Skin Samples/Kaiju_GTDB-TK_Results_with_Quality_skin.csv"
input_file_path <- "F:/Ph.D/Research Projects/Match Making/Results/Kaiju_GTDB-TK_Results_with_Quality.csv"

# Read the CSV file
data <- read_csv(input_file_path )

# Modify the combinations column
data$Method <- data$Method %>%
  gsub("MaxBin", "MaxBin2.0", .) %>%
  gsub("MetaBAT", "MetaBAT2", .)


# # Define specific ranges for the quartiles
 range_boundaries <- c(0.05, 0.5, 1, max(data$percent, na.rm = TRUE))
# 
# # Assign ranges based on the specified boundaries
  data <- data %>%
  mutate(Quartile = case_when(
     percent <= range_boundaries[2] ~ 1,
     percent > range_boundaries[2] & percent <= range_boundaries[3] ~ 2,
     percent > range_boundaries[3] & percent <= range_boundaries[4] ~ 3,
     TRUE ~ 4
   ))  

# # Define the range labels
 range_labels <- c("very low [0.05-0.5]", "low [0.5-1]", paste0("High [1.00-", round(range_boundaries[4], 2), "]"))
# 
# # Assign range labels to the data
data <- data %>%
   mutate(range_label = case_when(
     Quartile == 1 ~ range_labels[1],
     Quartile == 2 ~ range_labels[2],
     Quartile == 3 ~ range_labels[3],
     Quartile == 4 ~ paste0(range_labels[3], "+")
   ))

# Define specific ranges for the two categories
#range_boundaries <- c(0.42, 1, max(data$percent, na.rm = TRUE))

# Assign ranges based on the specified boundaries
#data <- data %>%
  #mutate(Quartile = case_when(
    #percent <= range_boundaries[2] ~ 1,
    #percent > range_boundaries[2] & percent <= range_boundaries[3] ~ 2
  #))

# Define the range labels
#range_labels <- c("low [0.42-1]", paste0("High [1.00-", round(range_boundaries[3], 2), "]"))

# Assign range labels to the data
#data <- data %>%
  #mutate(range_label = case_when(
    #Quartile == 1 ~ range_labels[1],
    #Quartile == 2 ~ range_labels[2]
  #))


# Set the order of the range labels
data <- data %>%
  mutate(range_label = factor(range_label, levels = range_labels))

# Set the order of MAGs_Quality
data <- data %>%
  mutate(MAGs_Quality = factor(MAGs_Quality, levels = c("High", "Medium", "Low")))

# Calculate the percentage of high, medium, and low-quality species recovered per method for each range
quality_percentages <- data %>%
  group_by(range_label, Method, MAGs_Quality) %>%
  summarize(count = n(), .groups = 'drop') %>%
  group_by(range_label, Method) %>%
  mutate(total = sum(count)) %>%
  mutate(percentage = (count / total) * 100) %>%
  select(range_label, Method, MAGs_Quality, percentage)

# Calculate the count of each method per range
count_per_range <- data %>%
  group_by(range_label, Method) %>%
  summarize(count = n(), .groups = 'drop')

# Calculate the count of each method per range
count_per_range1 <- data %>%
  group_by(range_label, Method, MAGs_Quality) %>%
  summarize(count = n(), .groups = 'drop')

# Define the order of MAGs_Quality
quality_order <- c("High", "Medium", "Low")

# Create the bubble plot with methods on the y-axis and ranges on the x-axis
bubble_plot <- ggplot(quality_percentages, aes(x = range_label, y = Method, size = percentage, color = range_label)) +
  geom_point(alpha = 0.7) +
  scale_size(range = c(2, 10), name = "Proportion (%)") +
  scale_color_manual(values = wes_palette("Zissou1", n = 3, type = "continuous"), name = "MAGs Quality", breaks = quality_order) +  # Use Wes Anderson's FantasticFox1 palette
  labs(x = "Species Abundance (%)", y = " ") +
  theme_classic2() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        legend.position = "right") +
  facet_wrap(~ MAGs_Quality, ncol = 3, labeller = labeller(MAGs_Quality = setNames(quality_order, quality_order))) +
  #facet_wrap(~ MAGs_Quality, ncol = 3, labeller = labeller(MAGs_Quality = setNames(c("High", "Medium", "Low"), c("High", "Medium", "Low")))) +
  #guides(color = guide_legend(override.aes = list(size = 5)))
guides(color = guide_legend(override.aes = list(size = 5)), 
       size = guide_legend(override.aes = list(color = "gray")))  # Ensure the size legend has black color for bubbles


bubble_plot

######################################## Combining Plots########################################

# Adjust the size of each plot
plot_width <- 4  # You can adjust this value
plot_height <- 2  # You can adjust this value

# Remove legend and title from each plot
plot1 <-plot_taxa_low+ theme(legend.position = "bottom", plot.title = element_blank())
plot2 <- bubble_plot + theme(legend.position = "right", plot.title = element_blank())

combined_plot1 <- ggarrange(plot1, plot2, nrow=1, ncol=2, widths = c(plot_width, plot_width), labels=c("A", "B"))
print(combined_plot1)


#Save the plot as SVG
ggsave("Low Abundance Taxa gut.svg", width = 13.5, height = 8, dpi=600)
# Save the plot as PDF
ggsave("Low Abundance Taxa gut.pdf",width = 13.5, height = 8, dpi=600)
getwd()


