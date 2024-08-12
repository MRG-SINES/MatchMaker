# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library (RColorBrewer)
library(wesanderson)
library(cowplot)
library(viridis)
library(ggridges)
library(dunn.test)
library(ggpubr)
library(ggthemes)
library(viridis)
library(hrbrthemes)


#install.packages("forcats")

setwd ("F:/Ph.D/Research Projects/Match Making/Results/Plots/Binning")

####################################Plotting No of Bins Recovered Ridges #######################################

# Read the text file (assuming it's tab-delimited)
data <- read.delim("Test.txt")  

# Modify the combinations column
data$Combination <- data$Combination %>%
  gsub("MaxBin", "MaxBin2.0", .) %>%
  gsub("MetaBAT", "MetaBAT2", .)

#Clean data: Remove non-numeric characters and convert to numeric
data$Bins <- as.numeric(gsub("[^0-9]", "", data$Bins))

#Check for NAs in "Bins" after conversion
if (any(is.na(data$Bins))) {
  stop("Error: Some values in 'Bins' could not be converted to numeric.")
}

# Define custom colors for fill and outline
#fill_color <- brewer.pal(8, "Set3")[1:9]  # Using the first three colors from Paired
outline_color <- c("darkslateblue", "dodgerblue4", "blue4", "darkorchid", "darkmagenta", "brown3","coral3", "darkorange2", "darkgoldenrod2")


#Defining function for customizing y axis
label_number <- function(scale = 1, suffix = "") {
  function(x) {
    scales::number_format(scale = scale, suffix = suffix)(x)
  }
}

# Clean data: Remove non-numeric characters and convert to numeric
data$Bins <- as.numeric(gsub("[^0-9]", "", data$Bins))

# Check for NAs in "Bins" after conversion
if (any(is.na(data$Bins))) {
  stop("Error: Some values in 'Bins' could not be converted to numeric.")
}

# Convert "Bins" to numeric
data$Bins <- as.numeric(data$Bins)

# Check for NAs in "Bins" after conversion
if (any(is.na(data$Bins))) {
  stop("Error: Some values in 'Bins' could not be converted to numeric.")
}

# Calculate the means of the groups for each Combination
mean_data <- aggregate(Bins ~ Combination, data = data, FUN = mean)

# Reorder Combination factor levels based on the mean number of bins
data$Combination <- factor(data$Combination, levels = mean_data$Combination[order(mean_data$Bins)])
######################virids palette##########################################

fill_color <- brewer.pal(8, "Paired")[1:9]  # Using the first three colors from Paired
outline_color <- brewer.pal(8, "Paired")[1:9] 

# Calculate the means of the groups for each Combination
mean_data <- aggregate(Bins ~ Combination, data = data, FUN = mean)
#write.csv (mean_data, "Mean Bins Per Method.csv")

# Reorder Combination factor levels based on the mean number of bins
data$Combination <- factor(data$Combination, levels = mean_data$Combination[order(mean_data$Bins)])

# Create the ridge plot
plot_Ridge <- ggplot(data, aes(x = Bins, y = Combination, fill = Combination, color = Combination)) +
  geom_density_ridges(scale = 2.5, rel_min_height = 0.01, alpha = 0.65, linewidth = 1) +
  labs(x = "Avg. No. of MAGs", y = " ", fill = "Combination", color = "Combination") +
  scale_fill_manual(values = fill_color) +
  scale_color_manual(values = outline_color) +
  theme_classic() +
  theme(aspect.ratio = 0.5) +
  theme(legend.position = "none")  # Place the legend on the right
 # Remove y-axis labels

# Print the ridge plot
print(plot_Ridge)

#Save the plot as SVG
ggsave("Total_No_of_Bins.svg", width = 13.5, height = 7)
# Save the plot as PDF
ggsave("Total_No_of_Bins.pdf",width = 13.5, height = 7)

###################################################################################################################
setwd ("F:/Ph.D/Research Projects/Match Making/Results/Plots/Taxonomy")

# Read the CSV file
df <- read.csv("bins_method.csv", header = TRUE, stringsAsFactors = FALSE)

# Modify the combinations column
df$Method <- df$Method %>%
  gsub("MaxBin", "MaxBin2.0", .) %>%
  gsub("MetaBAT", "MetaBAT2", .)

# Filter species where percentage is greater than 0.1%
df_filtered <- df
#################################### Taxa Counts in Quartiles######################################################

# Calculate quartile ranges
quartile_ranges <- df_filtered %>%
  mutate(quartile = ntile(percent, 4)) %>%
  group_by(quartile) %>%
  summarise(min_percent = min(percent), max_percent = max(percent)) %>%
  mutate(quartile_label = paste0("Q", quartile, "\n(", round(min_percent, 2), " - ", round(max_percent, 2), ")"))

# Create presence-absence matrix
presence_absence <- df_filtered %>%
  mutate(presence = 1) %>%
  mutate(quartile = ntile(percent, 4)) %>%
  select(quartile, taxon_name, Method, presence) %>%
  distinct() %>%
  pivot_wider(names_from = Method, values_from = presence, values_fill = list(presence = 0))

# Calculate the number of taxa recovered by each method for each quartile
taxa_count_by_quartile <- presence_absence %>%
  select(-taxon_name) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

# Merge with quartile ranges for labels
taxa_count_by_quartile <- taxa_count_by_quartile %>%
  left_join(quartile_ranges, by = "quartile") %>%
  select(-min_percent, -max_percent)

# Convert to long format for ggplot2
taxa_count_long <- melt(taxa_count_by_quartile, id.vars = c("quartile", "quartile_label"), variable.name = "Method", value.name = "count")

# Set factor levels for Method to ensure kaiju is last
taxa_count_long$Method <- factor(taxa_count_long$Method, levels = c( "Kaiju","metaSPAdes-MetaBAT2","metaSPAdes-CONCOCT", "MEGAHIT-CONCOCT","IDBA-UD-CONCOCT","IDBA-UD-MetaBAT2", "metaSPAdes-MaxBin2.0", "MEGAHIT-MaxBin2.0", "MEGAHIT-MetaBAT2", "IDBA-UD-MaxBin2.0"))

# Create the heatmap
plot_taxa_count <- ggplot(taxa_count_long, aes(x = quartile_label, y = Method, fill = count)) +
  geom_tile(color = NA) +
  geom_text(aes(label = count), color = "black", size = 3) +
  scale_fill_gradientn(colours = brewer.pal(5, "Blues"), name = "No. of Species") +
  labs(x = " ", y = " ", title = " ") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        axis.text.y = element_text(size = 8),
        legend.position = "right")

# Print the heatmap
print(plot_taxa_count)

###################################Without distinct############################################

# Calculate the number of taxa recovered by each method for each quartile (including repeated instances)
taxa_count_by_quartile <- df_filtered %>%
  mutate(quartile = ntile(percent, 4)) %>%
  group_by(quartile, Method) %>%
  summarise(count = n(), .groups = 'drop') %>%
  left_join(quartile_ranges, by = "quartile")

# Convert to long format for ggplot2
taxa_count_long <- taxa_count_by_quartile %>%
  select(quartile, quartile_label, Method, count)

# Set factor levels for Method to ensure kaiju is last
taxa_count_long$Method <- factor(taxa_count_long$Method, levels = c( "Kaiju","metaSPAdes-MetaBAT2","metaSPAdes-CONCOCT", "MEGAHIT-CONCOCT","IDBA-UD-CONCOCT","IDBA-UD-MetaBAT2", "metaSPAdes-MaxBin2.0", "MEGAHIT-MaxBin2.0", "MEGAHIT-MetaBAT2", "IDBA-UD-MaxBin2.0"))

# Create the heatmap
plot_taxa_count <- ggplot(taxa_count_long, aes(x = quartile_label, y = Method, fill = count)) +
  geom_tile(color = NA) +
  geom_text(aes(label = count), color = "black", size = 3) +
  scale_fill_gradientn(colours = brewer.pal(5, "Blues"), name = "No. of Species") +
  labs(x = " Species Abundance (%)", y = "", title = " ") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        axis.text.y = element_text(size = 8),
        legend.position = "right")

#Print the heatmap
print(plot_taxa_count)


#################################################Presence Absence Plots#########################################################

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)

# Read the CSV file
df <- read.csv("bins_method.csv", header = TRUE, stringsAsFactors = FALSE)

# Filter species where percentage is greater than 0.1%
df_filtered <- df %>%
  filter(percent > 0.05, Method != "Kaiju")

# Create presence-absence matrix
presence_absence <- df_filtered %>%
  mutate(presence = 1) %>%  # Add a column to indicate presence
  select(taxon_id, taxon_name, Method, presence) %>%  # Select necessary columns
  distinct() %>%  # Remove duplicate entries
  pivot_wider(names_from = Method, values_from = presence, values_fill = list(presence = 0))  # Pivot methods into columns and fill NAs with 0

write.csv(presence_absence, "presence_absence_of_Species_per_method.csv")

# Convert to long format for ggplot2
presence_absence_long <- melt(presence_absence, id.vars = c("taxon_id", "taxon_name"))

# Create the heatmap
plot_taxa <- ggplot(presence_absence_long, aes(x = variable, y = taxon_name)) +
  geom_tile(aes(fill = factor(value)), color = NA) +
  scale_fill_manual(values = c("0" = "black", "1" = "goldenrod2"), name = "Species Recovered") +
  labs(x = NULL, y = NULL, title = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        axis.text.y = element_text(size = 8),
        legend.position = "bottom")+
  coord_flip()
plot_taxa

#Save the plot as SVG
ggsave("Genomes_Presence_Absence.svg", width = 13.5, height = 8, dpi=600)
# Save the plot as PDF
ggsave("Genomes_Presence_Absence.pdf",width = 13.5, height = 8, dpi=600)


##############################box plots Final##################################################################
setwd("F:/Ph.D/Research Projects/Match Making/Results")

# Read the CSV file
df <- read.csv("Specie Level Classification Cleaned V5.csv", header = TRUE, stringsAsFactors = FALSE)

# Modify the combinations column
df$Method <- df$Method %>%
  gsub("MaxBin", "MaxBin2.0", .) %>%
  gsub("MetaBAT", "MetaBAT2", .)

#fill_color <- brewer.pal(8, "Paired")[1:9]  # Using the first three colors from Paired
#outline_color <- brewer.pal(8, "Paired")[1:9] 

# Define custom colors for fill and outline
fill_color <- c("red2","wheat4","indianred1","darkorange", "seagreen", "forestgreen", "#0066cc","tan1", "cornflowerblue")
# outline_color <- c("#E56B5DFF", "#CC4678FF", "#A92395FF", "goldenrod2", "orange", "#7E03A8FF","#F89441FF", "#4C02A1FF", "#0D0887FF")

# Define the fixed order of methods
method_order <- c("metaSPAdes-MetaBAT2", "metaSPAdes-CONCOCT","MEGAHIT-MetaBAT2",  
                  "MEGAHIT-CONCOCT","IDBA-UD-MetaBAT2","metaSPAdes-MaxBin2.0",
                  "MEGAHIT-MaxBin2.0", "IDBA-UD-CONCOCT", "IDBA-UD-MaxBin2.0")

# Use factor to set the order of Method
df$Method <- factor(df$Method, levels = method_order)

# Calculate total number of taxa per SampleID and Method, excluding "Kaiju"
taxa_counts <- df %>%
  filter(Method != "Kaiju") %>%
  group_by(SampleID, Method) %>%
  summarize(Total_Taxa = n()) %>%
  ungroup() %>%
  arrange(desc(Total_Taxa))  # Arrange in descending order by Total_Taxa

# Calculate total number of taxa per Method, excluding "Kaiju"
taxa_counts_total <- df %>%
  filter(Method != "Kaiju") %>%
  group_by(Method) %>%
  summarize(Total_Taxa = n()) %>%
  ungroup() %>%
  arrange(desc(Total_Taxa))  # Arrange in descending order by Total_Taxa


write.csv (taxa_counts, "No of MAGs Recovered by Per Sample per Method.csv")

# Create box plot
plot_MAGs_No <- ggplot(taxa_counts, aes(x = Method, y = Total_Taxa, fill = Method, color = Method)) +
  geom_boxplot(alpha = 0.6, lwd=1, width=0.4,outliers = FALSE) +
  geom_jitter(width = 0.1, alpha = 1) +  # Adding jitter for better visualization
  labs(x = NULL, y = "No. of Species") +
  scale_fill_manual(values = fill_color) +
  scale_color_manual(values = fill_color) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))
plot_MAGs_No


######################################## Combining Plots########################################
library(ggpubr)
library(ggplot2)

# Adjust the size of each plot
plot_width <- 2  # You can adjust this value
plot_height <- 2  # You can adjust this value

# Remove legend and title from each plot
plot1 <- plot_taxa_count+ theme(legend.position = "right", plot.title = element_blank())
plot1
plot2 <- plot_Ridge + theme(legend.position = "none", plot.title = element_blank())
plot2
plot3 <- plot_MAGs_No + theme(legend.position = "none", plot.title = element_blank())
plot3

combined_taxa1 <- ggarrange(plot2, plot3, nrow=2, ncol=1, widths = c(plot_height, plot_height, plot_width,  plot_width), labels=c( "A", "B"))
combined_taxa1

# Arrange plots with adjusted dimensions
combined_taxa2 <- ggarrange(
  combined_taxa1, plot1, 
  nrow = 1, ncol = 2, 
  widths = c(plot_width, plot_width),  # Adjust widths of columns
  heights = c(plot_height),            # Adjust height of the single row
  labels = c(" ", "C")                 # Optional: Add labels to plots
)

combined_taxa2


#Save the plot as SVG
ggsave("Taxa_classic_ALL.svg", width = 13.5, height = 8, dpi=600)
# Save the plot as PDF
ggsave("Taxa_classic_ALL.pdf",width = 13.5, height = 8, dpi=600)

