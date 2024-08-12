library(dplyr)
library(RColorBrewer)
library(cowplot)
library(viridis)
library(hrbrthemes)
library(ggridges)
library(ggplot2)
library(dunn.test)
library(ggpubr)
library(ggthemes)
library(ggdist)

setwd("F:/Ph.D/Research Projects/Match Making/Results/Plots/Binning/Skin")
####################################Plotting No of Bins Recovered Ridges #######################################

# Read the text file (assuming it's tab-delimited)
data <- read.delim("Skin_Bins.txt")  

# Modify the combinations column
data$Binner <- data$Binner %>%
  gsub("MaxBin", "MaxBin2.0", .) %>%
  gsub("MetaBAT", "MetaBAT2", .)


# Define custom colors for fill and outline
fill_color <- brewer.pal(9, "Spectral")[1:9]  # Using the first three colors from Paired
outline_color <- c( "brown3","coral3", "darkorange2","darkgoldenrod2","goldenrod1" ,"green3", "seagreen","darkolivegreen","blue4")

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
mean_data <- aggregate(Bins ~ Binner, data = data, FUN = mean)


# Calculate the standard deviations of the bins for each Combination
sd_data_skin <- data %>%
  group_by(Binner) %>%
  summarize(sd_bins = sd(Bins, na.rm = TRUE))

# Aggregate data to find the total number of bins recovered per method
aggregated_data_skin <- data %>%
  group_by(Binner) %>%
  summarize(total_bins = sum(Bins, na.rm = TRUE))


# Reorder Combination factor levels based on the mean number of bins
data$Binner <- factor(data$Binner, levels = mean_data$Binner[order(mean_data$Bins)])
######################Ridge plot##########################################
# Create the ridge plot
plot_Ridge <- ggplot(data, aes(x = Bins, y = Binner, fill = Binner, color = Binner)) +
  geom_density_ridges(scale = 2.5, rel_min_height = 0.01, alpha = 0.85, linewidth=1) +
  labs(x = "No. of MAGs", y = " ", fill = "Binner", color = "Binner") +
  scale_fill_manual(values = fill_color) +
  #scale_fill_viridis_d(option = "C") +  # Use discrete viridis scale
  scale_color_manual(values = outline_color) +
  theme_classic() +
  theme(aspect.ratio = 0.5) +
  theme(legend.position = "none") +  # Remove the legend
  theme(axis.text.y = element_text(angle = 0, hjust = 1))  # Adjust y-axis labels
print(plot_Ridge)

#Save the plot as SVG
ggsave("Total_No_of_Bins_skin.svg", width = 13.5, height = 7)
# Save the plot as PDF
ggsave("Total_No_of_Bins_skin.pdf",width = 13.5, height = 7)

##############################Rain cloud plots##################################################################
setwd("F:/Ph.D/Research Projects/Match Making/Skin Samples")

# Read the CSV file
df <- read.csv("Specie Level Classification Cleaned Skin.csv", header = TRUE, stringsAsFactors = FALSE)

# Modify the combinations column
df$Method <- df$Method %>%
  gsub("MaxBin", "MaxBin2.0", .) %>%
  gsub("MetaBAT", "MetaBAT2", .)

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


write.csv (taxa_counts, "No of MAGs Recovered by Per Sample per Method Skin.csv")

# Create raincloud plot
plot_MAGs_No <- ggplot(taxa_counts, aes(x = Method, y = Total_Taxa, fill = Method, color = Method)) +
  stat_halfeye(
    adjust = 0.5,
    width = 0.6, 
    justification = -0.2, 
    .width = 0, 
    alpha = 1,
    point_colour = NA,
    outlier.color = NA
  ) +
  geom_boxplot(
    width = 0.12, 
    outlier.color = NA, 
    alpha = 1
  ) +
  #geom_jitter(
    #width = 0.1, 
    #size = 1, 
    #outlier.color = NA,
    #alpha = 0.9
  #) +
  labs(x = NULL, y = "No. of Species") +
  scale_fill_manual(values = fill_color) +
  scale_color_manual(values = fill_color) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))

# Print the raincloud plot
print(plot_MAGs_No)

#####################################################################################################

# Read the CSV file
df <- read.csv("Kaiju_GTDB-TK-Merged_Final_Skin.csv", header = TRUE, stringsAsFactors = FALSE)

# Modify the combinations column
df$Method <- df$Method %>%
  gsub("MaxBin", "MaxBin2.0", .) %>%
  gsub("MetaBAT", "MetaBAT2", .)

# Filter species where percentage is greater than 0.1%
df_filtered <- df %>%
  filter(Method != "Kaiju")
#################################### Taxa Counts in Quartiles######################################################

# Calculate quartile ranges
quartile_ranges <- df_filtered %>%
  mutate(quartile = ntile(percent, 4)) %>%
  group_by(quartile) %>%
  summarise(min_percent = min(percent), max_percent = max(percent)) %>%
  mutate(quartile_label = paste0("Q", quartile, "\n(", round(min_percent, 2), " - ", round(max_percent, 2), ")"))

# Calculate the number of taxa recovered by each method for each quartile (including repeated instances)
taxa_count_by_quartile <- df_filtered %>%
  mutate(quartile = ntile(percent, 4)) %>%
  group_by(quartile, Method) %>%
  summarise(count = n(), .groups = 'drop') %>%
  left_join(quartile_ranges, by = "quartile")

# Convert to long format for ggplot2
taxa_count_long <- taxa_count_by_quartile %>%
  select(quartile, quartile_label, Method, count)

# Set factor levels for Method to ensure Kaiju is last
taxa_count_long$Method <- factor(taxa_count_long$Method, levels = c("Kaiju", "metaSPAdes-MetaBAT2", "metaSPAdes-CONCOCT", "MEGAHIT-CONCOCT", "IDBA-UD-CONCOCT", "IDBA-UD-MetaBAT2", "metaSPAdes-MaxBin2.0", "MEGAHIT-MaxBin2.0", "MEGAHIT-MetaBAT2", "IDBA-UD-MaxBin2.0"))

# Create the heatmap
plot_taxa_count <- ggplot(taxa_count_long, aes(x = quartile_label, y = Method, fill = count)) +
  geom_tile(color = NA) +
  geom_text(aes(label = count), color = "black", size = 3) +
  scale_fill_gradientn(colours = brewer.pal(5, "Greens"), name = "No. of Species") +
  labs(x = "Species Abundance (%) ", y = "", title = " ") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        axis.text.y = element_text(size = 8),
        legend.position = "right")

#Print the heatmap
print(plot_taxa_count)


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
ggsave("Taxa_classic_Skin.svg", width = 13.5, height = 8, dpi=600)
# Save the plot as PDF
ggsave("Taxa_classic_Skin.pdf",width = 13.5, height = 8, dpi=600)
