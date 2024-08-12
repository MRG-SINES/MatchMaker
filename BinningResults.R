# Load the package
library(dunn.test)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(dplyr)
library(RColorBrewer)
library(cowplot)
library(viridis)
library(hrbrthemes)
library(ggridges)

setwd ("F:/CAMI Dataset/Assembly Plots")
setwd("F:/Ph.D/Research Projects/Match Making/Results/Plots/Binning/Skin")

####################################Plotting No of Bins Recovered Ridges #######################################

# Read the text file (assuming it's tab-delimited)
data <- read.delim("Skin_Bins.txt")  

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
######################virids palette##########################################
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

############################Rain Cloud Plots for Skin ####################################
# Create the raincloud plot
plot_Raincloud <- ggplot(data, aes(x = Bins, y = Binner, fill = Binner, color = Binner)) +
  ggdist::stat_halfeye(
    adjust = 0.5,
    width = 0.6,
    justification = -0.2,
    .width = 1,
    point_colour = NA
  ) +
  geom_boxplot(
    width = 0.12,
    outlier.shape = NA,
    alpha = 0.5
  ) +
  geom_jitter(
    width = 0.1,
    size = 1,
    alpha = 0.6
  ) +
  labs(x = "Ang. No. of MAGs", y = "", color = "Binner", fill = "Binner") +
  scale_fill_manual(values = fill_color) +
  scale_color_manual(values = outline_color) +
  theme_classic() +
  theme(aspect.ratio = 1) +
  theme(legend.position = "none") +  # Remove the legend
  theme(axis.text.x = element_text(angle = 60, vjust = 0.8, hjust = 1))+  # Rotate x-axis labels vertically
  coord_flip()

# Print the plot
print(plot_Raincloud)
#Save the plot as SVG
ggsave("Assembly_Length_Rain_Cloud.svg", width = 13.5, height = 7)

#############################################MAGs Quality Circular Bar Plots###########################################

# Load the necessary library
library(tidyverse)
library(ggplot2)
library(ggridges)

# Example data based on your MAGs data
data <- data.frame(
  Combination = c("MetaSPAdes_MetaBAT", "MetaSPAdes_MaxBin", "MetaSPAdes_CONCOCT",
                  "MEGAHIT_MetaBAT", "MEGAHIT_MaxBin", "MEGAHIT_CONCOCT",
                  "IDBA_UD_MetaBAT", "IDBA_UD_MaxBin", "IDBA_UD_CONCOCT"),
  HQ = c(26.4998, 25.5144, 14.025, 22.3343, 22.1731, 12.2035, 18.4615, 19.8068, 9.0088),
  MQ = c(24.5145, 21.3404, 7.95672, 27.1854, 23.6892, 7.76197, 28.9459, 23.5105, 8.54562),
  LQ = c(46.0078, 49.0888, 76.5542, 47.1662, 49.7789, 78.5468, 48.661, 51.1272, 80.8476)
)
# Define custom colors for each Quality level
custom_colors <- c("HQ" = "#1b9e77", "MQ" = "#d95f02", "LQ" = "#7570b3")

# Reshape the data to long format
data_long <- data %>%
  pivot_longer(cols = -Combination, names_to = "Quality", values_to = "Percentage")


# Order data by Quality and then by Percentage
data_long <- data_long %>%
  arrange(Quality, Percentage)

# Add empty bars to the dataset
empty_bar <- 3
to_add <- data.frame(matrix(NA, empty_bar * nlevels(data_long$Quality), ncol(data_long)))
colnames(to_add) <- colnames(data_long)
to_add$Quality <- rep(levels(data_long$Quality), each = empty_bar)
data_long <- rbind(data_long, to_add)
data_long <- data_long %>% arrange(Quality, Percentage)
data_long$id <- seq(1, nrow(data_long))

# Get the name and the y position of each label
label_data <- data_long %>% filter(!is.na(Combination))
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id - 0.5) / number_of_bar
label_data$hjust <- ifelse(angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle + 180, angle)

# Prepare a data frame for base lines
base_data <- data_long %>%
  group_by(Quality) %>%
  summarize(start = min(id), end = max(id) - empty_bar) %>%
  rowwise() %>%
  mutate(title = mean(c(start, end)))

# Prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[c(nrow(grid_data), 1:nrow(grid_data) - 1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1, ]

# Make the plot
p <- ggplot(data_long, aes(x = as.factor(id), y = Percentage, fill = Quality)) +
  geom_bar(stat = "identity", width = 0.8, alpha = 0.7, na.rm = TRUE) +
  
  # Add lines for 100/75/50/25%
  geom_segment(data = grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha = 1, size = 0.3, inherit.aes = FALSE) +
  geom_segment(data = grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha = 1, size = 0.3, inherit.aes = FALSE) +
  geom_segment(data = grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha = 1, size = 0.3, inherit.aes = FALSE) +
  geom_segment(data = grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha = 1, size = 0.3, inherit.aes = FALSE) +
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(data_long$id, na.rm = TRUE), 4), y = c(20, 40, 60, 80), label = c("20", "40", "60", "80"), color = "grey", size = 3, angle = 0, fontface = "bold", hjust = 1) +
  
  # Circular bar plot adjustments
  ylim(-100, 120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1, 4), "cm")
  ) +
  coord_polar() +
  
  # Add individual labels
  geom_text(data = label_data, aes(x = id, y = Percentage + 10, label = Combination, hjust = hjust),
            color = "black", fontface = "bold", alpha = 0.6, size = 2.5, angle = label_data$angle, inherit.aes = FALSE) +
  
  # Add base line information
  geom_segment(data = base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha = 0.8, size = 0.6, inherit.aes = FALSE) +
  geom_text(data = base_data, aes(x = title, y = -18, label = Quality), hjust = 0.5, colour = "black", alpha = 0.8, size = 4, fontface = "bold", inherit.aes = FALSE)+


# Apply custom colors
scale_fill_manual(values = custom_colors)

print(p)

#############################################MAGs Quality Spider Plot Collective###########################################
# Install and load required packages
install.packages("fmsb")
library(fmsb)
library(tidyverse)

# Transform the data to wide format
data_wide <- data %>%
  pivot_longer(cols = c(HQ, MQ, LQ), names_to = "Quality", values_to = "Percentage") %>%
  pivot_wider(names_from = Combination, values_from = Percentage)

# Add maximum and minimum values for scaling the radar chart (To add LQ layer)
data_radar <- rbind(rep(100, ncol(data_wide) - 1), rep(0, ncol(data_wide) - 1), data_wide[-1])

# Ensure that the data is correctly formatted for radar chart
data_radar <- rbind(rep(100, ncol(data_wide) - 1), rep(0, ncol(data_wide) - 1), data_wide[-1, -1])

# Plot the radar chart
radarchart(data_radar, 
           axistype = 1, 
           pcol = c("#1b9e77", "#d95f02", "#7570b3"), 
           pfcol = c(scales::alpha("#1b9e77", 0.3), scales::alpha("#d95f02", 0.3), scales::alpha("#7570b3", 0.3)), 
           plwd = 2, 
           cglcol = "grey", 
           cglty = 1, 
           axislabcol = "grey", 
           caxislabels = seq(0, 100, 20), 
           vlcex = 0.8,
           title = "MAGs Quality across Different Combinations")

# Add a legend
legend(x = 1.2, y = 1, legend = data_wide$Quality, bty = "n", pch = 20, col = c("#1b9e77", "#d95f02", "#7570b3"),
       text.col = "black", cex = 0.8, pt.cex = 1.5)

###################################Separate Radar Plots###############################################################

# Example data based on your MAGs data
data <- data.frame(
  Combination = c("MetaSPAdes_MetaBAT", "MetaSPAdes_MaxBin", "MetaSPAdes_CONCOCT",
                  "MEGAHIT_MetaBAT", "MEGAHIT_MaxBin", "MEGAHIT_CONCOCT",
                  "IDBA_UD_MetaBAT", "IDBA_UD_MaxBin", "IDBA_UD_CONCOCT"),
  HQ = c(26.4998, 25.5144, 14.025, 22.3343, 22.1731, 12.2035, 18.4615, 19.8068, 9.0088),
  MQ = c(24.5145, 21.3404, 7.95672, 27.1854, 23.6892, 7.76197, 28.9459, 23.5105, 8.54562),
  LQ = c(46.0078, 49.0888, 76.5542, 47.1662, 49.7789, 78.5468, 48.661, 51.1272, 80.8476)
)

# Transform the data to wide format for each quality level
data_HQ <- data %>% select(Combination, HQ) %>% pivot_wider(names_from = Combination, values_from = HQ)
data_MQ <- data %>% select(Combination, MQ) %>% pivot_wider(names_from = Combination, values_from = MQ)
data_LQ <- data %>% select(Combination, LQ) %>% pivot_wider(names_from = Combination, values_from = LQ)

# Prepare the data for radar chart by adding min and max rows
prepare_radar_data <- function(df) {
  rbind(rep(100, ncol(df)), rep(0, ncol(df)), df)
}

data_HQ_radar <- prepare_radar_data(data_HQ)
data_MQ_radar <- prepare_radar_data(data_MQ)
data_LQ_radar <- prepare_radar_data(data_LQ)

# Define a function to plot radar charts
plot_radar <- function(data, title, color) {
  radarchart(data, 
             axistype = 1, 
             pcol = color, 
             pfcol = scales::alpha(color, 0.5), 
             plwd = 2, 
             cglcol = "grey", 
             cglty = 1, 
             axislabcol = "grey", 
             caxislabels = seq(0, 100, 20), 
             vlcex = 0.8,
             title = title)
}

plot_radar <- function(data, title, color) {
  par(cex.main = 1.5, cex.axis = 0.5, cex.lab = 1.5)  # Adjust font sizes here
  radarchart(data, 
             axistype = 1, 
             pcol = color, 
             pfcol = scales::alpha(color, 0.6), 
             plwd = 2, 
             cglcol = "darkgrey", 
             cglty = 1, 
             axislabcol = "darkgrey", 
             caxislabels = seq(0, 100, 20), 
             vlcex = 0.8,
             title = title)
}

# Open a JPEG device
svg("combined_radar_charts.svg", width = 13.5, height = 8)

par(mfrow = c(1, 3), mar = c(2, 1, 2, 1), oma = c(2, 0, 2, 0))

# Plot radar charts for HQ, MQ, and LQ
plot_radar(data_HQ_radar, "HQ MAGs", "#1b9e77")
plot_radar(data_MQ_radar, "MQ MAGs", "#7570b3" )
plot_radar(data_LQ_radar, "LQ MAG", "#d95f02")

# Reset par settings to default after plotting
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1, oma = c(0, 0, 0, 0))

# Close the JPEG device
dev.off()

############################################################################################################


# Aggregate data to find the total number of bins recovered per method
aggregated_data <- data %>%
  group_by(Combination) %>%
  summarize(total_bins = sum(Bins, na.rm = TRUE))

# Prepare data for radar plot
# Add min and max rows for radar plot scaling
radar_data <- rbind(
  max = rep(max(aggregated_data$total_bins), ncol(aggregated_data)),
  min = rep(0, ncol(aggregated_data)),
  aggregated_data
)

# Convert Combination column to row names
row.names(radar_data) <- radar_data$Combination
radar_data <- radar_data %>% select(-Combination)

# Create radar plot
radar_chart <- radarchart(radar_data, axistype = 1,
                          pcol = rgb(0.2, 0.5, 0.5, 0.9), 
                          pfcol = rgb(0.2, 0.5, 0.5, 0.5), 
                          plwd = 2, 
                          cglcol = "grey", 
                          cglty = 1, 
                          axislabcol = "grey", 
                          caxislabels = seq(0, max(aggregated_data$total_bins), by = 5), 
                          cglwd = 0.8,
                          vlcex = 0.8)
#####################################################################################################################



