# Load necessary libraries
library(dplyr)
library(tidyr)
library(fmsb)
library(tidyverse)
library(ggplot2)
library(ggridges)
library(scales)


setwd ("F:/Ph.D/Research Projects/Match Making/Results/Plots/Binning")

# Example data based on your MAGs data
data <- data.frame(
  Combination = c("metaSPAdes_MetaBAT2", "metaSPAdes_MaxBin2.0", "metaSPAdes_CONCOCT",
                  "MEGAHIT_MetaBAT2", "MEGAHIT_MaxBin2.0", "MEGAHIT_CONCOCT",
                  "IDBA_UD_MetaBAT2", "IDBA_UD_MaxBin2.0", "IDBA_UD_CONCOCT"),
  HQ = c(27.313167, 26.593137, 14.233419, 23.099851, 23.183620, 12.38781, 19.217082, 20.971867, 9.155095),
  MQ = c(25.266904, 22.242647, 8.074935, 28.117238, 24.768824, 7.879186, 30.130486, 24.893436, 8.684396),
  LQ = c(47.41993, 51.16422, 77.69165, 48.78291, 52.04756, 79.69165, 50.661, 54.13470, 82.16051)
)

# Transform the data to wide format for each quality level
data_HQ <- data %>% select(Combination, HQ) %>% pivot_wider(names_from = Combination, values_from = HQ)
data_MQ <- data %>% select(Combination, MQ) %>% pivot_wider(names_from = Combination, values_from = MQ)
data_LQ <- data %>% select(Combination, LQ) %>% pivot_wider(names_from = Combination, values_from = LQ)

# Check the transformed data
print("Data HQ:")
print(data_HQ)
print("Data MQ:")
print(data_MQ)
print("Data LQ:")
print(data_LQ)

# Prepare the data for radar chart by adding min and max rows
prepare_radar_data <- function(df) {
  rbind(rep(100, ncol(df)), rep(0, ncol(df)), df)
}

data_HQ_radar <- prepare_radar_data(data_HQ)
data_MQ_radar <- prepare_radar_data(data_MQ)
data_LQ_radar <- prepare_radar_data(data_LQ)

# Check the radar data
print("Radar Data HQ:")
print(data_HQ_radar)
print("Radar Data MQ:")
print(data_MQ_radar)
print("Radar Data LQ:")
print(data_LQ_radar)

plot_radar <- function(data, title, color) {
  radarchart(data, 
             axistype = 1, 
             pcol = color, 
             pfcol = scales::alpha(color, 0.6), 
             plwd = 2, 
             cglcol = "darkgrey", 
             cglty = 1, 
             axislabcol = "darkgrey", 
             caxislabels = seq(0, 100, 20), 
             cex=2,
             vlcex = 2,
             calcex=2.5,
    )
  # Add the title with a specific size
  title(main = title, cex.main = 3,5)
}

#Open a SVG device
svg("combined_radar_charts_New2.svg", width = 38, height = 15)

#Arrange the plots in a 1x3 grid
par(mfrow = c(2, 3), mar = c(2, 1, 2, 1), oma = c(2, 0, 2, 0))

#Arrange the plots in a 1x3 grid
#par(mfrow = c(1, 3), mar = c(3, 3, 3, 3), oma = c(2, 0, 2, 0))

# Plot radar charts for HQ, MQ, and LQ
plot_radar(data_HQ_radar, "High Quality", "#1b9e77")
plot_radar(data_MQ_radar, "Medium Quality", "#7570b3")
plot_radar(data_LQ_radar, "Low Quality", "#d95f02")

# Reset par settings to default after plotting
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1, oma = c(0, 0, 0, 0))

# Close the SVG device
dev.off()

# Check if the file is created
file_created <- file.exists("combined_radar_charts_New.svg")
print(paste("File created:", file_created))


######################################################Total No of MAGs Per Combination################################################################

#Load necessary libraries
library(tidyverse)
library(fmsb)

#Read the data
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


# Calculate the standard deviations of the bins for each Combination
sd_data <- data %>%
  group_by(Combination) %>%
  summarize(sd_bins = sd(Bins, na.rm = TRUE))

# Aggregate data to find the total number of bins recovered per method
aggregated_data <- data %>%
  group_by(Combination) %>%
  summarize(total_bins = sum(Bins, na.rm = TRUE))

# Prepare the data for radar chart by adding min and max rows
prepare_radar_data <- function(df) {
  max_vals <- max(df$total_bins)  # Max value for total_bins
  min_vals <- 0  # Min value is 0
  radar_df <- rbind(rep(max_vals, ncol(df)), rep(min_vals, ncol(df)), t(df$total_bins))
  colnames(radar_df) <- df$Combination
  return(as.data.frame(radar_df))
}

#Prepare radar data
radar_data <- prepare_radar_data(aggregated_data)

#Check radar data structure
print(radar_data)

# Define a function to plot radar charts
plot_radar <- function(data, title, color) {
  max_val <- max(data, na.rm = TRUE)  # Find the maximum value in the data
  radarchart(data,
             axistype = 1,
             pcol = color,
             pfcol = scales::alpha(color, 0.6),
             plwd = 2,
             cglcol = "darkgrey",
             cglty = 1,
             axislabcol = "darkgrey",
             caxislabels = seq(0, max_val, by = 1000),  # Adjust the scale here
             vlcex = 0.8,
             title = title)
}


#Open a SVG device
svg("MAGs_radar_charts_MAGs.svg", width = 13.5, height = 8)

#Arrange the plots in a 1x3 grid
par(mfrow = c(1, 1), mar = c(2, 1, 2, 1), oma = c(2, 0, 2, 0))

#Plot radar chart for total bins
plot_radar(radar_data, "Total No. of MAGs", "#92C5DE")

#Reset par settings to default after plotting
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1, oma = c(0, 0, 0, 0))

#Close the SVG device
dev.off()

#Check if the file is created
file_created <- file.exists("MAGs_radar_charts_New.svg")
print(paste("File created:", file_created))
               

#############################################################################################

# Install required packages if not already installed
# install.packages("rsvg")
install.packages("cairoDevice")
install.packages("svglite")

# Load the required library
library(svglite)

# Define the file paths
svg_file <- "combined_radar_charts_New2.svg"
pdf_file <- "combined_radar_charts_New2.pdf"

# Plot and save as SVG using svglite
svglite(svg_file, width = 42, height = 25)







dev.off()

# Convert the SVG file to PDF using an external tool
# For instance, you might use a command-line tool or an online converter
