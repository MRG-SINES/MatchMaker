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

setwd ("F:/Ph.D/Research Projects/Match Making/Results")


# Read the CSV file
df <- read.csv("Kaiju_GTDB-TK-Merged_Final_V4.csv", header = TRUE, stringsAsFactors = FALSE)

fill_color <- brewer.pal(8, "Set1")[1:10]  # Using the first three colors from Paired
outline_color <- brewer.pal(8, "Paired")[1:10]

###################################With Kaiju ###################################################
# Calculate number of taxa per Sample and Method
taxa_counts <- df %>%
  group_by(SampleID, Method) %>%
  summarize(num_taxa = n_distinct(taxon_id)) %>%
  ungroup() %>%
  arrange(desc(num_taxa))  # Arrange in descending order by num_taxa


ggplot(taxa_counts, aes(x = reorder(Method, -num_taxa), y = num_taxa)) +
  geom_boxplot(fill = fill_color, color = outline_color) +
  labs(x = "Method", y = "Number of Taxa", title = "Number of Taxa Recovered by Method for each Sample") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

  
  #################################################Presence Absence Plots#########################################################
  
  # Load necessary libraries
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(reshape2)
  
  # Read the CSV file
  df <- read.csv("Kaiju_GTDB-TK-Merged_Final_V4.csv", header = TRUE, stringsAsFactors = FALSE)
  
  # Filter species where percentage is greater than 0.1%
  df_filtered <- df %>%
    filter(percent > 0.1, Method != "Kaiju")
  
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
    scale_fill_manual(values = c("0" = "black", "1" = "goldenrod1"), name = "Low Abundnace Species Recovered") +
    labs(x = NULL, y = NULL, title = NULL) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1),
          axis.text.y = element_text(size = 8),
          legend.position = "bottom")+
   coord_flip()
  plot_taxa
  
  #################################Low Abundance Species ############################################
  # Filter data to include only species with percent <= 1 and >= 0.05, and Method != "Kaiju"
  df_filtered_low <- df %>%
    filter(percent <= 1 & percent >= 0.05, Method != "Kaiju")
  
  # Calculate the mean percent of each species per method
  mean_percent_per_species <- df_filtered_low %>%
    group_by(taxon_name, Method) %>%
    summarize(mean_percent = mean(percent), .groups = 'drop')
  
  # Arrange species by mean percent in ascending order
  taxa_order <- mean_percent_per_species %>%
    group_by(taxon_name) %>%
    summarize(min_mean_percent = min(mean_percent), .groups = 'drop') %>%  # Get minimum mean percent for each species
    arrange(min_mean_percent) %>%  # Arrange taxa in ascending order of mean percent
    pull(taxon_name)  # Extract taxa names
  
  # Create a presence-absence matrix
  presence_absence_low <- df_filtered_low %>%
    mutate(presence = 1) %>%  # Add a column to indicate presence
    select(taxon_id, taxon_name, Method, presence) %>%  # Select necessary columns
    distinct() %>%  # Remove duplicate entries
    pivot_wider(names_from = Method, values_from = presence, values_fill = list(presence = 0))  # Pivot methods into columns and fill NAs with 0
  
  # Write the presence-absence matrix to a CSV file
  write.csv(presence_absence_low, "presence_absence_of_Low_Species_per_method.csv")
  
  # Convert to long format for ggplot2
  presence_absence_long_low <- melt(presence_absence_low, id.vars = c("taxon_id", "taxon_name"))
  
  # Create the heatmap
  plot_taxa_low <- ggplot(presence_absence_long_low, aes(x = variable, y = taxon_name)) +
    geom_tile(aes(fill = factor(value)), color = NA) +
    scale_fill_manual(values = c("0" = "black", "1" = "goldenrod1"), name = " Species Recovered") +
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
  
  ######################################Reordering rows and columns for heatmap##############################
  # Load necessary libraries
  library(ggplot2)
  library(reshape2)
  library(dplyr)
  
  # Read the CSV file
  df <- read.csv("FinalResult.csv", header = TRUE, stringsAsFactors = FALSE)
  
  # Filter species where percentage is greater than 0.1%
  df_filtered <- df %>%
    filter(percent > 0.15)
  
  # Create presence-absence matrix
  presence_absence <- df_filtered %>%
    mutate(presence = 1) %>%  # Add a column to indicate presence
    select(taxon_id, taxon_name, Method, presence) %>%  # Select necessary columns
    distinct() %>%  # Remove duplicate entries
    pivot_wider(names_from = Method, values_from = presence, values_fill = list(presence = 0))  # Pivot methods into columns and fill NAs with 0
  
  # Calculate the number of taxa recovered per method
  method_taxa_counts <- presence_absence %>%
    select(-taxon_id, -taxon_name) %>%
    summarise_all(sum) %>%
    gather(key = "Method", value = "Taxa_Count") %>%
    arrange(desc(Taxa_Count))
  
  # Reorder methods based on the number of recovered taxa
  method_order <- method_taxa_counts$Method
  
  # Convert to long format for ggplot2
  presence_absence_long <- melt(presence_absence, id.vars = c("taxon_id", "taxon_name"))
  
  # Set the factor levels for methods based on the ordered methods
  presence_absence_long$variable <- factor(presence_absence_long$variable, levels = method_order)
  
  # Create the heatmap
  plot_taxa <- ggplot(presence_absence_long, aes(x = variable, y = taxon_name)) +
    geom_tile(aes(fill = factor(value)), color = NA) +
    scale_fill_manual(values = c("0" = "black", "1" = "goldenrod1"), name = "Species Recovered") +
    labs(x = NULL, y = NULL, title = NULL) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1),
          axis.text.y = element_text(size = 8),
          legend.position = "bottom") +
    coord_flip()
  
  # Print the plot
  print(plot_taxa)
  
##############################################Upset Plot##############################################################
  # Install required package
  # install.packages("UpSetR")
  library(UpSetR)
  library(dplyr)
  library(tidyr)
  
  # Read the CSV file
  df <- read.csv("FinalResult.csv", header = TRUE, stringsAsFactors = FALSE)
  
  # Filter species where percentage is greater than 0.1%
  df_filtered <- df %>%
    filter(percent <= 0.1 & percent >=0.05)
  
  # Create presence-absence matrix without the percent column
  presence_absence <- df_filtered %>%
    mutate(presence = 1) %>%  # Add a column to indicate presence
    select(taxon_name, Method, presence) %>%  # Select necessary columns
    distinct() %>%  # Remove duplicate entries
    pivot_wider(names_from = Method, values_from = presence, values_fill = list(presence = 0))  # Pivot methods into columns and fill NAs with 0
  
  # Convert to the format suitable for UpSetR
  upset_data <- as.data.frame(presence_absence)
  upset_data <- as.data.frame(lapply(upset_data, as.integer))  # Convert logical columns to integer
  
  # Create the UpSet plot
  species_stats <- upset(upset_data, sets = colnames(upset_data)[2:ncol(upset_data)], 
        order.by = "freq", 
        main.bar.color = "goldenrod2", 
        sets.bar.color = "darkgreen")
  species_stats
  
  
  ###################################################Bubble Plot###########################################################
  # Load required libraries
  library(ggplot2)
  library(viridis)
  library(dplyr)
  
  # Read the CSV file (replace "FinalResult.csv" with your actual file path)
  df <- read.csv("FinalResult.csv", header = TRUE, stringsAsFactors = FALSE)
  
  # Filter and select the maximum percent value for each taxa-method combination
  df_filtered <- df %>%
    filter(percent > 1) %>%  # Filter taxa with percent > 1
    group_by(Method, taxon_name) %>%
    summarize(max_percent = max(percent)) %>%
    ungroup()
  
  # Calculate the number of taxa recovered per method
  taxa_counts <- df_filtered %>%
    group_by(Method) %>%
    summarise(Num_Taxa = n_distinct(taxon_name)) %>%
    arrange(desc(Num_Taxa))  # Arrange methods in descending order by number of taxa
  
  # Convert Method to factor with specified levels (descending order of taxa counts)
  df_filtered$Method <- factor(df_filtered$Method, levels = taxa_counts$Method)
  
  # Arrange taxa by max_percent in ascending order
  df_filtered <- df_filtered %>%
    arrange(max_percent) 
  
  # Create the bubble plot
  bubble_plot <- ggplot(df_filtered, aes(x = Method, y = reorder(taxon_name, max_percent), size = max_percent, fill = Method)) +
    geom_point(color = "black", alpha = 0.8, shape = 21) +  # Outline color and transparency
    scale_size_continuous(range = c(1, 10), guide = "legend") +  # Adjust the range of bubble sizes
    scale_fill_viridis_d(option = "D", guide = "none") +  # Viridis color palette for fill and remove legend for fill
    labs(x = " ", y = " ", title = " ") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          axis.text.y = element_text(size = 8)) +  # Adjust text size
    guides(size = guide_legend(title = "Species Abundance (%)")) +  # Legend for bubble sizes
    coord_flip()
  
  # Print the bubble plot
  print(bubble_plot)
  
  ######################################## Combining Plots########################################
  
  # Adjust the size of each plot
  plot_width <- 4  # You can adjust this value
  plot_height <- 2  # You can adjust this value
  
  # Remove legend and title from each plot
  plot1 <- plot_MAGs_No + theme(legend.position = "none", plot.title = element_blank())
  plot2 <- plot_taxa + theme(legend.position = "bottom", plot.title = element_blank())
  
  combined_plot1 <- ggarrange(species_stats, plot1, nrow=1, ncol=2, widths = c(plot_width, plot_width), labels=c("A", "B"))
  combined_taxa <- ggarrange(combined_plot1, plot2, nrow=2, ncol=1, widths = c(plot_width, plot_width), labels=c( " ", "C"))
  print(combined_taxa)
  
  
  #Save the plot as SVG
  ggsave("Taxa.svg", width = 13.5, height = 8, dpi=600)
  # Save the plot as PDF
  ggsave("Taxa.pdf",width = 13.5, height = 8, dpi=600)