# Install and load necessary libraries
# Install the dunn.test package (if not already installed)
install.packages("dunn.test")

# Load the package
library(dunn.test)

library(ggplot2)
library(ggpubr)
library(ggthemes)
library(dplyr)
library(RColorBrewer)
library(cowplot)

#########################################Plotting Bins#########################################3
setwd ("F:/CAMI Dataset/")

# Read the text file (assuming it's tab-delimited)
data <- read.delim("Test.txt")  # Make sure to replace "your_file.txt" with the actual file path

#Generating the comparison list for pairwise comparisons
my.comparisons <- list( c("MaxBin-MEGAHIT", "MaxBin-METABAT"),c("MaxBin-MEGAHIT", "MaxBin-metaSPAdes"), c("MaxBin-MEGAHIT", "METABAT-MEGAHIT") )

# Identify problematic entries
problematic_entries <- data[!grepl("^\\d+$", data$Bins), "Bins"]

# Print problematic entries
print(problematic_entries)

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

# Save mean_data as a CSV file
write.csv(mean_data, "mean_data_bins.csv", row.names = FALSE)

plotTest <- ggplot(data, aes(x = Combination, y = Bins, fill = Combination)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.09, fill = "white", color = "black", outlier.shape = NA) +
  labs(title = "Mean Bins by Combination",
       x = "Combination",
       y = "Mean Bins") +
  scale_fill_manual(values = fill_color) +
  stat_compare_means(aes(label = ..p.signif..), comparisons = my.comparisons) +
  theme(aspect.ratio = 1) +
  guides(fill = guide_legend(title = "K-mer set")) +  # Adjust fill legend title
  theme_minimal_grid() +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.8, hjust=1))  # Rotate x-axis labels vertically

print(plotTest)

#Save the plot as SVG
ggsave("violin_plot_bins.svg", width = 13.5, height = 7)

#################################################################################################################
# Calculate the means of the groups for each Combination
mean_data <- aggregate(Bins ~ Combination, data = data, FUN = mean)

# Create the bar plot
plot_bins <- ggplot(mean_data, aes(x = Combination, y = Bins, fill = Combination)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Bins by Combination",
       x = "Combination",
       y = "Mean Bins") +
  scale_fill_brewer(palette = "Set3") +  # You can choose a different color palette
  theme_minimal()

# Perform Kruskal-Wallis test
kruskal_result <- kruskal.test(Bins ~ Combination, data = data)

# Add Kruskal-Wallis test result to the plot
plot_bins <- plot_bins +
  annotate("text", x = 1, y = max(mean_data$Bins), label = paste("Kruskal-Wallis p =", signif(kruskal_result$p.value, digits = 3)), vjust = 1, hjust = 1) +
  theme(legend.position="none")  # Remove legend, as it's not relevant for Kruskal-Wallis

# Print the plot
print(plot_bins)

#Save the plot as SVG
ggsave("Barn_plot_bins.svg", width = 13.5, height = 7)
#############################
# Assuming 'data' is your dataframe and 'Group' is the grouping variable
anova_result <- aov(Bins ~ Combination, data = data)
summary(anova_result)

# Assuming 'data' is your dataframe and 'Combination' is the grouping variable
anova_result <- aov(Bins ~ Combination, data = data)

# Check if the anova_result object is not empty
if (!is.null(anova_result)) {
  # Check for overall significance by looking at the p-value in the ANOVA table
  anova_table <- anova(anova_result)
  p_value <- anova_table$"Pr(>F)"[1]  # Assuming the first row corresponds to the 'Combination' factor
  
  if (!is.na(p_value) && p_value < 0.05) {
    # Perform post-hoc test (Tukey's HSD)
    posthoc_result <- TukeyHSD(anova_result)
    print(posthoc_result)
  } else {
    cat("ANOVA result is not statistically significant.\n")
  }
} else {
  cat("ANOVA result is empty or not available.\n")
}
###################################################Plotting no. of contigs#############################

# Identify problematic entries in "Contigs"
problematic_entries <- data[!grepl("^\\d+$", data$Contigs), "Contigs"]

# Print problematic entries
print(problematic_entries)

# Clean data: Remove non-numeric characters and convert to numeric
data$Contigs <- as.numeric(gsub("[^0-9]", "", data$Contigs))

# Check for NAs in "Contigs" after conversion
if (any(is.na(data$Contigs))) {
  stop("Error: Some values in 'Contigs' could not be converted to numeric.")
}

# Calculate the means of the groups for each Combination
mean_data <- aggregate(Contigs ~ Combination, data = data, FUN = mean)

# Save mean_data as a CSV file
write.csv(mean_data, "mean_data_contigs.csv", row.names = FALSE)

# Create the ggplot for Contigs
plotTest <- ggplot(data, aes(x = Combination, y = Contigs, fill = Combination)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.09, fill = "white", color = "black", outlier.shape = NA) +
  labs(title = "Mean Contigs by Combination",
       x = "Combination",
       y = "Mean Contigs") +
  scale_fill_manual(values = fill_color) +
  stat_compare_means(aes(label = ..p.signif..), comparisons = my.comparisons) +
  theme(aspect.ratio = 1) +
  guides(fill = guide_legend(title = "K-mer set")) +  # Adjust fill legend title
  theme_minimal_grid() +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.8, hjust = 1))  # Rotate x-axis labels vertically

print(plotTest)

# Save the plot as SVG
ggsave("violin_plot_contigs.svg", plot = plotTest, width = 13.5, height = 7)
##################################################Binned Contigs###################################

# Identify problematic entries
problematic_entries <- data[!grepl("^\\d+$", data$BinnedContig), "BinnedContig"]

# Print problematic entries
print(problematic_entries)

# Clean data: Remove non-numeric characters and convert to numeric
data$BinnedContig <- as.numeric(gsub("[^0-9]", "", data$BinnedContig))

# Check for NAs in "BinnedContig" after conversion
if (any(is.na(data$BinnedContig))) {
  stop("Error: Some values in 'BinnedContig' could not be converted to numeric.")
}

# Calculate the means of the groups for each Combination
mean_data <- aggregate(BinnedContig ~ Combination, data = data, FUN = mean)
# Save mean_data as a CSV file
write.csv(mean_data, "mean_data_binnedcontigs.csv", row.names = FALSE)

# Generate the plot
plotTest <- ggplot(data, aes(x = Combination, y = BinnedContig, fill = Combination)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.09, fill = "white", color = "black", outlier.shape = NA) +
  labs(title = "Mean BinnedContig by Combination",
       x = "Combination",
       y = "Mean BinnedContig") +
  scale_fill_manual(values = fill_color) +
  stat_compare_means(aes(label = ..p.signif..), comparisons = my.comparisons) +
  theme(aspect.ratio = 1) +
  guides(fill = guide_legend(title = "K-mer set")) +  # Adjust fill legend title
  theme_minimal_grid() +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.8, hjust = 1))  # Rotate x-axis labels vertically

print(plotTest)

# Save the plot as SVG
ggsave("violin_plot_binnedcontig.svg", plot = plotTest, width = 13.5, height = 7)

