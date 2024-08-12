# Load necessary package
library(dplyr)
library(wesanderson)

setwd("F:/Ph.D/Research Projects/Match Making/Skin Samples")

# Specify the path to your text file
#file_path <- "CheckM_Merged.txt"

# Read the data from the file
#data <- read.table(file_path, header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# Read the CSV file
data <- read.csv("CheckM_Skin_result_with_quality.csv", header = TRUE, stringsAsFactors = FALSE)

##############################Caluclating Completenss and Contamination of MAGs##############################
# Calculate the mean and standard deviation of Completeness and Contamination per Method
stats_summary <- data %>%
  group_by(Method) %>%
  summarize(
    Mean_Completeness = mean(Completeness, na.rm = TRUE),
    SD_Completeness = sd(Completeness, na.rm = TRUE),
    Mean_Contamination = mean(Contamination, na.rm = TRUE),
    SD_Contamination = sd(Contamination, na.rm = TRUE)
  )

# Calculate the mean Completeness and Contamination per Method
mean_stats <- data %>%
  group_by(Method) %>%
  summarize(
    Mean_Completeness = mean(Completeness, na.rm = TRUE),
    Mean_Contamination = mean(Contamination, na.rm = TRUE)
  )
#############################################Plotting Completeness###################################################################
# Scale Completeness and Contamination to a 0-100 scale
data_scaled <- data %>%
  mutate(
    Contamination_Scaled = rescale(Contamination, to = c(0, 10)),
    Completeness_Scaled = rescale(Completeness, to = c(0, 100))
  )

# Calculate the means of the groups for each Combination
mean_data_cont <- aggregate(Contamination ~ Method, data = data_scaled, FUN = mean)

# Calculate the means of the groups for each Combination
mean_data_comp <- aggregate(Completeness ~ Method, data = data_scaled, FUN = mean)

# Create scatter plot with Contamination on the x-axis and Completeness on the y-axis, faceted by Method
ggplot(data_scaled, aes(x = Contamination_Scaled, y = Completeness_Scaled, color = Method)) +
  geom_point(alpha = 0.6) +  # Scatter plot with some transparency
  facet_wrap(~ Method) +  # Facet by Method with free scales
  labs(
    title = " ",
    x = "Contamination (%)",
    y = "Completeness (%)"
  ) +
  scale_color_manual(values = wes_palette("FantasticFox1", n = length(unique(data_scaled$Method)), type = "continuous")) +  # Wes Anderson color palette
  scale_x_continuous(limits = c(0, 10)) +  # Limit the x-axis scale to 0-50
  theme_minimal() +  # Minimal theme for better aesthetics
  theme(
    legend.position = "none",  # Remove the legend
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.text.x = element_text( hjust = 1)  # Angle x-axis text for readability
  )

#####################################MAGs Quality %##########################################################

# Read the CSV file
data <- read.csv("CheckM_Skin_result_with_quality.csv", header = TRUE, stringsAsFactors = FALSE)

# Read the data from the file
data <- read.table(file_path, header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# Print the first few rows of the data to verify
print("First few rows of the data:")
print(head(data))

# Replace MAGs_Quality values to remove " Quality" and just have "High", "Medium", "Low"
data$MAGs_Quality <- gsub(" Quality", "", data$MAGs_Quality)

# Print the modified MAGs_Quality column to verify changes
print("Modified MAGs_Quality column:")
print(unique(data$MAGs_Quality))

# Calculate the count and percentage of each quality category per method
quality_summary <- data %>%
  group_by(Method, MAGs_Quality) %>%
  summarize(
    Count = n(),  # Count of each quality category
    .groups = 'drop'
  )

# Print the quality count data
print("Quality count per method:")
print(quality_summary)

# Calculate the total counts per method
total_count <- quality_summary %>%
  group_by(Method) %>%
  summarize(Total = sum(Count), .groups = 'drop')

# Print the total counts per method
print("Total counts per method:")
print(total_count)

# Join the counts with total counts to calculate percentages
quality_percentage <- quality_summary %>%
  left_join(total_count, by = "Method") %>%
  mutate(
    Percentage = (Count / Total) * 100  # Calculate percentage
  ) %>%
  pivot_wider(
    names_from = MAGs_Quality,
    values_from = c(Count, Percentage),
    values_fill = list(Count = 0, Percentage = 0)
  )

# Print the quality percentage data
print("Quality percentage per method:")
print(quality_percentage)

quality_stats <- quality_percentage %>%
  group_by(Method) %>%
  summarize(
    Total_MAGs = sum(Count_High + Count_Medium + Count_Low),
    Mean_High_Percentage = mean(Percentage_High, na.rm = TRUE),
    SD_High_Percentage = ifelse(n() > 1, sd(Count_High, na.rm = TRUE), NA),
    Mean_Medium_Percentage = mean(Percentage_Medium, na.rm = TRUE),
    SD_Medium_Percentage = ifelse(n() > 1, sd(Count_Medium, na.rm = TRUE), NA),
    Mean_Low_Percentage = mean(Percentage_Low, na.rm = TRUE),
    SD_Low_Percentage = ifelse(n() > 1, sd(Count_Low, na.rm = TRUE), NA)
  )

# Write the results to a new text file
write.table(quality_percentage, "quality_percentage_per_method.txt", sep = "\t", row.names = FALSE, quote = FALSE)
write.csv(quality_percentage, "quality_percentage_per_method.csv", row.names = FALSE)

# Print confirmation message
print("Quality percentage data saved to 'quality_percentage_per_method.txt' and 'quality_percentage_per_method.csv'")

# Write the statistics to a new text file
write.table(quality_stats, "quality_stats_per_method.txt", sep = "\t", row.names = FALSE, quote = FALSE)
write.csv(quality_stats, "quality_stats_per_method.csv", row.names = FALSE)

################################Calculating Mean Data#################################################################

# Calculate mean counts per quality per method
quality_mean <- quality_summary %>%
  group_by(Method, MAGs_Quality) %>%
  summarise(Total_Count = sum(Count), .groups = 'drop') %>%
  mutate(
    Mean = Total_Count / 9  # Divide each quality count per method by 9
  ) %>%
  pivot_wider(
    names_from = MAGs_Quality,
    values_from = c(Total_Count, Mean),
    values_fill = list(Total_Count = 0, Mean = 0)
  )

# Calculate mean counts and standard deviation per quality across methods
quality_summary_stats <- quality_summary %>%
  group_by(Method, MAGs_Quality) %>%
  summarise(Total_Count = sum(Count), .groups = 'drop') %>%
  group_by(MAGs_Quality) %>%
  summarise(
    Mean = mean(Total_Count),
    SD = sd(Total_Count),  # Calculate standard deviation of counts
    .groups = 'drop'
  ) %>%
  pivot_wider(
    names_from = MAGs_Quality,
    values_from = c(Mean, SD),
    values_fill = list(Mean = 0, SD = 0)
  )



