# Load the package
library(dunn.test)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(dplyr)
library(RColorBrewer)
library(cowplot)
library(ggridges)

############################Plotting Assembly Length by Different Assemblers#####################################
setwd ("F:/Ph.D/Research Projects/Match Making/Results/Plots/Assembly/Assembly Plots")

# Read the text file (assuming it's tab-delimited)
data <- read.delim("Assembly_Stats.txt")  

#Generating the comparison list for pairwise comparisons
my.comparisons <- list( c("MEGAHIT", "metaSPAdes"),c("MEGAHIT", "IDBA-UD"), c("metaSPAdes", "IDBA-UD"))

# Define custom colors for fill and outline
fill_color <- brewer.pal(8, "Set3")[4:6]  # Using the first three colors from Paired
outline_color <- c("brown3", "dodgerblue4", "darkorange3")


#Defining function for customizing y axis
label_number <- function(scale = 1, suffix = "") {
  function(x) {
    scales::number_format(scale = scale, suffix = suffix)(x)
  }
}

# Convert "N5O bp" column to kbp
data$TotalLengthKb <- data$TotalLength / 1000

# Calculate the means of the groups for each Combination
mean_data <- aggregate(TotalLengthKb ~ Assembler, data = data, FUN = mean)

# Calculate the standard deviations of the groups for each Combination
sd_data <- aggregate(TotalLengthKb ~ Assembler, data = data, FUN = sd)

# Create the plot
plot_TotalLength <- ggplot(data, aes(y = TotalLengthKb, x = Assembler, fill = Assembler)) +
  geom_violin(trim = TRUE, width = 0.6, linewidth = 1.2, aes(color = Assembler)) +
  geom_jitter(aes(color = Assembler), width = 0.2, size = 1.5, alpha = 0.6) +
  #geom_boxplot(width = 0.1, fill = "white", color = "black", outlier.shape = NA) +
  labs(x= NULL, y = "Assembly Length (Kbp)", color = "Assembler") +
  scale_fill_manual(values = fill_color) +
  scale_color_manual(values = outline_color) +
  stat_compare_means(aes(label = ..p.signif..), comparisons = my.comparisons) +
  theme_classic() +
  theme(aspect.ratio = 1) +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "k")) +  # Convert labels to k units
  theme(legend.position = "none")   # Remove the legend
  #theme(axis.text.x = element_text( vjust = 0.8, hjust = 1))  # Rotate x-axis labels vertically
 
# Print the plot
print(plot_TotalLength)

#Save the plot as SVG
ggsave("Assembly_Length_No_Jitter.svg", width = 13.5, height = 7)
# Save the plot as PDF
ggsave("Assembly_Length.pdf",width = 13.5, height = 7)

#####################################################Skin Samples###################################################
setwd ("F:/Ph.D/Research Projects/Match Making/Results/Plots/Assembly/Skin")

# Read the text file (assuming it's tab-delimited)
data <- read.delim("Assembly_Skin.txt")  

#Generating the comparison list for pairwise comparisons
my.comparisons <- list( c("MEGAHIT", "metaSPAdes"),c("MEGAHIT", "IDBA-UD"), c("metaSPAdes", "IDBA-UD"))

# Define custom colors for fill and outline
fill_color <- brewer.pal(8, "Set3")[4:6]  # Using the first three colors from Paired
outline_color <- c("brown3", "dodgerblue4", "darkorange3")


#Defining function for customizing y axis
label_number <- function(scale = 1, suffix = "") {
  function(x) {
    scales::number_format(scale = scale, suffix = suffix)(x)
  }
}

# Convert "N5O bp" column to kbp
data$TotalLengthKb <- data$TotalLength / 1000

# Calculate the means of the groups for each Combination
mean_data <- aggregate(TotalLengthKb ~ Assembler, data = data, FUN = mean)

# Create the plot
plot_TotalLength_Skin <- ggplot(data, aes(y = TotalLengthKb, x = Assembler, fill = Assembler)) +
  geom_violin(trim = TRUE, width = 0.6, linewidth = 1.2, aes(color = Assembler)) +
  geom_jitter(aes(color = Assembler), width = 0.2, size = 1.5, alpha = 0.6) +
  #geom_boxplot(width = 0.1, fill = "white", color = "black", outlier.shape = NA) +
  labs(x= NULL, y = "Assembly Length (Kbp)", color = "Assembler") +
  scale_fill_manual(values = fill_color) +
  scale_color_manual(values = outline_color) +
  stat_compare_means(aes(label = ..p.signif..), comparisons = my.comparisons) +
  theme_classic() +
  theme(aspect.ratio = 1) +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "k")) +  # Convert labels to k units
  theme(legend.position = "none")   # Remove the legend
#theme(axis.text.x = element_text( vjust = 0.8, hjust = 1))  # Rotate x-axis labels vertically

# Print the plot
print(plot_TotalLength_Skin)

#Save the plot as SVG
ggsave("Assembly_Length_No_Jitter.svg", width = 13.5, height = 7)
# Save the plot as PDF
ggsave("Assembly_Length.pdf",width = 13.5, height = 7)

############################Plotting Assembly Length by Different Assemblers#####################################
setwd ("F:/CAMI Dataset/Assembly Plots")

# Read the text file (assuming it's tab-delimited)
data <- read.delim("Assembly_Stats.txt")  

#Generating the comparison list for pairwise comparisons
my.comparisons <- list( c("MEGAHIT", "metaSPAdes"),c("MEGAHIT", "IDBA-UD"), c("metaSPAdes", "IDBA-UD"))

# Define custom colors for fill and outline
fill_color <- brewer.pal(8, "Set1")[3:5]  # Using the first three colors from Paired
outline_color <- c("forestgreen","darkmagenta", "darkorange3")


#Defining function for customizing y axis
label_number <- function(scale = 1, suffix = "") {
  function(x) {
    scales::number_format(scale = scale, suffix = suffix)(x)
  }
}

# Convert "N5O bp" column to kbp
data$TotalLengthKb <- data$TotalLength / 1000

# Calculate the means of the groups for each Combination
mean_data <- aggregate(TotalLengthKb ~ Assembler, data = data, FUN = mean)

# Create the plot
plot_TotalLength <- ggplot(data, aes(y = TotalLengthKb, x = Assembler, fill = Assembler)) +
  geom_violin(trim = TRUE, width = 0.6, linewidth = 1.2, aes(color = Assembler)) +
  geom_jitter(aes(color = Assembler), width = 0.2, size = 1.5, alpha = 0.6) +
  #geom_boxplot(width = 0.1, fill = "white", color = "black", outlier.shape = NA) +
  labs(x= NULL, y = "Assembly Length (Kbp)", color = "Assembler") +
  scale_fill_manual(values = fill_color) +
  scale_color_manual(values = outline_color) +
  stat_compare_means(aes(label = ..p.signif..), comparisons = my.comparisons) +
  theme_classic() +
  theme(aspect.ratio = 1) +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "k")) +  # Convert labels to k units
  theme(legend.position = "none")   # Remove the legend
#theme(axis.text.x = element_text( vjust = 0.8, hjust = 1))  # Rotate x-axis labels vertically

# Print the plot
print(plot_TotalLength)

#Save the plot as SVG
ggsave("Assembly_Length_No_Jitter.svg", width = 13.5, height = 7)
# Save the plot as PDF
ggsave("Assembly_Length.pdf",width = 13.5, height = 7)

###########################################Rain Cloud Plots#####################################################################
# Convert "N5O bp" column to kbp
data$TotalLengthKb <- data$TotalLength / 1000

# Create the raincloud plot
plot_Raincloud <- ggplot(data, aes(x = Assembler, y = TotalLengthKb, fill = Assembler, color = Assembler)) +
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
  labs(x = "Assemblers", y = "Assembly Length (Kbp)", color = "Assembler", fill = "Assembler") +
  scale_fill_manual(values = fill_color) +
  scale_color_manual(values = outline_color) +
  stat_compare_means(aes(label = ..p.signif..), comparisons = my.comparisons) +
  theme_minimal() +
  theme(aspect.ratio = 1) +
  theme(legend.position = "none") +  # Remove the legend
  theme(axis.text.x = element_text(angle = 60, vjust = 0.8, hjust = 1))+  # Rotate x-axis labels vertically
  coord_flip()

# Print the plot
print(plot_Raincloud)
#Save the plot as SVG
ggsave("Assembly_Length_Rain_Cloud.svg", width = 13.5, height = 7)
###############################################No. of Contigs###################################################################

# Calculate the means of the groups for each Combination
mean_data <- aggregate(Contigs ~ Assembler, data = data, FUN = mean)

# Calculate the standard deviations of the groups for each Combination
sd_data <- aggregate(Contigs ~ Assembler, data = data, FUN = sd)

# Create the plot
plot_Contigs <- ggplot(data, aes(y = Contigs, x = Assembler, fill = Assembler)) +
  geom_violin(trim = TRUE, width = 0.6, linewidth = 1.2, aes(color = Assembler)) +
  geom_jitter(aes(color = Assembler), width = 0.2, size = 1.5, alpha = 0.6) +
  #geom_boxplot(width = 0.1, fill = "white", color = "black", outlier.shape = NA) +
  labs(x= NULL, y = "No. of Contigs", color = "Assembler") +
  scale_fill_manual(values = fill_color) +
  scale_color_manual(values = outline_color) +
  stat_compare_means(aes(label = ..p.signif..), comparisons = my.comparisons) +
  theme_classic() +
  theme(aspect.ratio = 1) +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "k")) +
  theme(legend.position = "none")   # Remove the legend
  #theme(axis.text.x = element_text(vjust = 0.8, hjust = 1))  # Rotate x-axis labels vertically

# Print the plot
print(plot_Contigs)

#Save the plot as SVG
ggsave("Total_Contigs_No_Jitter.svg", width = 13.5, height = 7)
# Save the plot as PDF
ggsave("Total_Contigs_No_Jitter.svg.pdf",width = 13.5, height = 7)

#########################################Skin####################################################

# Calculate the means of the groups for each Combination
mean_data <- aggregate(Contigs ~ Assembler, data = data, FUN = mean)

# Create the plot
plot_Contigs <- ggplot(data, aes(y = Contigs, x = Assembler, fill = Assembler)) +
  geom_violin(trim = TRUE, width = 0.6, linewidth = 1.2, aes(color = Assembler)) +
  geom_jitter(aes(color = Assembler), width = 0.2, size = 1.5, alpha = 0.6) +
  #geom_boxplot(width = 0.1, fill = "white", color = "black", outlier.shape = NA) +
  labs(x= NULL, y = "No. of Contigs", color = "Assembler") +
  scale_fill_manual(values = fill_color) +
  scale_color_manual(values = outline_color) +
  stat_compare_means(aes(label = ..p.signif..), comparisons = my.comparisons) +
  theme_classic() +
  theme(aspect.ratio = 1) +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "k")) +
  theme(legend.position = "none")   # Remove the legend
#theme(axis.text.x = element_text(vjust = 0.8, hjust = 1))  # Rotate x-axis labels vertically

# Print the plot
print(plot_Contigs)

#Save the plot as SVG
ggsave("Total_Contigs_No_Jitter.svg", width = 13.5, height = 7)
# Save the plot as PDF
ggsave("Total_Contigs_No_Jitter.svg.pdf",width = 13.5, height = 7)

################################ Max Contig Size ##############################################
# Convert " bp to kbp" column to kbp
data$MaxContigKb <- data$MaxContig / 1000

# Calculate the means of the groups for each Combination
mean_data <- aggregate(MaxContigKb ~ Assembler, data = data, FUN = mean)

# Calculate the standard deviations of the groups for each Combination
sd_data <- aggregate(MaxContigKb  ~ Assembler, data = data, FUN = sd)

# Create the plot
plot_MaxContig <- ggplot(data, aes(y = MaxContigKb, x = Assembler, fill = Assembler)) +
  geom_violin(trim = TRUE, width = 0.8, linewidth = 1.2, aes(color = Assembler)) +
  geom_jitter(aes(color = Assembler), width = 0.2, size = 1.5, alpha = 0.6) +
  #geom_boxplot(width = 0.1, fill = "white", color = "black", outlier.shape = NA) +
  labs(x= NULL, y = "Max. Contig Size (Kbp)", color = "Assembler") +
  scale_fill_manual(values = fill_color) +
  scale_color_manual(values = outline_color) +
  stat_compare_means(aes(label = ..p.signif..), comparisons = my.comparisons) +
  theme_classic() +
  theme(aspect.ratio = 1) +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "k")) +
  theme(legend.position = "none")   # Remove the legend
 # theme(axis.text.x = element_text(vjust = 0.8, hjust = 1))  # Rotate x-axis labels vertically

# Print the plot
print(plot_MaxContig)

#Save the plot as SVG
ggsave("plot_MaxContig_No_Jitter.svg", width = 13.5, height = 7)
# Save the plot as PDF
ggsave("plot_MaxContig_No_Jitter.svg.pdf",width = 13.5, height = 7)
###################################Skin##################################################################

# Convert " bp to kbp" column to kbp
data$MaxContigKb <- data$MaxContig / 1000

# Calculate the means of the groups for each Combination
mean_data <- aggregate(MaxContigKb ~ Assembler, data = data, FUN = mean)

# Create the plot
plot_MaxContig <- ggplot(data, aes(y = MaxContigKb, x = Assembler, fill = Assembler)) +
  geom_violin(trim = TRUE, width = 0.8, linewidth = 1.2, aes(color = Assembler)) +
  geom_jitter(aes(color = Assembler), width = 0.2, size = 1.5, alpha = 0.6) +
  #geom_boxplot(width = 0.1, fill = "white", color = "black", outlier.shape = NA) +
  labs(x= NULL, y = "Max. Contig Size (Kbp)", color = "Assembler") +
  scale_fill_manual(values = fill_color) +
  scale_color_manual(values = outline_color) +
  stat_compare_means(aes(label = ..p.signif..), comparisons = my.comparisons) +
  theme_classic() +
  theme(aspect.ratio = 1) +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "k")) +
  theme(legend.position = "none")   # Remove the legend
# theme(axis.text.x = element_text(vjust = 0.8, hjust = 1))  # Rotate x-axis labels vertically

# Print the plot
print(plot_MaxContig)

#Save the plot as SVG
ggsave("plot_MaxContig_No_Jitter.svg", width = 13.5, height = 7)
# Save the plot as PDF
ggsave("plot_MaxContig_No_Jitter.svg.pdf",width = 13.5, height = 7)

################################ N50 ##############################################
# Convert "N5O bp" column to kbp
data$N50Kb <- data$N50 / 1000

# Calculate the means of the groups for each Combination
mean_data <- aggregate(N50Kb ~ Assembler, data = data, FUN = mean)

# Calculate the standard deviations of the groups for each Combination
sd_data <- aggregate(N50Kb  ~ Assembler, data = data, FUN = sd)

# Create the plot
plot_N50 <- ggplot(data, aes(y = N50Kb, x = Assembler, fill = Assembler)) +
  geom_violin(trim = TRUE, width = 0.8, linewidth = 1.2, aes(color = Assembler)) +
  geom_jitter(aes(color = Assembler), width = 0.2, size = 1.5, alpha = 0.6) +
  #geom_boxplot(width = 0.1, fill = "white", color = "black", outlier.shape = NA) +
  labs(x= NULL, y = "N50 (Kbp)", color = "Assembler") +
  scale_fill_manual(values = fill_color) +
  scale_color_manual(values = outline_color) +
  stat_compare_means(aes(label = ..p.signif..), comparisons = my.comparisons) +
  theme_classic() +
  theme(aspect.ratio = 1) +
  theme(legend.position = "none")   # Remove the legend
  #theme(axis.text.x = element_text(vjust = 0.8, hjust = 1))  # Rotate x-axis labels vertically

# Print the plot
print(plot_N50)

#Save the plot as SVG
ggsave("plot_N50_No_Jitter.svg", width = 13.5, height = 7)
# Save the plot as PDF
ggsave("plot_N50_No_Jitter.svg.pdf",width = 13.5, height = 7)
###################################################Skin###########################################

# Convert "N5O bp" column to kbp
data$N50Kb <- data$N50 / 1000

# Calculate the means of the groups for each Combination
mean_data <- aggregate(N50Kb ~ Assembler, data = data, FUN = mean)

# Create the plot
plot_N50 <- ggplot(data, aes(y = N50Kb, x = Assembler, fill = Assembler)) +
  geom_violin(trim = TRUE, width = 0.8, linewidth = 1.2, aes(color = Assembler)) +
  geom_jitter(aes(color = Assembler), width = 0.2, size = 1.5, alpha = 0.6) +
  #geom_boxplot(width = 0.1, fill = "white", color = "black", outlier.shape = NA) +
  labs(x= NULL, y = "N50 (Kbp)", color = "Assembler") +
  scale_fill_manual(values = fill_color) +
  scale_color_manual(values = outline_color) +
  stat_compare_means(aes(label = ..p.signif..), comparisons = my.comparisons) +
  theme_classic() +
  theme(aspect.ratio = 1) +
  theme(legend.position = "none")   # Remove the legend
#theme(axis.text.x = element_text(vjust = 0.8, hjust = 1))  # Rotate x-axis labels vertically

# Print the plot
print(plot_N50)

#Save the plot as SVG
ggsave("plot_N50_No_Jitter.svg", width = 13.5, height = 7)
# Save the plot as PDF
ggsave("plot_N50_No_Jitter.svg.pdf",width = 13.5, height = 7)

####################################Arranging all plots #################################################

#####################################################################################################################
#                                Using ggarrange to arrange all Skin plots at one page                               #
#####################################################################################################################

# Extract the legend from plot_gut
#legend_skin <- cowplot::get_legend(plot_skin_n50 + theme(legend.position = "top"))

# Adjust the size of each plot
plot_width <- 4  # You can adjust this value
plot_height <- 2  # You can adjust this value

# Remove legend and title from each plot
plot1 <- plot_TotalLength + theme(legend.position = "none", plot.title = element_blank())
plot2 <- plot_Contigs + theme(legend.position = "none", plot.title = element_blank())
plot3 <- plot_MaxContig+ theme (legend.position="none", plot.title = element_blank())
plot4 <- plot_N50 +  theme(legend.position = "none", plot.title = element_blank())

combined_plot_Assembly <- ggarrange(plot1, plot2,plot3,plot4, nrow=2, ncol=3, widths = c(plot_width, plot_width, plot_width), labels=c("A", "B", "C", "D"))
print(combined_plot_Assembly)


#Save the plot as SVG
ggsave("Assembly_metrics_gut.svg", width = 13.5, height = 8, dpi=600)
# Save the plot as PDF
ggsave("Assembly_metrics_gut.pdf",width = 13.5, height = 8, dpi=600)
