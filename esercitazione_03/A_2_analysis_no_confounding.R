#-------------------------------
# Example A (solved): now load the complete dataset and explain the findings

rm(list=ls(all.names=TRUE))

#set the directory where the file is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# load packages

if (!require("data.table")) install.packages("data.table")
library(data.table)


#-------------------------------------------------------
# RELATIONSHIP BETWEEN LIGHTER, CANCER, AND CIGARETTES

# load the complete dataset

data <- fread(paste0(thisdir,"/dataset_complete.csv"))

#-----------------------------
# display in a bar plot the occurrence of CIGARETTES across strata of LIGHTER

# Calculate the percentage of records with CIGARETTES == 1 among those with LIGHTER == 0 and LIGHTER == 1
percentage_CIGARETTES_lighter_1 <- (sum(data$CIGARETTES == 1 & data$LIGHTER == 1) / sum(data$LIGHTER == 1)) * 100
percentage_CIGARETTES_lighter_0 <- (sum(data$CIGARETTES == 1 & data$LIGHTER == 0) / sum(data$LIGHTER == 0)) * 100

# Create a bar plot
barplot(c(percentage_CIGARETTES_lighter_1, percentage_CIGARETTES_lighter_0),
        names.arg = c("lighter in the pocket", "no lighter"),
        xlab = "LIGHTER", ylab = "Percentage with CIGARETTES == 1",
        col = c("brown", "orange"),
        ylim = c(0, 100))


#-----------------------------

# Create subsets of the data for CIGARETTES == 1 and CIGARETTES == 0
data_CIGARETTES_1 <- data[data$CIGARETTES == 1, ]
data_CIGARETTES_0 <- data[data$CIGARETTES == 0, ]

# Calculate the percentage of records with CANCER == 1 among those with LIGHTER == 1 for each group
percentage_cancer_lighter_1_CIGARETTES_1 <- (sum(data_CIGARETTES_1$CANCER == 1 & data_CIGARETTES_1$LIGHTER == 1) / sum(data_CIGARETTES_1$LIGHTER == 1)) * 100
percentage_cancer_lighter_1_CIGARETTES_0 <- (sum(data_CIGARETTES_0$CANCER == 1 & data_CIGARETTES_0$LIGHTER == 1) / sum(data_CIGARETTES_0$LIGHTER == 1)) * 100



# Calculate the percentage of records with CANCER == 1 among those with LIGHTER == 0 for each group
percentage_cancer_lighter_0_CIGARETTES_1 <- (sum(data_CIGARETTES_1$CANCER == 1 & data_CIGARETTES_1$LIGHTER == 0) / sum(data_CIGARETTES_1$LIGHTER == 0)) * 100
percentage_cancer_lighter_0_CIGARETTES_0 <- (sum(data_CIGARETTES_0$CANCER == 1 & data_CIGARETTES_0$LIGHTER == 0) / sum(data_CIGARETTES_0$LIGHTER == 0)) * 100

# Wrap the titles of the two subplots
title_1 <- strwrap("Percentage of CANCER == 1 among\nCIGARETTES == 1", width = 20)
title_2 <- strwrap("Percentage of CANCER == 1 among\nCIGARETTES == 0", width = 20)

# Create colors for the bars (brown for LIGHTER == 1, orange for LIGHTER == 0)
bar_colors <- c("brown", "orange")

# Calculate the maximum percentage value to set the y-axis limit
max_percentage <- max(percentage_cancer_lighter_1_CIGARETTES_1, percentage_cancer_lighter_0_CIGARETTES_1,
                      percentage_cancer_lighter_1_CIGARETTES_0, percentage_cancer_lighter_0_CIGARETTES_0)

# Create a bar plot with two subplots side by side
par(mfrow = c(1, 2))  # 1 row and 2 columns for the subplots

# Plot the first subplot with wrapped title, colors, and adjusted y-axis limit
barplot(c(percentage_cancer_lighter_1_CIGARETTES_1, percentage_cancer_lighter_0_CIGARETTES_1),
        names.arg = c("with lighter", "no lighter"),
        xlab = "LIGHTER", ylab = "Percentage with CANCER == 1",
        main = title_1,
        col = bar_colors,
        ylim = c(0, 100))  # Set y-axis limit

# Plot the second subplot with wrapped title, colors, and adjusted y-axis limit
barplot(c(percentage_cancer_lighter_1_CIGARETTES_0, percentage_cancer_lighter_0_CIGARETTES_0),
        names.arg = c("with lighter", "no lighter"),
        xlab = "LIGHTER", ylab = "Percentage with CANCER == 1",
        main = title_2,
        col = bar_colors,
        ylim = c(0, 100))  # Set y-axis limit

# Add a legend
# legend("topright", legend = c("with lighter", "no lighter"),
#       fill = bar_colors, title = "LIGHTER")

# Reset the plotting layout
par(mfrow = c(1, 1))  # Reset to a single plot
