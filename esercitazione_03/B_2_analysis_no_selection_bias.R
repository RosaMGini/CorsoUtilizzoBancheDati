#-------------------------------
# Example B (clarified): now load the complete dataset and explain the findings, try to conduct the analysis

rm(list=ls(all.names=TRUE))

#set the directory where the file is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# load packages

if (!require("data.table")) install.packages("data.table")
library(data.table)


#-------------------------------------------------------
# RELATIONSHIP BETWEEN DEATH, CIGARETTES, and LUNG_CANCER

# load the complete dataset

data <- fread(paste0(thisdir,"/dataset_complete.csv"))

#-----------------------------
# display in a bar plot the occurrence of LUNG_CANCER across strata of CIGARETTES

# Calculate the percentage of records with LUNG_CANCER == 1 among those with CIGARETTES == 0 and CIGARETTES == 1
percentage_LUNG_CANCER_lighter_1 <- (sum(data$LUNG_CANCER == 1 & data$CIGARETTES == 1) / sum(data$CIGARETTES == 1)) * 100
percentage_LUNG_CANCER_lighter_0 <- (sum(data$LUNG_CANCER == 1 & data$CIGARETTES == 0) / sum(data$CIGARETTES == 0)) * 100

# Create a bar plot
barplot(c(percentage_LUNG_CANCER_lighter_1, percentage_LUNG_CANCER_lighter_0),
        names.arg = c("Cigarettes in the pocket", "No cigarettes"),
        xlab = "CIGARETTES", ylab = "Percentage with LUNG_CANCER == 1",
        col = c("brown", "orange"),
        ylim = c(0, 100))


