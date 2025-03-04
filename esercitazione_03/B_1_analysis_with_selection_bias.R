#-------------------------------
# Example B:  load the dataset and explore the relationship between CIGARETTES and DEATH when restricted to those with LUNG_CANCER. 

rm(list=ls(all.names=TRUE))

#set the directory where the file is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# load packages

if (!require("data.table")) install.packages("data.table")
library(data.table)


#-------------------------------------------------------
# RELATIOSHIP BETWEEN CIGARETTES AND DEATH AMONG THOSE WITH LUNG_CANCER

# load the dataset

data <- read.csv(paste0(thisdir,"/dataset_selection_bias.csv"))

# View first rows

head(data)

# Frequency of CIGARETTES variable

CIGARETTES_frequency <- table(data$CIGARETTES)
print("Frequency of CIGARETTES:")
print(CIGARETTES_frequency)

# Frequency of DEATH variable

DEATH_frequency <- table(data$DEATH)
print("Frequency of DEATH:")
print(DEATH_frequency)

#-----------------------------
# display in a bar plot the occurrence of DEATH across strata of CIGARETTES

# Calculate the percentage of records with DEATH == 1 among those with CIGARETTES == 0 and CIGARETTES == 1
percentage_DEATH_CIGARETTES_1 <- (sum(data$DEATH == 1 & data$CIGARETTES == 1) / sum(data$CIGARETTES == 1)) * 100
percentage_DEATH_CIGARETTES_0 <- (sum(data$DEATH == 1 & data$CIGARETTES == 0) / sum(data$CIGARETTES == 0)) * 100

# Create a bar plot
barplot(c(percentage_DEATH_CIGARETTES_1, percentage_DEATH_CIGARETTES_0),
        names.arg = c("Cigarettes in the pocket", "No cigarettes"),
        xlab = "CIGARETTES", ylab = "Percentage with DEATH == 1",
        col = c("brown", "orange"),
        ylim = c(0, 100))


#-----------------------------


