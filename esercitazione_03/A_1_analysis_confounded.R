#-------------------------------
# Example A:  load the confounded dataset and explore the relationship between LIGHTER and CANCER. 

rm(list=ls(all.names=TRUE))

#set the directory where the file is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# load packages

if (!require("data.table")) install.packages("data.table")
library(data.table)


#-------------------------------------------------------
# RELATIONSHIP BETWEEN LIGHTER AND CANCER

# load the dataset

data <- fread(paste0(thisdir,"/dataset_confounding.csv"))

# View first rows

head(data)

# Frequency of LIGHTER variable

lighter_frequency <- table(data$LIGHTER)
print("Frequency of LIGHTER:")
print(lighter_frequency)

# Frequency of CANCER variable

cancer_frequency <- table(data$CANCER)
print("Frequency of CANCER:")
print(cancer_frequency)

#-----------------------------
# display in a bar plot the occurrence of CANCER across strata of LIGHTER

# Calculate the percentage of records with CANCER == 1 among those with LIGHTER == 0 and LIGHTER == 1
percentage_cancer_lighter_1 <- (sum(data$CANCER == 1 & data$LIGHTER == 1) / sum(data$LIGHTER == 1)) * 100
percentage_cancer_lighter_0 <- (sum(data$CANCER == 1 & data$LIGHTER == 0) / sum(data$LIGHTER == 0)) * 100

# Create a bar plot
barplot(c(percentage_cancer_lighter_1, percentage_cancer_lighter_0),
        names.arg = c("lighter in the pocket", "no lighter"),
        xlab = "LIGHTER", ylab = "Percentage with CANCER == 1",
        col = c("brown", "orange"),
        ylim = c(0, 100))


#-----------------------------


