#-------------------------------
# Create dataset with 
# A) confounded association between LIGHTER and CANCER. The association between LIGHTER and CIGARETTES is what create the confounding, because the true causal association is between CIGARETTES and CANCER
# B) false protective effect of CIGARETTES against DEATH when restricting to LUNG_CANCER: this is due to the effect of selection bias. DEATH is truly caused by an unobserved cause U, that causes a severe type of lung cancer


rm(list=ls(all.names=TRUE))

#set the directory where the file is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))


# load packages

if (!require("data.table")) install.packages("data.table")
library(data.table)


# Store in a variable the size of the dataset
df_size <- 10000

# Create an empty data table
data <- data.table(PERSON_ID = 1:df_size, 
                   LIGHTER = rep(0, df_size), 
                   CIGARETTES = rep(0, df_size), 
                   CANCER = rep(0, df_size),
                   LUNG_CANCER = rep(0, df_size),
                   DEATH  = rep(0, df_size)
                   )

# Generate CIGARETTES variable (binary variable with 1 in 50% records)
set.seed(748441)
data[, random := runif(.N, min = 0, max = 1)]
data[ , CIGARETTES := fifelse(random < .5,1,0) ]

# Generate LIGHTER variable based on CIGARETTES: those with CIGARETTES == 1 have LIGHTER 80% of times, the others have LIGHTER 20% of times
set.seed(124)
data[, random := runif(.N, min = 0, max = 1)]
data <- data[CIGARETTES == 1, LIGHTER := fifelse(random < .8, 1, 0)]
data <- data[CIGARETTES == 0, LIGHTER := fifelse(random < .1, 1, 0)]

# Generate U variable (binary variable with 1 in 5% records): this is a genetic factor and is independent on anything else
set.seed(484412)
data[, random := runif(.N, min = 0, max = 1)]
data[ , U := fifelse(random < .05,1,0) ]

# Generate LUNG_CANCER_SEVERE variable based on U: those with U == 1 have LUNG_CANCER_SEVERE 90% of times, the others never
set.seed(2278)
data[, random := runif(.N, min = 0, max = 1)]
data <- data[U == 1, LUNG_CANCER_SEVERE := fifelse(random < .9, 1, 0)]


# Generate CANCER variable based on CIGARETTES: those with CIGARETTES == 1 have CANCER 70% of times, the others have CANCER 10% of times, unless they have LUNG_CANCER_SEVERE, in which case they have CANCER as well
set.seed(22711)
data[, random := runif(.N, min = 0, max = 1)]
data <- data[CIGARETTES == 1, CANCER := fifelse(random < .7, 1, 0)]
data <- data[CIGARETTES == 0, CANCER := fifelse(random < .1, 1, 0)]
data <- data[LUNG_CANCER_SEVERE == 1, CANCER := 1]

# Generate LUNG_CANCER variable based on CANCER: those with CANCER == 1 have LUNG_CANCER 40% of times, unless they have LUNG_CANCER_SEVERE, in which case they have LUNG_CANCER as well
set.seed(7214)
data[, random := runif(.N, min = 0, max = 1)]
data <- data[CANCER == 1, LUNG_CANCER := fifelse(random < .7, 1, 0)]
data <- data[LUNG_CANCER_SEVERE == 1, LUNG_CANCER := 1]
data <- data[is.na(LUNG_CANCER), LUNG_CANCER := 0]


# Generate LUNG_CANCER variable based on CANCER, LUNG_CANCER, LUNG_CANCER_SEVERE, CIGARETTES: those with CANCER == 1 have DEATH 30% of times, unless they have LUNG_CANCER_SEVERE, in which case they have DEATH 90%, those with CIGARETTES and not CANCER have DEATH 20% of times and the others 5%
set.seed(7145)
data[, random := runif(.N, min = 0, max = 1)]
data <- data[CANCER == 1, DEATH := fifelse(random < .3, 1, 0)]
data <- data[LUNG_CANCER_SEVERE == 1, DEATH := fifelse(random < .9, 1, 0)]
data <- data[is.na(DEATH) & CIGARETTES == 1, DEATH := fifelse(random < .2, 1, 0)]
data <- data[is.na(DEATH), DEATH := fifelse(random < .05, 1, 0)]


# save the dataset in csv format

write.csv(data, paste0(thisdir,"/dataset_complete.csv"), row.names = FALSE)

# keep only the confounded variables (PERSON_ID,LIGHTER,CANCER) and save the dataset in csv format

dataset_confounding <- data[, .(PERSON_ID,LIGHTER,CANCER)]
write.csv(dataset_confounding, paste0(thisdir,"/dataset_confounding.csv"), row.names = FALSE)

# keep only the  variables affected by selectin bias (PERSON_ID,LUNG_CANCER,CIGARETTES,DEATH) and save the dataset in csv format

dataset_selbias <- data[, .(PERSON_ID,LUNG_CANCER,CIGARETTES,DEATH)]
dataset_selbias <- dataset_selbias[LUNG_CANCER == 1,]
write.csv(dataset_selbias, paste0(thisdir,"/dataset_selection_bias.csv"), row.names = FALSE)
