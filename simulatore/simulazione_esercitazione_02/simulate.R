# generate synthetic data
# author: rosa gini

# v0.1
# 26 Jan 2025

# stored in a directory  dirinput_seed there are 'seed' xls files, whose names are stored here in tables_datasource. this script produces random variations of such datasets and binds them, every xls will result in a csv file of data

# set the file directory as working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

dirinput_seed <- file.path(thisdir,"i_seeds")
dir_simdata <- file.path(thisdir,"g_simdata")

dir.create(dir_simdata, showWarnings = F)

#######################
# load packeges

list.of.packages <- c("tidyverse", "lubridate", "AdhereR","readxl", "gtsummary","zoo","data.table")


new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
invisible(lapply(list.of.packages, require, character.only = T))

#########
# tool


age_fast = function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  
  age = to_lt$year - from_lt$year
  
  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}

# parameters

agebands_num <- c(0, 18, 40, 60, 80)
agebands <- c("0","18","40","60","80")


##############################
# read the conceptsets

source(file.path(thisdir,"3_concept_sets.R"))
source(file.path(thisdir,"4_parameters_study.R"))


##############################
# create variations

# list the 'seed' files
tables_datasource <- c("ANAGRAFE_ASSISTITI","sdo","fed","ps")

# list of variables

varlist <- list()

varlist[["ANAGRAFE_ASSISTITI"]] <- c("id","datanas","sesso",	"data_inizioass","data_fineass","asl","datadec")
varlist[["sdo"]] <- c("id","data_a","data_d", "dia","meaning","pres")
varlist[["ps"]] <- c("id","data_a", "dia","meaning","esito")
varlist[["fed"]] <- c("id","datasped","aic", "atc", "pezzi")


#list the dates of each seed file
listdates_datasource <- list()
listdates_datasource[["ANAGRAFE_ASSISTITI"]] <- c("datanas","data_inizioass","data_fineass", "datadec")
listdates_datasource[["fed"]] <- c("datasped")
listdates_datasource[["sdo"]] <- c("data_a","data_d")
listdates_datasource[["ps"]] <- c("data_a")

listyears_datasource <- list()
# 
# # variations within each _datasource table 
# variations_datasource <- list()
# variations_datasource[["ANAGRAFE_ASSISTITI"]] <- 500
# variations_datasource[["fed"]] <- 5
# variations_datasource[["sdo"]] <- 100
# variations_datasource[["ps"]] <- 50 

# baseline date
baselinedate <- as.Date(lubridate::ymd(20180101))

# size of population

df_size <- 2000

####################
# ANAGRAFE_ASSISTITI

tab <- "ANAGRAFE_ASSISTITI"

# create base 

data <- data.table::data.table(id = 1:df_size )

# sesso

set.seed(1234)
data[, sesso := as.character(sample(1:2, df_size, replace = TRUE, prob = c(.6,.4)))]
data[,sesso := fifelse(sesso == "1","M","F") ]


# datanas

set.seed(5234)

data[, ageband := sample(agebands, df_size, replace = TRUE, prob = c(.001,.01,.2,.4, 1 - (.001 + .01 + .2 + .4)))]
data[, ageband := as.factor(ageband)]
# Function to generate age based on ageband
generate_age <- function(ageband) {
  lower <- as.numeric(ageband)
  upper <- ifelse(ageband == "80", 100, agebands_num[which(agebands == ageband) + 1])
  sample(lower:upper, 1)
}

# Apply the function to generate age
data[, age := sapply(as.character(ageband), generate_age)]

set.seed(4563)
data[, anchor := round(runif(.N, min = 1, max = as.numeric(study_end_date - study_start_date)))]
data[, datanas := as.Date(ymd(study_start_date) + anchor - round(365.25*age))]
# data_inizioass

data <- data[, data_inizioass := max(as.Date(study_start_date - 1000),datanas)]


# data_fineass

data <- data[, data_fineass := study_end_date]

# datadec

data <- data[, datadec := NA_Date_]

# asl

data <- data[, asl := NA_character_]

# clean and assign

listvar <- varlist[[tab]]
data <- data[,..listvar]

assign(tab, data)

####################
# fed

tab <- "fed"

# create base 

data <- data.table::data.table(id = 1:df_size )

# datasped

set.seed(45281)
data[, anchor := round(runif(.N, min = 1, max = as.numeric(study_end_date - study_start_date)))]
data[, datasped := as.Date(ymd(study_start_date) + anchor)]

data <- data[, num := round(runif(.N, min = 1, max = pmax(round(as.numeric(study_end_date - datasped)/30),1)))]

data <- data[, anchor := NULL]

# Set the mean of the Poisson distribution
mean_poisson <- 50

# Expand the dataset
data <- data[num > 1 , {
  # Generate the sequence of `num` rows for each `id`
  n <- num
  len_values <- rpois(n - 1, mean_poisson) # Generate Poisson-distributed lengths (n-1 values)
  new_dates <- c(datasped, datasped + cumsum(len_values)) # Recursively add len
  .(id = rep(id, n), datasped = new_dates) # Create the new dataset
}, by = id]
data <- data[,.(id,datasped)]

# remove some rows

set.seed(5680)
data <- data[, random := runif(.N, min = 0, max = 1)]
data <- data[ random <.95, ]
data <- data[,.(id,datasped)]

# pezzi

set.seed(1680)
data <- data[, random := runif(.N, min = 0, max = 1)]
data <- data[ , pezzi := fifelse(random <.95,1,2)]
data <- data[,.(id,datasped,pezzi)]

# aic

tool <- as.data.table(readxl::read_excel((file.path(dirinput_seed, paste0("duration_MoI", ".xlsx") ))))

codes <- unlist(unique(tool[,.(AIC)]))

set.seed(1237) 
temp <- data.table::data.table(id = 1:df_size )
temp[, AIC := sample(codes, .N, replace = TRUE)]

temp <- merge(temp,tool, by = "AIC")
names(temp)

temp <- temp[,.(id,AIC,atc)]

setnames(temp, c("AIC"),c("aic"))

data <- merge(data, temp, by = "id")

# clean and assign

setorderv(
  data,
  c("id","datasped")
)


listvar <- varlist[[tab]]
data <- data[,..listvar]

assign(tab, data)

####################
# ps

tab <- "ps"

# create base 

data <- data.table::data.table(id = 1:df_size )

# set dates

set.seed(1145)
data[, data_a := round(runif(.N, min = 0, max = as.numeric(study_end_date - study_start_date)))]
data[,data_a := as.Date(ymd(study_start_date) + data_a)]


# remove some rows (remove more probably young people)

# dependency on ageband

data <- merge (data,ANAGRAFE_ASSISTITI,by = "id", all.x = T)
data[, age := age_fast(datanas,data_a)]

# Create age bands
data[, ageband := cut(
  age, 
  breaks = c(agebands_num, Inf),  # Add Inf to include the last "80+"
  right = FALSE,             # Left-inclusive, right-exclusive intervals
  labels = c("0-17", "18-39", "40-59", "60-79", "80+")
)]


# initialize b

data[, b := 0]

coeff <- list()

coeff[["0"]] <- log(.1)
coeff[["18"]] <- log(.2)
coeff[["40"]] <- log(.5)
coeff[["60"]] <- log(2)
coeff[["80"]] <- log(3)

for (agestr in agebands) {
  print(coeff[[agestr]])
  data[, b := b + coeff[[agestr]] * fifelse(sub("-.*", "", ageband) == agestr, 1, 0)]
}

data[, prob := 1/(1 + exp(-b))]
set.seed(5801)

data[, random := runif(.N, min = 0, max = 1)]
data <- data[ random < prob, ]
data <- data[,.(id,data_a)]

set.seed(657484)
datadd <- data[, random := runif(.N, min = 0, max = 1)]
datadd <- datadd[ random < .05, ]

set.seed(644)
datadd[, add := rpois(.N, 300) ]
datadd[, data_a := data_a + add]

data <- rbind(data,datadd,fill = T)
data <- data[,.(id,data_a)]

# dia
set.seed(6473)
codes <- unlist(concept_set_codes_our_study[["bleeding_narrow"]])
data[, dia := sample(codes, .N, replace = TRUE)]

# esito
set.seed(6473)
data[, esito := sample(1:2, .N, replace = TRUE)]

# other causes
set.seed(6731)
data[, dia := fifelse(runif(.N,min = 0, max = 1) < .2,dia,"XXX.XX")]

# progress to hosp
sdo_from_ps <- data[esito == 1,]

# meaning

data[,meaning := "emergency_room_diagnosis"]

# clean and assign

setorderv(
  data,
  c("id","data_a")
)



listvar <- varlist[[tab]]
data <- data[,..listvar]

assign(tab, data)

####################
# sdo

tab <- "sdo"

# create base 

data <- data.table::data.table(id = 1:df_size )

# set dates

set.seed(145222)
data[, data_a := round(runif(.N, min = 0, max = as.numeric(study_end_date - study_start_date)))]
data[,data_a := as.Date(ymd(study_start_date) + data_a)]


# remove some rows (remove more probably young people)

# dependency on ageband

data <- merge (data,ANAGRAFE_ASSISTITI,by = "id", all.x = T)
data[, age := age_fast(datanas,data_a)]

# Create age bands
data[, ageband := cut(
  age, 
  breaks = c(agebands_num, Inf),  # Add Inf to include the last "80+"
  right = FALSE,             # Left-inclusive, right-exclusive intervals
  labels = c("0-17", "18-39", "40-59", "60-79", "80+")
)]


# initialize b

data[, b := 0]

coeff <- list()

coeff[["0"]] <- log(.1)
coeff[["18"]] <- log(.2)
coeff[["40"]] <- log(.5)
coeff[["60"]] <- log(2)
coeff[["80"]] <- log(5)

for (agestr in agebands) {
  print(coeff[[agestr]])
  data[, b := b + coeff[[agestr]] * fifelse(sub("-.*", "", ageband) == agestr, 1, 0)]
}

data[, prob := 1/(1 + exp(-b))]
set.seed(5102)

data[, random := runif(.N, min = 0, max = 1)]
data <- data[ random < prob, ]
data <- data[,.(id,data_a)]

set.seed(748441)
datadd <- data[, random := runif(.N, min = 0, max = 1)]
datadd <- datadd[ random < .2, ]

set.seed(6441)
datadd[, add := rpois(.N, 300) ]
datadd[, data_a := data_a + add]

data <- rbind(data,datadd,fill = T)
data <- data[,.(id,data_a)]

# dia principale
set.seed(6472)
codes <- unlist(concept_set_codes_our_study[["bleeding_narrow"]])
data[, dia := sample(codes, .N, replace = TRUE)]
data <- rbind(data,sdo_from_ps, fill = T)
data[ , meaning := "hospital_main_diagnosis"]

# dia sec
set.seed(1472)
datadd <- data[, random := runif(.N, min = 0 , max = 1)]
datadd <- datadd[random < .3,]
datadd <- datadd[,,.(id, data_a)]
set.seed(6721)
codes <- unlist(concept_set_codes_our_study[["bleeding_narrow"]])
datadd[, dia := sample(codes, .N, replace = TRUE)]
datadd[ , meaning := "hospital_sec_diagnosis"]
data <- rbind(data, datadd, fill = T)

# dia sec
set.seed(198763)
datadd <- data[, random := runif(.N, min = 0 , max = 1)]
datadd <- datadd[random < .3,]
datadd <- datadd[,.(id, data_a)]
set.seed(6212)
codes <- unlist(concept_set_codes_our_study[["bleeding_narrow"]])
datadd[, dia := sample(codes, .N, replace = TRUE)]
datadd[ , meaning := "hospital_sec_diagnosis"]
data <- rbind(data, datadd, fill = T)

# other causes
set.seed(6313)
data[, dia := fifelse(runif(.N,min = 0, max = 1) < .2,dia,"XXX.XX")]

# data_d

data[, data_d := data_a + 10]

# pres
set.seed(11313)
data[, pres := fifelse(runif(.N,min = 0, max = 1) < .2,1,0)]


# clean and assign

listvar <- varlist[[tab]]
data <- data[,..listvar]

# sort

setorderv(
  data,
      c("id","data_a","meaning")
   )

assign(tab, data)

###############################
# export

for (dataset in tables_datasource) {
  fwrite(get(dataset), file.path(dir_simdata, paste0(dataset,".csv") ))
}
namedataset <- ""
# 

# 
# # set the number of variarions
# increment <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,1920,30,40,50,100,200,300,400,500,600,700,800,1000)
# repetitions <- 1:9
# # # use the following lines for testing purposes
# # increment <- c(2,10)
# # repetitions <- 1:1
# # tables_datasource <- c("sdo")
# 
# 
# for (tab in tables_datasource){ 
#   data <- data.table()
#   assign(paste0(tab,"data"),data)
# }
# 
# for (tab in tables_datasource){
#   for (i in increment){
#     for (j in repetitions){
#       thisbase <- as.numeric(paste0("2.",i,j,"1") ) * 10^10
#       print(paste(tab,i,j) )
#       data <- as.data.table(readxl::read_excel((file.path(dirinput_seed, paste0(tab, ".xlsx") ))))
#       data[, person_id := thisbase + person_id]
#       # set the seed independent on the date
#       thisseedtab <- as.integer(paste0("123",i,j) )
#       set.seed(thisseedtab)
#       normrepj <- round(i * rnorm(1)) - i 
#       print(normrepj)
#       k <- 1
#       for (datevar in listdates_datasource[[tab]]) {
#         # Set the seed for this date
#         thisseed <- as.integer(paste0("123",i,j,k) )
#         set.seed(thisseed)
#         # variation in date depends on the repetition and the type of table
#         data[, (datevar) :=  get(datevar) + normrepj  + round( variations_datasource[[tab]]* rnorm(.N, 0))]
#         # let's not remove too many observation periods
#         if (tab == "ANAGRAFE_ASSISTITI" & datevar == "op_end_date"){
#           data <- data[,(datevar) := pmin(get(datevar),730)]
#         }
#         # keep only records within a rasonable time range, this will also geneate some cases tp be discarded
#         data <- data[(get(datevar) <= 730 & get(datevar) > -700 ) | is.na(get(datevar)),]
#         # transforms in dates
#         data[, (datevar) := as.Date(get(datevar) + baselinedate)]
#         
#         k <- k + 1
#       }
#       # View(data)
#       if (tab == "ANAGRAFE_ASSISTITI"){
#         data <- data[op_start_date <= op_end_date, ]
#       }
#       data <- rbind(get(paste0(tab,"data")),data,fill = T)
#       assign(paste0(tab,"data"),data)
#       print(nrow(data))
#     }
#   }
#   df <- get(paste0(tab,"data"))
#   if (!is.null(listdates_datasource[[tab]]) ){
#     date_cols <- listdates_datasource[[tab]]
#     df[, (date_cols) := lapply(.SD, format, "%Y%m%d"), .SDcols = date_cols]
#   }
#   fwrite(df, file.path(dir_simdata, paste0(paste0(tab,"_sim"),".csv") ))
# }
# 
# 
# namedataset <- ""
# fwrite(df_data, file.path(dir_simdata, paste0(namedataset,".csv") ))
