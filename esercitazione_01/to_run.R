rm(list=ls(all.names=TRUE))

# assegna la directory radice

if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# carica i pacchetti

if (!require("data.table")) install.packages("data.table")
library(data.table)
if (!require("lubridate")) install.packages("lubridate")
library(lubridate)

##################
# carica i dataset e assegna il formato data

# ANAGRAFE

ANAGRAFE <- as.data.table(readxl::read_excel(file.path(thisdir, "ANAGRAFE.xlsx") , col_types = "text"))

ANAGRAFE <- ANAGRAFE[, data_ingresso := lubridate::ymd(data_ingresso)]
ANAGRAFE <- ANAGRAFE[, data_uscita := lubridate::ymd(data_uscita)]
ANAGRAFE <- ANAGRAFE[, data_nascita := lubridate::ymd(data_nascita)]
ANAGRAFE <- ANAGRAFE[, data_morte := lubridate::ymd(data_morte)]

# SDO

SDO <- as.data.table(readxl::read_excel(file.path(thisdir, "SDO.xlsx") , col_types = "text"))

SDO <- SDO[, data_ammissione := lubridate::ymd(data_ammissione)]
SDO <- SDO[, data_dimissione := lubridate::ymd(data_dimissione)]

# ESENZIONI

ESENZIONI <- as.data.table(readxl::read_excel(file.path(thisdir, "ESENZIONI.xlsx"), col_types = "text"))

ESENZIONI <- ESENZIONI[, data_inizio := lubridate::ymd(data_inizio)]
