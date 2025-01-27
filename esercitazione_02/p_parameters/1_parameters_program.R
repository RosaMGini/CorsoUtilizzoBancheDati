# #set directory with input data

dirinput <- paste0(thisdir,"/i_input/")
dirtest <- file.path(thisdir,"i_test")
dircodelist <- file.path(thisdir,"p_parameters","archive_parameters")

batch_size_countprevalence <- 20000000

set_and_create_dir <- function(x) {
  # x <- paste0(thisdir, x)
  dir.create(file.path(x), showWarnings = F)
  return(x)
}

diroutput <- set_and_create_dir(paste0(thisdir,"/g_output/"))
dirtemp <- set_and_create_dir(paste0(thisdir,"/g_intermediate/"))
dirconceptsets <- set_and_create_dir(paste0(dirtemp,"/conceptset_datasets/"))
direxp <- set_and_create_dir(paste0(thisdir,"/g_export/"))
dirD6 <- set_and_create_dir(paste0(direxp,"/Formatted tables/"))
dirfig <- set_and_create_dir(paste0(direxp,"/Figures/"))
dirmacro <- paste0(thisdir,"/p_macro/")
dirpargen <- set_and_create_dir(paste0(thisdir,"/g_parameters/"))

rm(set_and_create_dir)

# load packages
read_library <- function(...) {
  x <- c(...)
  invisible(lapply(x, library, character.only = TRUE))
}

list.of.packages <- c("MASS", "haven", "tidyverse", "lubridate", "AdhereR", "stringr", "purrr", "readr", "dplyr",
                      "survival", "rmarkdown", "ggplot2","scales", "data.table", "qpdf", "parallel", "readxl", "gtsummary",
                      "labelled", "huxtable", "metafor", "markdown", "R.utils", "RcppAlgos", "qs","zoo","knitr", "kableExtra", 
                      "officer", "writexl","pdftools","png","grid","gridExtra","cowplot", "bit64", "splines", "pbs", "lme4", 
                      "Matrix", "clubSandwich", "lmtest", "sandwich", "openxlsx", "broom", "broom.mixed", "sjPlot", "performance")


new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
invisible(lapply(list.of.packages, require, character.only = T))

rm(read_library, new.packages, list.of.packages)

# load macros
source(paste0(dirmacro, "CreateConceptSetDatasets_v20.R"))
source(paste0(dirmacro, "CreateItemsetDatasets_v03.R"))
source(paste0(dirmacro, "MergeFilterAndCollapse_v5.R"))
source(paste0(dirmacro, "CreateSpells_v15.R"))
source(paste0(dirmacro, "CreateFlowChart.R"))
source(paste0(dirmacro, "CountPersonTimeV14.0.R"))
source(paste0(dirmacro, "df_to_list_of_list.R"))
source(paste0(dirmacro, "list_of_list_to_df.R"))
source(paste0(dirmacro, "dsr.R"))
source(paste0(dirmacro, "launch_step.R"))
source(paste0(dirmacro, "CompareListsOfCodes.R"))
source(paste0(dirmacro, "CountPrevalence.R"))
source(paste0(dirmacro, "Cube.R"))
source(paste0(dirmacro, "test utility functions.R"))
source(paste0(dirmacro, "GenerateTDDataset_v0.921.R"))
source(paste0(dirmacro, "ApplyComponentStrategy_v15.R"))
source(paste0(dirmacro, "CreateFigureComponentStrategy_v4.R"))


# ###################################################################

age_fast = function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  
  age = to_lt$year - from_lt$year
  
  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}


