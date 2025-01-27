# CORSO UTILIZZo BANCHE DATI - ESERCITAZIONE

# author: Rosa Gini

# preso e adattato da https://github.com/ARS-toscana/emorragie_gravi

rm(list=ls(all.names=TRUE))

# setta la directory del file to_run come directory di lavoro
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

####################
# carica i parametri
source(paste0(thisdir,"/p_parameters/1_parameters_program.R"))
source(paste0(thisdir,"/p_parameters/2_parameters_CDM.R"))
source(paste0(thisdir,"/p_parameters/3_concept_sets.R"))
source(paste0(thisdir,"/p_parameters/4_parameters_study.R"))
source(paste0(thisdir,"/p_parameters/5_parameters_postprocessing.R"))


TEST <- T

#

######################################
# lanciare gli step

source(paste0(thisdir,"/p_steps/01_T2_10_create_conceptsets.R"))
source(paste0(thisdir,"/p_steps/01_T2_20_create_spells.R"))
source(paste0(thisdir,"/p_steps/01_T2_30_create_persons.R"))
source(paste0(thisdir,"/p_steps/01_T2_50_clean_DOACs.R"))
source(paste0(thisdir,"/p_steps/01_T2_60_create_episodes_of_treatement.R"))
source(paste0(thisdir,"/p_steps/01_T2_80_selection_criteria_from_PERSON_to_persons_with_MoI.R"))
source(paste0(thisdir,"/p_steps/02_T3_10_select_persons_with_MoI.R"))
source(paste0(thisdir,"/p_steps/03_T3_10_create_source_population.R"))
source(paste0(thisdir,"/p_steps/04_T2_10_create_bleeding_components.R"))
source(paste0(thisdir,"/p_steps/05_T4_10_analyse_bleeding_components.R"))
source(paste0(thisdir,"/p_steps/06_T2_10_create_bleeding_events.R"))
source(paste0(thisdir,"/p_steps/06_T2_20_create_study_population.R"))
source(paste0(thisdir,"/p_steps/06_T2_50_create_study_outcomes.R"))


source(paste0(thisdir,"/p_steps/08_T4_20_create_D5_Table_2_descr_study_pop.R"))

source(paste0(thisdir,"/p_steps/09_T5_35_create_Figure_1.R"))

