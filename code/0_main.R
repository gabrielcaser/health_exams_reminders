# Main script

# This program cleans the dataset


# TODO
## criar IDs mesclando os nomes

# Setting the ambience
rm(list = ls())     # Remove todos objetos
graphics.off()      # Fecha todas as janelas gráficas
cat("\014")         # Limpa console (RStudio)
gc()                # Garbage collection (libera memória)
set.seed(1235)

# Paths
setwd("C:/Users/gabri/Documents/Github/Personal/health_exams_reminders")

data_path <- "C:/Users/gabri/OneDrive/Gabriel/Gov_Back/propria/data"

# Common used packages
library(data.table) # Data management
library(skimr)      # Sum stats
library(labelled)   # Create labels for data
library(pROC)       # ROC curves (ML)
library(ggplot2)    # Visualizations
library(lubridate)  # Date management
library(dplyr)      # Data manipulation
library(stringi)
library(stringdist)

# Running
source("code/1_clean.R")
source("code/2_construct.R")
source("code/3_training_model.R")
#source("code/4_visualizations.R")
