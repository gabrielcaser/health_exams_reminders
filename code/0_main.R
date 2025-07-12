# Main script

# This program cleans the dataset

# TODO
## criar IDs mesclando os nomes

# Setting the ambience
rm(list = ls())     # Remove todos objetos
graphics.off()      # Fecha todas as janelas gráficas
cat("\014")         # Limpa console (RStudio)
gc()                # Garbage collection (libera memória)

setwd("C:/Users/gabri/Documents/Github/health_exams_reminders")

# Common used packages
library(data.table)
library(skimr)
library(labelled)


# Running
source("code/1_clean.R")