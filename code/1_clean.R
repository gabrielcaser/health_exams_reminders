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

# Opening dataset
dt <- fread(
  "data/raw/raw_data_exams.txt",
  sep = "|",
  header = FALSE
)

# Adding names to columns
setnames(
  dt,
  c(
    "situacao",
    "raca",
    "genero",
    "cns",
    "nome",
    "diagendou",
    "diaserafeito",
    "unidade"
  )
)

# Creating provisory number of exams
dt_c <- dt[, n_exam := seq_len(.N), by = .(cns, diagendou)] # REMOVER DEPOIS

# Deidentifing
dt_c <- dt_c[, pacient_id := as.character(as.integer(factor(cns)))]
dt_c <- dt_c[, nome := NULL]
dt_c <- dt_c[, cns := NULL]
dt_c <- dt_c[order(diagendou, n_exam)]
dt_c <- dt_c[, pde_id := pacient_id]

# Droping duplicates
dt_duplicatet <- dt_c[duplicated(dt_c), ]
dt_c <- dt_c[!duplicated(dt_c), ]

# Tidying

## pacient level
dt_p <- dt_c[, .(pacient_id, raca, genero)]
dt_p <- dt_p[!duplicated(dt_p), ]

## pacient-day-exam level
dt_pde <- dt_c[, .(pde_id, n_exam, pacient_id, situacao, unidade, diagendou, diaserafeito)]
setcolorder(dt_pde, c("pde_id", "pacient_id", "diagendou", "n_exam"))

# Changing types
dt_pde[, unidade := factor(unidade)]
dt_pde[, diagendou := as.Date(diagendou, format = "%d/%m/%Y")]
dt_pde[, diaserafeito := as.Date(diaserafeito, format = "%d/%m/%Y")]

# Labeling

# For pacient-level dataset
var_label(dt_p) <- list(
  pacient_id = "Anonymized patient ID",
  raca       = "Race",
  genero     = "Gender"
)

# For exam-level dataset
var_label(dt_pde) <- list(
  pde_id         = "Unique patient-day-exam ID",
  n_exam         = "Exam name",
  pacient_id     = "Anonymized patient ID",
  situacao       = "Exam status",
  unidade        = "Health unit scheduled",
  diagendou      = "Date when exam was scheduled",
  diaserafeito   = "Date when exam was performed"
)

# Removing other datasets
rm(dt, dt_c, dt_duplicatet)

# Saving clean dataset
saveRDS(dt_p,   "data/intermediary/clean_data_pacient.rds")
saveRDS(dt_pde, "data/intermediary/clean_data_pacient-day-exam.rds")
