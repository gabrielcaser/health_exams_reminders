# This program cleans the dataset

# Setting the ambience

setwd("C:/Users/gabri/Documents/Github/health_exams_reminders")

# Common used packages
library(data.table)
library(skimr)
library()

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
dt_c[, pacient_id := as.character(as.integer(factor(cns)))]
dt_c[, nome := NULL]
dt_c[, cns := NULL]
dt_c[, exam_id := as.character(.I)]

# Droping duplicates
dt_duplicatet <- dt_c[duplicated(dt_c), ]
dt_c <- dt_c[!duplicated(dt_c), ]

# Tidying
## pacient level
dt_p <- dt_c[, .(pacient_id, raca, genero)]
dt_p <- dt_p[!duplicated(dt_p), ]

## exam level
dt_e <- dt_c[, .(exam_id, n_exam, pacient_id, situacao, unidade, diagendou, diaserafeito)]

# For pacient-level dataset
labelled::var_label(dt_p) <- list(
  pacient_id = "Anonymized patient ID",
  raca       = "Race",
  genero     = "Gender"
)

# For exam-level dataset
labelled::var_label(dt_e) <- list(
  exam_id        = "Unique exam ID",
  n_exam         = "Exam name",
  pacient_id     = "Anonymized patient ID",
  situacao       = "Exam status",
  unidade        = "Health unit name",
  diagendou      = "Date when exam was scheduled",
  diaserafeito   = "Date when exam was performed"
)

# Removing other datasets
rm(dt, dt_c, dt_duplicatet)

# Saving clean dataset
saveRDS(dt_p, "data/intermediary/clean_data_pacient.rds")
saveRDS(dt_e, "data/intermediary/clean_data_exam.rds")
