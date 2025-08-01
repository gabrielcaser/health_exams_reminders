# This program cleans the dataset

# Opening dataset
dt <- fread(
  paste0(data_path,"/raw/raw_data_exams_2020_2025.txt"),
  sep = "|",
  header = FALSE
)

# Adding names to columns
setnames(
  dt,
  c(
    "cns",
    "nome",
    "idade",
    "situacao",
    "municipio",
    "bairro",
    "raca",
    "genero",
    "diagendou",
    "diaserafeito",
    "unidade",
    "procedimento"
  )
)


# Deidentifing
dt_c <- copy(dt)
dt_c <- dt_c[, pacient_id := as.character(as.integer(factor(cns)))]
dt_c <- dt_c[, nome := NULL]
dt_c <- dt_c[, cns := NULL]
dt_c <- dt_c[order(diagendou, procedimento)]
dt_c <- dt_c[, pd_id := paste0(pacient_id,"|",as.character(diagendou))]
dt_c <- dt_c[, pde_id := paste0(pacient_id,"|",as.character(diagendou),"|",procedimento)]

# Droping duplicates
dt_duplicated <- dt_c[duplicated(dt_c), ]
dt_c <- dt_c[!duplicated(dt_c), ]

# Removing  unidade IOSE because records are not right
dt_c <- dt_c[unidade != "IOSE"]

# Tidying

## pacient level
dt_p <- dt_c[, .(pacient_id, raca, genero, idade)]
dt_p <- dt_p[!duplicated(dt_p), ]

## pacient-day-exam level
dt_pde <- dt_c[, .(pde_id, pd_id, procedimento, pacient_id, situacao, unidade, bairro, diagendou, diaserafeito)]
setcolorder(dt_pde, c("pde_id", "pd_id", "pacient_id", "diagendou", "n_exam"))

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
  pde_id         = "Unique patient|day|exam ID",
  pd_id          = "Unique patient|day ID",
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
