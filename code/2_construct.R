# This program construct the dataset for analysis

# Opening clean datasets
dt_p   <- as.data.table(readRDS("data/intermediary/clean_data_pacient.rds"))
dt_pde <- as.data.table(readRDS("data/intermediary/clean_data_pacient-day-exam.rds"))

# Creating variables
dt_p <- dt_p[, homem := fcase(genero == "MASCULINO", 1L,
                      genero == "FEMININO", 0L,
                      default = NA_integer_)]

dt_pde <- dt_pde[, dias_espera := as.integer(diaserafeito - diagendou)]

dt_pde <- dt_pde[, dia_feito_semana := format(as.Date(diaserafeito), "%a")]
dt_pde <- dt_pde[, dia_feito_semana := factor(
  dia_feito_semana,
  levels = c("seg", "ter", "qua", "qui", "sex", "sáb", "dom"),
  ordered = TRUE
)]

dt_pde <- dt_pde[, compareceu := fcase(situacao == "Realizou", 1L,
                           situacao == "Faltou", 0L,
                           default = NA_integer_)]

# Reshaping to Pacient-Day level

dt_pd <- dt_pde[, .(
  n_exames = .N,
  n_compareceu = sum(compareceu)
), by = .(pacient_id, diagendou)][order(diagendou)]

dt_pd <- dt_pd[, mean_compareceu := n_compareceu / n_exames]

dt_pd <- dt_pd[, pd_id := as.character(.I)]

setcolorder(dt_pd, c("pd_id", "pacient_id"))

# Removing variables
dt_p[, genero := NULL]
dt_pde[, situacao := NULL]

# Changing types
dt_pde[, compareceu := factor(compareceu, levels = c(0, 1), labels = c("No", "Yes"))]
dt_p[, homem := factor(homem, levels = c(0, 1), labels = c("No", "Yes"))]

# Labels for dt_p
var_label(dt_p$pacient_id) <- "Anonymized patient identifier"
var_label(dt_p$raca)       <- "Patient race/color"
var_label(dt_p$homem)      <- "Male indicator"

# Labels for dt_pde
var_label(dt_pde$compareceu)       <- "Attendance indicator"
var_label(dt_pde$dias_espera)      <- "Days between scheduling and performance"
var_label(dt_pde$dia_feito_semana) <- "Day of the week to attend"

# Labels for dt_pd
var_label(dt_pd$pd_id)           <- "Unique patient-day identifier"
var_label(dt_pd$n_exames)        <- "Number of exams scheduled on the day"
var_label(dt_pd$n_compareceu)    <- "Number of attended exams on the day"
var_label(dt_pd$mean_compareceu) <- "Proportion of exams attended on the day"

# Changing columns order
setcolorder(dt_pde, c(
  "pde_id",
  "pacient_id",
  "n_exam",
  "diagendou",
  "diaserafeito",
  "dia_feito_semana",
  "dias_espera",
  "unidade",
  "compareceu"
))

# Save cleaned datasets
saveRDS(dt_p,  "data/intermediary/constructed_data_pacient.rds")
saveRDS(dt_pd, "data/intermediary/constructed_data_pacient-day.rds")
saveRDS(dt_pde,"data/intermediary/constructed_data_pacient-day-exam.rds")


